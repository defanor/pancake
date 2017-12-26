{-
Copyright (C) 2017  defanor <defanor@uberspace.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{- |
Description :  A CLI web\/gopher browser
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

A CLI\/Emacs web\/gopher\/file browser inspired by LMB.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Text.Pandoc as P
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Network.URI
import System.Process
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import Data.List
import System.Console.Terminfo
import System.Environment
import qualified Data.Map as M
import System.Directory
import System.Exit
import Control.Exception
import Data.Char
import System.IO.Error
import Control.Applicative
import Data.Version
import System.Console.GetOpt
import System.Posix.Signals
import Control.Concurrent
import Text.Regex.TDFA

import Pancake.Common
import Pancake.Configuration
import Pancake.Command
import Pancake.Reading
import Pancake.Rendering
import Pancake.Printing
import Pancake.Unclutter
import Paths_pancake

-- | A zipper kind of thing, for scrolling and history traversal.
type Sliding a = ([a], [a])

-- | A history entry.
data HistoryEntry = HE { hURI :: URI
                       , hDoc :: P.Pandoc
                       , hPos :: Int
                       }

-- | Main event loop's state.
data LoopState = LS { history :: Sliding HistoryEntry
                    , position :: Int
                    , rendered :: [RendererOutput]
                    , conf :: Config
                    , embedded :: Bool
                    , noWrap :: Bool
                    , interrupted :: Bool
                    , unclutterRegexps :: [(Regex, String)]
                    , columns :: Maybe Int
                    }

-- | Main event loop's type.
type Pancake a = forall m. MonadIO m => StateT LoopState m a

-- | Renders a parsed document.
printDoc :: URI -> P.Pandoc -> Pancake ()
printDoc uri doc = do
  term <- liftIO setupTermFromEnv
  st <- get
  let cols = fromMaybe 80 $ columns st <|> getCapability term termColumns
      l = renderDoc cols (noWrap st) (conf st) doc
      textLines = rLines l
  modify (\s -> s { rendered = l })
  if embedded st
    then showSexps uri l
    else do
    let rows = fromMaybe 25 (getCapability term termLines) - 1
    showLines $ if paginate (conf st)
                then take rows textLines
                else textLines
    modify (\s -> s { position = rows })

-- | Updates 'LoopState' with user configuration.
updateConfig :: Maybe FilePath -> Pancake ()
updateConfig mp = do
  c <- loadConfig mp
  u <- prepareUnclutter c
  modify $ \s -> s { conf = c, unclutterRegexps = u }

-- | A wrapper around 'retrieve' that adjusts the URI.
loadRaw :: URI -> Pancake (URI, Maybe (BS.ByteString, Maybe URI, Maybe String))
loadRaw rawURI = do
  st <- get
  let ddg = isPrefixOf "/l/?kh=-1&uddg=" $ uriToString id rawURI ""
      adjustedURI = case (ddg, uriIsAbsolute rawURI, history st) of
        -- fix DDG links (that's rather hacky, todo: improve)
        (True, _, _) -> fromMaybe rawURI $
          parseAbsoluteURI (unEscapeString $ drop 12 (uriQuery rawURI))
        -- handle relative URIs
        (_, False, (h:_, _)) -> relativeTo rawURI (hURI h)
        _ -> rawURI
      uScheme = case uriScheme adjustedURI of
        [] -> "unknown"
        s -> init s
      cmd = fromMaybe (defaultCommand $ conf st) $
        M.lookup uScheme (commands $ conf st)
  doc <- liftIO $ retrieve cmd adjustedURI
  pure (adjustedURI, doc)

-- | Decides what to do with a given URI; either returns a document or
-- runs an external viewer. Used by both 'GoTo' and 'Reload'.
loadDocument :: Maybe String
             -- ^ Document type.
             -> URI
             -- ^ Document URI.
             -> Pancake (URI, Maybe P.Pandoc)
loadDocument sType rawURI = do
  st <- get
  (adjustedURI, docData) <- loadRaw rawURI
  case docData of
    Nothing -> pure (adjustedURI, mzero)
    Just (rawDoc, mdURI, mdType) -> liftIO $ do
      let effectiveURI = fromMaybe adjustedURI mdURI
          fType = sType <|> mdType
          ext = case (fType, takeExtension $ uriPath effectiveURI) of
            (Just x, _) -> x
            (_, '.':xs) -> map toLower xs
            (_, other) -> other
      case M.lookup ext (externalViewers $ conf st) of
        Nothing -> do
          uDoc <- tryUnclutter (unclutterRegexps st) effectiveURI rawDoc
          doc <- readDoc uDoc fType effectiveURI
          case doc of
            Left err -> do
              putErrLn $ show err
              pure (effectiveURI, mzero)
            Right r -> pure (effectiveURI, pure r)
        Just ev -> do
          dir <- getXdgDirectory XdgCache "pancake"
          let tmpPath = dir </> takeFileName (uriPath effectiveURI)
          handle
            (\(e :: SomeException) ->
               putErrLn (concat ["Failed to open `", tmpPath, "` with `"
                                , ev, "`: ", show e])) $ do
            createDirectoryIfMissing True dir
            BS.writeFile tmpPath rawDoc
            curEnv <- getEnvironment
            ec <- withCreateProcess
              ((shell ev) { env = Just (("FILE", tmpPath) : curEnv) }) $
              \_ _ _ p -> waitForProcess p
            when (ec /= ExitSuccess) $
              putErrLn $ "An error occurred. Exit code: " ++ show ec
          pure (effectiveURI, mzero)

-- | Visits an URI, updates history accordingly.
goTo :: Maybe String -> URI -> Pancake ()
goTo t u' = do
  (uri, d) <- loadDocument t u'
  case d of
    Nothing -> pure ()
    Just doc -> do
      printDoc uri doc
      modify $ \s ->
        let (prev, _) = history s
        in s {history = (take (historyDepth $ conf s) $ HE uri doc 0:prev, [])}

-- | Line number to a fixed block's number.
lineToBlockNumber :: [(Int, Int)] -> Int -> Int
lineToBlockNumber bs n =
  case filter (\(_, (f, l)) -> f <= n && n < l) (zip [0..] bs) of
    [] -> 0
    xs -> fst $ maximumBy (\(_, (l, _)) (_, (l', _)) -> compare l l') xs

-- | Fixed block's number to line number.
blockNumberToLine :: [(Int, Int)] -> Int -> Int
blockNumberToLine bs p
  | length bs <= p = 1
  | otherwise = fst $ bs !! p

-- | Scrolls to a line, which would be at the bottom for CLI.
scrollToLine :: Int -> Pancake ()
scrollToLine n = get >>= \st -> when (n > position st || embedded st) $ do
  -- update history entry's position
  case history st of
    (h : prev, next) -> modify $ \s ->
      s {history =
         (h {hPos = lineToBlockNumber (rBlocks $ rendered st) n}:prev, next)}
    _ -> pure ()
  -- go to line
  if embedded st
    then putSexpLn ["goto", show n]
    else do
    showLines $ take (n - position st) $
      drop (position st) (rLines $ rendered st)
    modify (\s -> s { position = n })

-- | Scrolls to a fixed block's position.
scrollToBlock :: Int -> Pancake ()
scrollToBlock b = get
  >>= \s -> scrollToLine $ blockNumberToLine (rBlocks $ rendered s) b

-- | Evaluates user commands.
command :: Command -> Pancake ()
command (Save (RURI uri') p) = do
  (uri, mraw) <- loadRaw uri'
  st <- get
  case mraw of
    Nothing -> pure ()
    Just (raw, euri, _) -> liftIO $ do
      (targetDir, mTargetName) <- case p of
        Nothing -> do
          cacheDir <- getXdgDirectory XdgCache "pancake"
          pure (cacheDir, Nothing)
        Just fp -> do
          exists <- doesDirectoryExist fp
          pure $ case (exists, takeFileName fp) of
            (True, _) -> (fp, Nothing)
            (_, "") -> (fp, Nothing)
            (False, fn) -> (takeDirectory fp, pure fn)
      createDirectoryIfMissing True targetDir
      let remoteURI = fromMaybe uri euri
          remoteURIStr = uriToString id remoteURI ""
          remoteFileName' = takeFileName $ uriPath remoteURI
          remoteFileName = if remoteFileName' `elem` [".", "..", ""]
                           then map escapeURI remoteURIStr
                           else remoteFileName'
          targetFileName = fromMaybe remoteFileName mTargetName
          targetPath = targetDir </> targetFileName
      e <- try $ BS.writeFile targetPath raw
      case e of
        Left (err :: SomeException) ->
          putErrLn $ unwords ["Failed to write", targetPath ++ ":", show err]
        Right () -> do
          when (embedded st) $
            putSexpLn [ "saved"
                      , encodeSexpStr $ uriToString id uri' ""
                      , encodeSexpStr targetPath]
          putErrLn $ unwords ["Saved", remoteURIStr, "as", targetPath]
  where
    escapeURI c
      | isPathSeparator c = '-'
      | otherwise = c
command (Save (RNumber i) p) = do
  st <- get
  if length (rLinks $ rendered st) > i
    then command $ Save (RURI $ rLinks (rendered st) !! i) p
    else putErrLn "No such link"
command (Save RCurrent p) = do
  st <- get
  case history st of
    (h:_, _) -> command $ Save (RURI $ hURI h) p
    _ -> pure ()
command (GoTo t (RURI u@(URI _ _ _ _ ('#':xs)))) = do
  -- follow an URI first, if it's not just a fragment
  case u of
    (URI "" Nothing "" "" _) -> pure ()
    _ -> goTo t u
  -- get to the fragment
  st <- get
  term <- liftIO setupTermFromEnv
  let lineCount = fromMaybe 25 (getCapability term termLines)
  maybe (putErrLn $ "Unknown identifier: " ++ xs)
    (\pos -> scrollToLine $ if embedded st then pos else pos + lineCount - 2)
    (lookup xs (rIdentifiers $ rendered st))
command (GoTo t (RURI u)) = goTo t u
command (GoTo t (RNumber i)) = do
  st <- get
  if length (rLinks $ rendered st) > i
    then command (GoTo t $ RURI $ rLinks (rendered st) !! i)
    else putErrLn "No such link"
command Back = do
  st <- get
  case history st of
    (cur:h:prev, next) -> do
      printDoc (hURI h) (hDoc h)
      scrollToBlock (hPos h)
      modify $ \s ->
        s { history = (h:prev, take (historyDepth $ conf s) $ cur : next) }
    _ -> putErrLn "There's nothing back there"
command Forward = do
  st <- get
  case history st of
    (prev, h:next) -> do
      printDoc (hURI h) (hDoc h)
      scrollToBlock (hPos h)
      modify $ \s ->
        s { history = (take (historyDepth $ conf s) $ h:prev, next) }
    _ -> putErrLn "Nowhere to go"
command More = do
  st <- get
  unless (embedded st) $ do
    term <- liftIO setupTermFromEnv
    let lineCount = fromMaybe 25 (getCapability term termLines)
    scrollToLine (position st + lineCount - 3)
command (GoTo t RCurrent) = do
  st <- get
  case history st of
    (h : prev, next) -> do
      (uri, d) <- loadDocument t (hURI h)
      case d of
        Nothing -> pure ()
        Just doc -> do
          printDoc uri doc
          scrollToBlock (hPos h)
          modify $ \s -> s {history = (HE (hURI h) doc (hPos h) : prev, next)}
    _ -> putErrLn "There's nothing to reload"
command Help = do
  st <- get
  putErrLn $ intercalate "\n"
    [ "basic commands: [ and ] to go back or forward, ',' to reload"
    , "<URI> or [,]<number> to open a document" ]
  when (paginate $ conf st) $ putErrLn "RET to scroll"
command (Show n) = do
  st <- get
  putErrLn $ if length (rLinks $ rendered st) > n
             then show $ rLinks (rendered st) !! n
             else "No such link"
command ShowCurrent = do
  st <- get
  case history st of
    (h:_, _) -> putErrLn $ show (hURI h)
    _ -> pure ()
command (Shortcut u q) = command . GoTo Nothing . RURI . fromJust . parseURI $
  u ++ escapeURIString isUnreserved q
command (LoadConfig p) = updateConfig p
command Quit = liftIO $ do
  dir <- getXdgDirectory XdgCache "pancake"
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
command Interrupt =
  putErrLn "Received SIGINT. Interrupt twice in a row to quit."
command (SetWidth w) = modify $ \s -> s { columns = w }
command (SetPos mp) = let p = fromMaybe 0 mp in modify $ \s ->
  s { position = p
    , history =
      case history s of
        (h:prev, next) ->
          (h { hPos = lineToBlockNumber (rBlocks $ rendered s) p } : prev, next)
        other -> other}
command Redisplay = do
  st <- get
  case history st of
    (h:_, _) -> do
      printDoc (hURI h) (hDoc h)
      scrollToBlock (hPos h)
    _ -> putErrLn "There's nothing to redisplay"


-- | Reads commands, runs them with 'command'.
eventLoop :: Pancake ()
eventLoop = do
  st <- get
  c <- liftIO $ catches (parseCommand (conf st) <$> getLine)
       [Handler handleIO, Handler handleAsync]
  unless (c == Interrupt && interrupted st) $ do
    command c
    modify $ \s -> s { interrupted = c == Interrupt }
    when (c /= Quit) eventLoop
  where
    handleIO :: IOException -> IO Command
    handleIO e = unless (isEOFError e)
                 (putErrLn ("Unexpected error: " ++ show e))
                 >> pure Quit
    handleAsync :: AsyncException -> IO Command
    handleAsync UserInterrupt = pure Interrupt
    handleAsync other = throw other

-- | Command-line options.
data Option = OVersion
            | OHelp
            | OEmbedded
            | ONoWrap
            | OConfig FilePath
  deriving (Show, Eq)

-- | Command-line option descriptions for 'getOpt'.
options :: [OptDescr Option]
options = [ Option [] ["version"] (NoArg OVersion)
            "show version number and exit"
          , Option [] ["help"] (NoArg OHelp) "show help message and exit"
          , Option ['e'] ["embedded"] (NoArg OEmbedded)
            "run in the embedded mode"
          , Option ['n'] ["no-wrap"] (NoArg ONoWrap)
            "leave line wrapping to UI when appropriate"
          , Option ['c'] ["config"] (ReqArg OConfig "FILE")
            "load configuration from a specified file"
          ]

-- | Loads configuration and runs 'eventLoop'.
main :: IO ()
main = do
  args <- getArgs
  -- A hack to receive SIGINT reliably.
  tid <- myThreadId
  _ <- installHandler sigINT (Catch (throwTo tid UserInterrupt)) Nothing
  let (opts, cmd, errors) = getOpt Permute options args
      run
        | OVersion `elem` opts = putStrLn $ "pancake " ++ showVersion version
        | OHelp `elem` opts || not (null errors) = do
            mapM_ putErrLn errors
            p <- getProgName
            putStrLn $ usageInfo
              ("Usage: " ++ p ++ " [option ...] [command ...]") options
        | otherwise = do
            let maybeCommand =
                  if null cmd
                  then pure ()
                  else get
                       >>= \st -> command (parseCommand (conf st) (unwords cmd))
            _ <- runStateT
                 (updateConfig (findConf opts) >> maybeCommand >> eventLoop)
                 LS { history = ([],[])
                    , position = 0
                    , rendered = []
                    , conf = def
                    , embedded = OEmbedded `elem` opts
                    , noWrap = ONoWrap `elem` opts
                    , interrupted = False
                    , unclutterRegexps = []
                    , columns = Nothing
                    }
            pure ()
  run
  where
    findConf :: [Option] -> Maybe FilePath
    findConf [] = Nothing
    findConf (OConfig fp:_) = Just fp
    findConf (_:xs) = findConf xs
