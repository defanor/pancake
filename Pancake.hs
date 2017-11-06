{- |
Description :  A CLI web\/gopher browser
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

A CLI web\/gopher\/file browser inspired by
<https://en.wikipedia.org/wiki/Line_Mode_Browser Line Mode Browser>.

-}

{-# LANGUAGE ScopedTypeVariables #-}

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

import Pancake.Common
import Pancake.Configuration
import Pancake.Command
import Pancake.Reading
import Pancake.Rendering
import Pancake.Printing

-- | A zipper kind of thing, for scrolling and history traversal.
type Sliding a = ([a], [a])

-- | Main event loop's state.
data LoopState = LS { history :: Sliding (URI, P.Pandoc)
                    , position :: Int
                    , rendered :: [RendererOutput]
                    , conf :: Config
                    , embedded :: Bool
                    } deriving (Show)

-- | Renders a parsed document.
printDoc :: MonadIO m => URI -> P.Pandoc -> StateT LoopState m ()
printDoc uri doc = do
  term <- liftIO setupTermFromEnv
  st <- get
  let cols = fromMaybe 80 $ getCapability term termColumns
      l = renderDoc cols doc
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
updateConfig :: MonadIO m => StateT LoopState m ()
updateConfig = do
  c <- loadConfig
  modify $ \s -> s { conf = c }

-- | Decides what to do with a given URI; either returns a document or
-- runs an external viewer. Used by both 'GoTo' and 'Reload'.
loadDocument :: MonadIO m
             => Maybe String
             -- ^ Document type.
             -> URI
             -- ^ Document URI.
             -> StateT LoopState m (URI, Maybe P.Pandoc)
loadDocument sType rawURI = do
  st <- get
  let ddg = isPrefixOf "/l/?kh=-1&uddg=" $ uriToString id rawURI ""
      adjustedURI = case (ddg, uriIsAbsolute rawURI, history st) of
        -- fix DDG links (that's rather hacky, todo: improve)
        (True, _, _) -> fromMaybe rawURI $
          parseAbsoluteURI (unEscapeString $ drop 12 (uriQuery rawURI))
        -- handle relative URIs
        (_, False, ((cur, _):_, _)) -> relativeTo rawURI cur
        _ -> rawURI
      uScheme = case uriScheme adjustedURI of
        [] -> "unknown"
        s -> init s
      cmd = fromMaybe (defaultCommand $ conf st) $
        M.lookup uScheme (commands $ conf st)
  liftIO $ do
    docData <- retrieve cmd adjustedURI
    case docData of
      Nothing -> pure (adjustedURI, mzero)
      Just (rawDoc, mdURI, mdType) -> do
        let effectiveURI = fromMaybe adjustedURI mdURI
            fType = sType <|> mdType
            ext = case (fType, takeExtension $ uriPath effectiveURI) of
              (Just x, _) -> x
              (_, '.':xs) -> map toLower xs
              (_, other) -> other
        case M.lookup ext (externalViewers $ conf st) of
          Nothing -> do
            doc <- readDoc rawDoc fType effectiveURI
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
                putErrLn $ "An error occured. Exit code: " ++ show ec
            pure (effectiveURI, mzero)

-- | Visits an URI, updates history accordingly.
goTo :: MonadIO m => Maybe String -> URI -> StateT LoopState m ()
goTo t u' = do
  (uri, d) <- loadDocument t u'
  case d of
    Nothing -> pure ()
    Just doc -> do
      printDoc uri doc
      modify $ \s ->
        let (prev, _) = history s
        in s {history = (take (historyDepth $ conf s) $ (uri, doc) : prev, [])}

-- | Evaluates user commands.
command :: MonadIO m => Command -> StateT LoopState m ()
command (GoTo t u@(URI _ _ _ _ ('#':xs))) = do
  -- follow an URI first, if it's not just a fragment
  case u of
    (URI "" Nothing "" "" _) -> pure ()
    _ -> goTo t u
  -- get to the fragment
  st <- get
  case (lookup xs (rIdentifiers $ rendered st), embedded st) of
    (Nothing, _) -> putErrLn $ "Unknown identifier: " ++ xs
    (Just x, False) -> do
      term <- liftIO setupTermFromEnv
      let lineCount = fromMaybe 25 (getCapability term termLines)
      when (x + lineCount - 2 > position st) $ do
        -- scroll to the given position without skipping anything
        showLines $ take (x - position st + lineCount - 2) $
          drop (position st) (rLines $ rendered st)
        modify (\s -> s { position = x + lineCount - 2 })
    (Just x, True) -> putSexpLn ["goto", show x]
command (GoTo t u) = goTo t u
command (Follow i) = do
  st <- get
  if length (rLinks $ rendered st) > i
    then command (GoTo Nothing $ rLinks (rendered st) !! i)
    else liftIO $ putErrLn "No such link"
command Back = do
  st <- get
  case history st of
    (cur:p@(uri, d):prev, next) -> do
      printDoc uri d
      modify $ \s ->
        s { history = (p:prev, take (historyDepth $ conf s) $ cur : next) }
    _ -> liftIO $ putErrLn "There's nothing back there"
command Forward = do
  st <- get
  case history st of
    (prev, n@(uri, d):next) -> do
      printDoc uri d
      modify $ \s ->
        s { history = (take (historyDepth $ conf s) $ n : prev, next) }
    _ -> liftIO $ putErrLn "Nowhere to go"
command More = do
  st <- get
  term <- liftIO setupTermFromEnv
  let lineCount' = fromMaybe 25 (getCapability term termLines)
      lineCount = lineCount' - div lineCount' 3
  showLines $ take lineCount $ drop (position st) (rLines $ rendered st)
  modify (\s -> s { position = position st + lineCount })
  pure ()
command Reload = do
  st <- get
  case history st of
    ((u, _):prev, next) -> do
      (uri, d) <- loadDocument Nothing u
      case d of
        Nothing -> pure ()
        Just doc -> do
          printDoc uri doc
          modify $ \s -> s { history = ( (u, doc):prev, next ) }
    _ -> putErrLn "There's nothing to reload"
command Help = do
  st <- get
  liftIO $ do
    putErrLn $ intercalate "\n"
      [ "[q]uit, [b]ack, [f]orward, [h]elp, [r]eload, [re]load config"
      , "type a number to follow a link, \"?<number>\" to print its URI"
      , "type an URI (absolute or relative) to open it"
      , "prefix it with a type (html, txt, org, etc) to choose a reader"]
    when (paginate $ conf st) $ putErrLn "RET to scroll"
command (Show n) = do
  st <- get
  liftIO . putErrLn $ if length (rLinks $ rendered st) > n
                      then show $ rLinks (rendered st) !! n
                      else "No such link"
command ShowCurrent = do
  st <- get
  case history st of
    ((u, _):_, _) -> liftIO $ putErrLn $ show u
    _ -> pure ()
command (Shortcut u q) = command . GoTo Nothing . fromJust . parseURI $
  u ++ escapeURIString isReserved q
command ReloadConfig = updateConfig
command Quit = liftIO $ do
    dir <- getXdgDirectory XdgCache "pancake"
    exists <- doesDirectoryExist dir
    when exists $ removeDirectoryRecursive dir

-- | Reads commands, runs them.
eventLoop :: MonadIO m => StateT LoopState m ()
eventLoop = do
  cmd' <- liftIO $ try getLine
  let onErr e = unless (isEOFError e)
                (putErrLn ("Unexpected error: " ++ show e))
                >> pure Quit
  st <- get
  c <- either onErr (pure . parseCommand (conf st)) cmd'
  command c
  when (c /= Quit) eventLoop

-- | Loads configuration and runs 'eventLoop'.
main :: IO ()
main = do
  args <- getArgs
  _ <- runStateT (updateConfig >> eventLoop) $
    LS ([],[]) 0 [] def ("--embedded" `elem` args)
  pure ()
