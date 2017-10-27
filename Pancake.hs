{- |
Description :  A CLI web\/gopher browser
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

A CLI web\/gopher\/file browser inspired by
<https://en.wikipedia.org/wiki/Line_Mode_Browser Line Mode Browser>.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Text.Pandoc as P
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Network.URI
import System.Process
import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Data.Maybe
import Data.Either
import Data.List
import Data.String
import Data.Monoid.Colorful
import qualified System.Console.Terminfo as TI
import System.Environment
import Data.Yaml
import GHC.Generics
import qualified Data.Map as M
import System.Directory
import System.Exit
import GHC.IO.Handle
import qualified Data.ByteString.UTF8 as BSUTF8
import Control.Exception
import Text.Pandoc.Readers.Plain
import Text.Pandoc.Readers.Gopher
import Control.Applicative
import qualified System.IO as SIO


-- | Prints a line into stderr.
putErrLn :: MonadIO m => String -> m ()
putErrLn s = liftIO $ do
  SIO.hPutStrLn SIO.stderr s
  SIO.hFlush SIO.stderr


-- * Document reading

-- | Retrieves a document. Prints an error message and returns an
-- empty string on failure.
retrieve :: String
         -- ^ Shell command to use for retrieval.
         -> URI
         -- ^ Document URI.
         -> IO BS.ByteString
         -- ^ Document contents.
retrieve cmd uri = do
  putErrLn $ "Retrieving " ++ show uri
  curEnv <- getEnvironment
  let envAuthority = maybe [] (\x -> [ ("URI_USERINFO", uriUserInfo x)
                                     , ("URI_REGNAME", uriRegName x)
                                     , ("URI_PORT", uriPort x) ])
        (uriAuthority uri)
      environment = ("URI", uriToString id uri "")
                    : ("URI_SCHEME", uriScheme uri)
                    : ("URI_PATH", uriPath uri)
                    : ("URI_QUERY", uriQuery uri)
                    : ("URI_FRAGMENT", uriFragment uri)
                    : curEnv
                    ++ envAuthority
  handle (\(e :: SomeException) ->
            putErrLn (concat ["Failed to run `", cmd, "`: ", show e])
            >> pure BS.empty) $
    withCreateProcess ((shell cmd) { env = Just environment
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe }) $
    \_ stdout stderr ph -> case stdout of
      Nothing -> putErrLn "No stdout" >> pure BS.empty
      Just stdout' -> do
        hSetBinaryMode stdout' True
        out <- BS.hGetContents stdout'
        ec <- waitForProcess ph
        if (ec /= ExitSuccess)
          then do
          putErrLn $ concat ["An error occured. Exit code: ", show ec]
          case stderr of
            Nothing -> pure ()
            Just stderr' -> do
              err <- BS.hGetContents stderr'
              putErrLn $ "stderr:\n" ++ BS.unpack err
          else putErrLn $ show uri
        pure out

-- | Reads a document: retrieves it and parses into a Pandoc
-- structure. The parser is chosen depending on the URI.
readDoc :: String
        -- ^ Shell command to use for retrieval.
        -> URI
        -- ^ Document URI.
        -> IO (Maybe P.Pandoc)
        -- ^ A parsed document.
readDoc cmd uri = do
  out <- retrieve cmd uri
  term <- TI.setupTermFromEnv
  let reader = case (uriScheme uri, takeExtension $ uriPath uri) of
        -- some exceptions and special cases (might be better to make
        -- this configurable)
        ("http:", ".php") -> html
        ("https:", ".php") -> html
        ("http:", "") -> html
        ("https:", "") -> html
        ("gopher:", ext) -> case splitDirectories $ uriPath uri of
          ("/":"1":_) -> gopher
          ("/":"h":_) -> html
          -- "0" should indicate plain text, but it's also the most
          -- suitable option for non-html markup. Not sure about this
          -- approach, but it's similar to ignoring HTTP content-type,
          -- and will do for now: better to render documents nicely
          -- when possible.
          ("/":"0":_) -> byExtension ext <|> plain
          -- unknown or unrecognized item type
          _ -> byExtension ext <|> gopher
        (_, ext) -> byExtension ext <|> plain
      cols = maybe 80 id $ TI.getCapability term TI.termColumns
      opts = def { P.readerColumns = cols }
  case reader of
    Left err -> putErrLn err >> pure Nothing
    Right reader' ->
      case reader' of
        P.StringReader f -> do
          r <- f opts $ BSUTF8.toString out
          case r of
            Left err -> putErrLn (show err) >> pure Nothing
            Right doc -> pure $ pure doc
        P.ByteStringReader f -> do
          r <- f opts $ BL.fromStrict out
          case r of
            Left err -> putErrLn (show err) >> pure Nothing
            Right (doc, _) -> pure $ pure doc
  where
    html = P.getReader "html"
    plain = pure . P.StringReader . const $ pure . readPlain
    gopher = pure . P.StringReader . const $ pure . readGopher
    byExtension "" = Left "No extension"
    byExtension ".md" = P.getReader "markdown"
    byExtension ".txt" = pure . P.StringReader . const $ pure . readPlain
    byExtension ext = P.getReader $ tail ext


-- * Rendering

-- | Renderer state.
data RS = RS { indentationLevel :: Int
             , linkCount :: Int
             , bulleted :: Bool
             , ordered :: Maybe Int
             , columns :: Int
             } deriving (Show, Eq)

-- | Used to render 'Pandoc' docs by writing text lines and collected
-- links using 'Writer'.
type Renderer a = WriterT [Either URI (Colored String)] (State RS) a

-- | Runs a 'Renderer'.
runRenderer :: Int
            -- ^ Column count (line width).
            -> Renderer a
            -- ^ A renderer.
            -> [Either URI (Colored String)]
            -- ^ Collected links and text lines.
runRenderer cols r = snd $ fst $ runState (runWriterT r) (RS 0 0 False Nothing cols)

-- | Stores a link, increasing the counter
storeLink :: URI -> Renderer Int
storeLink u = do
  tell [Left u]
  st <- get
  put (st { linkCount = linkCount st + 1 })
  pure $ linkCount st

-- | Increases indentation level, runs a renderer, decreases
-- indentation level.
withIndent :: Renderer a -> Renderer a
withIndent x = do
  modify (\s -> s { indentationLevel = indentationLevel s + 1 })
  r <- x
  modify (\s -> s { indentationLevel = indentationLevel s - 1 })
  pure r

-- | Renders indented (with the current indent level) lines.
indented :: [Colored String] -> Renderer ()
indented strings = do
  st <- get
  let indent = if bulleted st
               then indentationLevel st + 2
               else maybe (indentationLevel st)
                    ((indentationLevel st + 2 +) . length . show) (ordered st)
  case ( fitLines ((columns st) - indent) strings
       , bulleted st
       , ordered st) of
    ([], _, _) -> tell []
    (x:xs, True, _) ->
      tell $ Right (fromString (replicate (indentationLevel st) ' ') <> Fg Yellow "* " <> x)
      : map (Right . (fromString (replicate indent ' ') <>)) xs
    (x:xs, _, Just n) -> do
      tell $ Right (mconcat [ fromString (replicate (indentationLevel st) ' ')
                            , Fg Yellow $ fromString (show n ++ ".")
                            , " "
                            , x])
        : map (Right . (fromString (replicate indent ' ') <>)) xs
      modify (\s -> s { ordered = Just (n + 1) })
    (xs, _, _) -> tell $ map (Right . (fromString (replicate indent ' ') <>)) xs

-- todo: deal with non-breaking spaces
-- | Fits words into terminal lines of a given width.
fitLines :: Int
         -- ^ Line width.
         -> [Colored String]
         -- ^ Strings: usually words and similar short elements.
         -> [Colored String]
         -- ^ Fitted lines.
fitLines maxLen inlineBits = map mconcat $ map reverse $ fitWords [] 0 inlineBits
  where
    asString w = showColoredS TermDumb w ""
    -- handle newline characters
    fitWords curLine _ ("\n":ws) = curLine : fitWords [] 0 ws
    -- a new line
    fitWords _ 0 (w:ws) = fitWords [w] (length $ asString w) ws
    -- add a word to a line
    fitWords curLine curLen (w:ws) = let wLen = length (asString w) in
      if curLen + wLen <= maxLen
      then fitWords (w:curLine) (curLen + wLen) ws
      else curLine : fitWords [] 0 (case w of
                                              " " -> ws
                                              _ -> (w:ws))
    -- end, no words pending
    fitWords _ 0 [] = []
    -- end, with words pending
    fitWords curLine _ [] = [curLine]

-- | A helper function to put inline elements between two strings
-- (such as parens or quotes).
wrappedInlines :: Colored String
               -- ^ String on the left.
               -> Colored String
               -- ^ String on the right.
               -> [P.Inline]
               -- ^ Inlines to wrap.
               -> Renderer [Colored String]
               -- ^ Resulting inlines.
wrappedInlines s e r = do
  r' <- concat <$> mapM readInline r
  pure $ s : r' ++ [e]

-- | Reads an inline element, producing strings. Doesn't render them
-- (i.e., using 'Writer') on its own, but collects links.
readInline :: P.Inline -> Renderer [Colored String]
readInline (P.Str s) = pure $ intersperse " " $ map fromString $ words s
readInline (P.Emph s) = concatMap (fmap $ Style Italic) <$> mapM readInline s
readInline (P.Strong s) = concatMap (fmap $ Style Bold) <$> mapM readInline s
readInline (P.Strikeout s) = wrappedInlines "-" "-" s
readInline (P.Superscript s) = wrappedInlines "^{" "}" s
readInline (P.Subscript s) = wrappedInlines "_{" "}" s
readInline (P.SmallCaps s) = wrappedInlines "\\sc{" "}" s
readInline (P.Quoted P.SingleQuote s) = wrappedInlines "‘" "’" s
readInline (P.Quoted P.DoubleQuote s) = wrappedInlines "“" "”" s
readInline (P.Cite _ s) = concat <$> mapM readInline s
readInline (P.Code _ s) = pure $ map fromString $ intersperse "\n" $ lines s
readInline P.Space = pure . pure $ fromString " "
readInline P.SoftBreak = pure . pure $ fromString " "
readInline P.LineBreak = pure . pure $ fromString "\n"
readInline (P.Math _ s) = pure . pure $ fromString s
readInline (P.RawInline _ s) = pure . pure $ fromString s
readInline (P.Link _ alt (url, title)) =
  case parseURIReference url of
    Just uri -> do
      a <- mapM readInline alt
      let t = case (title, a) of
            ("", []) -> [fromString url]
            ("", alt') -> concat alt'
            (title', []) -> [fromString title']
            (_, alt') -> concat alt' -- [[fromString title'], [" ("], concat alt', [")"]]
      case uri of
        -- fragment links are mostly useless here, at least for now.
        -- but still marking them as links, to avoid confusion.
        (URI "" Nothing "" "" _) -> pure $ map (Fg Blue) t
        _ -> storeLink uri >>=
             \cnt -> pure $ map (Fg Cyan) t ++
                     [Fg Blue (mconcat ["[", fromString $ show cnt, "]"])]
    Nothing -> pure . pure $ fromString title
readInline (P.Image attr alt (url, title)) = do
  asLink <- readInline (P.Link attr alt (url, title))
  pure $ Fg Red "(image) " : asLink
readInline (P.Note _) = pure $ pure "(note: todo)"
readInline (P.Span _ i) = do
  strings <- concat <$> mapM readInline i
  pure strings

-- | Renders a block element.
renderBlock :: P.Block -> Renderer ()
renderBlock (P.Plain i) = do
  strings <- concat <$> mapM readInline i
  indented strings
renderBlock (P.Para i) = do
  strings <- concat <$> mapM readInline i
  indented strings
  tell [Right ""]
renderBlock (P.LineBlock i) = do
  strings <- concatMap mconcat <$> mapM (mapM readInline) i
  indented strings
renderBlock (P.CodeBlock _ s) = indented $ map fromString $ intersperse "\n" $ lines s
renderBlock (P.RawBlock _ s) = indented $ map fromString $ intersperse "\n" $ lines s
renderBlock (P.BlockQuote bs) = renderBlocks bs
renderBlock (P.OrderedList _ bs) = do
  st <- get
  let o = ordered st
  put (st { ordered = Just 1 })
  mapM_ renderBlocks bs
  modify (\s -> s { ordered = o })
renderBlock (P.BulletList bs) = do
  st <- get
  let b = bulleted st
  put (st { bulleted = True })
  mapM_ renderBlocks bs
  modify (\s -> s { bulleted = b })
renderBlock (P.DefinitionList dl) =
  let renderDefinition (term, definition) = do
        term' <- concat <$> mapM readInline term
        indented term'
        mapM_ renderBlocks definition
  in mapM_ renderDefinition dl
renderBlock (P.Header _ _ i) = do
  strings <- concat <$> mapM readInline i
  indented $ "\n" : map (Fg Green . Style Bold . Style Underline) strings
renderBlock P.HorizontalRule = do
  st <- get
  indented [fromString $ replicate (columns st - indentationLevel st * 2) '-']
renderBlock (P.Table _ _ _ headers rows) = do
  -- that's a silly, yet a simple way to render a table. improve it
  -- later (todo).
  renderStairs headers
  mapM_ renderStairs rows
  where
    renderStairs :: [[P.Block]] -> Renderer ()
    renderStairs [] = pure ()
    renderStairs (x:xs) = do
      renderBlocks x
      withIndent $ renderStairs xs
renderBlock (P.Div _ b) = renderBlocks b
renderBlock P.Null = pure ()

-- | Renders multiple block elements.
renderBlocks :: [P.Block] -> Renderer ()
renderBlocks b = withIndent $ mapM_ renderBlock b


-- * Configuration

-- | Application configuration.
data Config = Config { commands :: M.Map String String
                     -- ^ URI schemes and corresponding shell commands
                     -- for downloading.
                     , defaultCommand :: String
                     -- ^ A command to use if no other command
                     -- applies.
                     , externalViewers :: M.Map String String
                     -- ^ File extensions and corresponding external
                     -- applications.
                     , shortcuts :: M.Map String String
                     -- ^ Shortcuts to use (search engines,
                     -- dictionaries, etc).
                     , paginate :: Bool
                     -- ^ Enable pagination; print everything at once
                     -- otherwise.
                     } deriving (Generic, Show)

-- | For configuration parsing
instance FromJSON Config
-- | For configuration writing, particularly that of default
-- configuration if it is missing.
instance ToJSON Config
-- | The default configuration to use if user configuration is
-- missing.
instance Default Config where
  def = Config { commands = M.fromList
                 [ ("ssh", "scp \"${URI_REGNAME}:${URI_PATH}\" /dev/stdout")
                 -- gopher://bitreich.org:70/1/onion
                 , ("gopher", "torify curl \"${URI}\"")]
               , defaultCommand = "curl -4 -L \"${URI}\""
               , externalViewers = M.fromList $
                 map (flip (,) "emacsclient") ["hs", "cabal", "c", "h", "el", "scm", "idr"]
                 ++ map (flip (,) "xdg-open") ["svg", "png", "jpg", "jpeg", "gif", "pdf"]
               , shortcuts = M.fromList
                 [ ("ddg", "https://duckduckgo.com/lite/?q=")
                 , ("wp", "https://en.m.wikipedia.org/wiki/Special:Search?search=")
                 , ("wt", "https://en.m.wiktionary.org/w/index.php?search=")
                 , ("gp", "gopher://gopherpedia.com:70/7/lookup?")
                 , ("vs", "gopher://gopher.floodgap.com/7/v2/vs?")]
               , paginate = True
               }

-- | Loads configuration from an XDG config directory.
loadConfig :: MonadIO m => StateT LoopState m ()
loadConfig = do
  c <- liftIO $ do
    dir <- getXdgDirectory XdgConfig "pancake"
    createDirectoryIfMissing True dir
    let configPath = dir </> "config.yaml"
    exists <- doesFileExist configPath
    if exists
      then do
      c <- decodeFile configPath
      case c of
        Just config -> pure config
        Nothing -> putErrLn "Failed to read the configuration, using defaults" >> pure def
      else encodeFile configPath (def :: Config) >> pure def
  modify $ \s -> s {conf = c}


-- * Control

-- | Interactive user command.
data Command = Quit
             | Follow Int
             | More
             | GoTo URI
             | Reload
             | Back
             | Forward
             | Help
             | Show Int
             | ShowCurrent
             | Shortcut String String
             | ReloadConfig
             deriving (Show, Eq)

-- | A zipper kind of thing, for scrolling and history traversal.
type Sliding a = ([a], [a], [a])

-- | Main event loop's state.
data LoopState = LS { history :: Sliding (URI, P.Pandoc)
                    , display :: Sliding (Colored String)
                    , links :: [URI]
                    , conf :: Config
                    , embedded :: Bool
                    } deriving (Show)

-- | Prints rendered lines.
showLines :: MonadIO m => [Colored String] -> StateT LoopState m ()
showLines ls = liftIO $ do
  term <- getTerm
  mapM_ (\s -> printColoredS term s >> putChar '\n') ls

-- | Prints rendered lines as s-expressions.
showSexps :: MonadIO m => [Colored String] -> StateT LoopState m ()
showSexps l = liftIO $ do
  -- would be nicer to use some library for this, but they tend to be
  -- abandoned, and the task is simple enough to do it here
  putStrLn $ "( " ++ intercalate " " (map (\x -> concat ["(", showSexp x, ")"]) l) ++ " )"
  SIO.hFlush SIO.stdout
  where
    showSexp :: Colored String -> String
    -- no need for nils since the pairs are flattened
    showSexp Nil = ""
    showSexp (Value x) = concat ["\"", concatMap escape x, "\""]
      where escape '\\' = "\\\\"
            escape '"' = "\\\""
            escape other = pure other
    showSexp (Style s c) = concat ["(style ", show s, " ", showSexp c, ")"]
    showSexp (Unstyle s c) = concat ["(unstyle ", show s, " ", showSexp c, ")"]
    showSexp (Fg clr c) = concat ["(fg (", show clr, ") ", showSexp c, ")"]
    showSexp (Bg clr c) = concat ["(bg (", show clr, ") ", showSexp c, ")"]
    -- pairs are not important here, flattening at once
    showSexp (Pair x y) = concat [showSexp x, " ", showSexp y]

-- | Renders a parsed document.
renderDoc :: MonadIO m => P.Pandoc -> StateT LoopState m ()
renderDoc (P.Pandoc _ blocks) = do
  term <- liftIO TI.setupTermFromEnv
  st <- get
  let cols = maybe 80 id $ TI.getCapability term TI.termColumns
      l = runRenderer cols $ renderBlocks blocks
      textLines = rights l
  modify (\s -> s { links = lefts l })
  if embedded st
    then showSexps textLines
    else do
    let rows = maybe 25 id (TI.getCapability term TI.termLines) - 1
        (shownLines, nextLines) =
          if paginate (conf st)
          then splitAt rows textLines
          else (textLines, [])
    showLines shownLines
    modify (\s -> s { display = ([], shownLines, nextLines) })

-- | Evaluates user commands.
command :: MonadIO m => Command -> StateT LoopState m ()
command (GoTo u') = do
  st <- get
  let ddg = isPrefixOf "/l/?kh=-1&uddg=" $ uriToString id u' ""
      u = case (ddg, uriIsAbsolute u', history st) of
            -- fix DDG links (that's rather hacky, todo: improve)
            (True, _, _) -> maybe u' id $ parseAbsoluteURI (unEscapeString $ drop 12 (uriQuery u'))
            -- handle relative URIs
            (_, False, (_, [(cur, _)], _)) -> relativeTo u' cur
            _ -> u'
      cmd = maybe (defaultCommand $ conf st) id (M.lookup (init $ uriScheme u) (commands $ conf st))
  d <- liftIO $ do
    let ext = case takeExtension $ uriPath u of
          "" -> "html"
          x -> tail x
    case M.lookup ext (externalViewers $ conf st) of
      Nothing -> readDoc cmd u
      Just ev -> do
        d <- retrieve cmd u
        dir <- getXdgDirectory XdgCache "pancake"
        let tmpPath = dir </> (takeFileName $ uriPath u)
        handle
          (\(e :: SomeException) ->
             putErrLn (concat ["Failed to open `", tmpPath, "` with `" , cmd, "`: ", show e])) $ do
          createDirectoryIfMissing True dir
          BS.writeFile tmpPath d
          callCommand $ concat [ev, " ", tmpPath]
        pure Nothing
  case d of
    Nothing -> pure ()
    Just doc@(P.Pandoc _ _) -> do
      renderDoc doc
      modify $ \s ->
        let (prev, cur, _) = history s
        in s { history = ( cur ++ prev, [(u, doc)], []) }
command (Follow i) = do
  st <- get
  if length (links st) > i
    then command (GoTo $ links st !! i)
    else liftIO $ putErrLn "No such link"
command Back = do
  st <- get
  case history st of
    (p@(_, d):prev, cur, next) -> do
      renderDoc d
      modify $ \s -> s { history = (prev, [p], cur ++ next) }
    _ -> liftIO $ putErrLn "There's nothing back there"
command Forward = do
  st <- get
  case history st of
    (prev, cur, n@(_, d):next) -> do
      renderDoc d
      modify $ \s -> s { history = (cur ++ prev, [n], next) }
    _ -> liftIO $ putErrLn "Nowhere to go"
command More = do
  st <- get
  case display st of
    (_, _, []) -> pure ()
    (prev, cur, next) -> do
      term <- liftIO TI.setupTermFromEnv
      let lineCount' = maybe 25 id (TI.getCapability term TI.termLines)
          lineCount = lineCount' - div lineCount' 3
          (newLines, next') = splitAt lineCount next
      showLines newLines
      modify (\s -> s { display = (reverse cur ++ prev, newLines, next') })
      pure ()
command Reload = liftIO $ putErrLn "Not implemented yet (TODO)"
command Help = do
  st <- get
  liftIO $ do
    putErrLn "[q]uit, [b]ack, [f]orward, [h]elp, [re]load config"
    putErrLn "type a number to follow a link, \"<number>?\" to print its URI"
    putErrLn "type an URI (absolute or relative) to open it"
    when (paginate $ conf st) $ putErrLn "RET to scroll"
command (Show n) = do
  st <- get
  liftIO . putErrLn $ if length (links st) > n
                      then show $ links st !! n
                      else "No such link"
command ShowCurrent = do
  st <- get
  case history st of
    (_, [(u, _)], _) -> liftIO $ putErrLn $ show u
    _ -> pure ()
command (Shortcut u q) = command . GoTo . fromJust . parseURI $
  u ++ escapeURIString isReserved q
command ReloadConfig = loadConfig
command Quit = liftIO $ do
    dir <- getXdgDirectory XdgCache "pancake"
    exists <- doesDirectoryExist dir
    when exists $ removeDirectoryRecursive dir

-- | Reads commands, runs them.
eventLoop :: MonadIO m => StateT LoopState m ()
eventLoop = do
  cmd <- liftIO $ getLine
  st <- get
  let c = case cmd of
        "q" -> Quit
        "b" -> Back
        "f" -> Forward
        "r" -> Reload
        "re" -> ReloadConfig
        "h" -> Help
        "?" -> ShowCurrent
        _ -> case reads cmd of
          [(n, "")] -> Follow n
          [(n, "?")] -> Show n
          _ -> case words cmd of
            [] -> More
            (s:q) -> case M.lookup s (shortcuts (conf st)) of
              Just u -> Shortcut u $ unwords q
              Nothing -> case parseURIReference cmd of
                           Just uri -> GoTo uri
                           Nothing -> Help
  command c
  when (c /= Quit) eventLoop

-- | Loads configuration and runs 'eventLoop'.
main :: IO ()
main = do
  args <- getArgs
  insideEmacs <- lookupEnv "INSIDE_EMACS"
  _ <- runStateT (loadConfig >> eventLoop) $
    LS ([],[],[]) ([],[],[]) [] def (isJust insideEmacs || "--embedded" `elem` args)
  pure ()
