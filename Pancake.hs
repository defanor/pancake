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
import Data.Char


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
                                   , std_err = CreatePipe
                                   , delegate_ctlc = True }) $
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
        -> IO (Either P.PandocError P.Pandoc)
        -- ^ A parsed document.
readDoc cmd uri = do
  out <- retrieve cmd uri
  term <- TI.setupTermFromEnv
  let reader = either (const plain) id $
        case (uriScheme uri, map toLower $ takeExtension $ uriPath uri) of
          -- some exceptions and special cases (might be better to make
          -- this configurable)
          ("http:", ext) -> http ext
          ("https:", ext) -> http ext
          ("gopher:", ext) -> case uriPath uri of
            ('/':'1':_) -> gopher
            ('/':'h':_) -> html
            -- "0" should indicate plain text, but it's also the most
            -- suitable option for non-html markup. Not sure about this
            -- approach, but it's similar to ignoring HTTP content-type,
            -- and will do for now: better to render documents nicely
            -- when possible.
            ('/':'0':_) -> byExtension ext
            -- unknown or unrecognized item type
            _ -> byExtension ext <|> gopher
          (_, ext) -> byExtension ext
      cols = maybe 80 id $ TI.getCapability term TI.termColumns
      opts = def { P.readerColumns = cols }
  case reader of
    P.StringReader f -> f opts $ BSUTF8.toString out
    P.ByteStringReader f -> fmap fst <$> f opts (BL.fromStrict out)
  where
    http ext = byExtension ext <|> html
    html = P.getReader "html"
    plain = P.StringReader . const $ pure . readPlain
    gopher = pure . P.StringReader . const $ pure . readGopher
    byExtension "" = Left "No extension"
    byExtension ".md" = P.getReader "markdown"
    byExtension ".htm" = html
    byExtension ".ltx" = P.getReader "latex"
    byExtension ".tex" = P.getReader "latex"
    byExtension ".txt" = pure . P.StringReader . const $ pure . readPlain
    byExtension ext = P.getReader $ tail ext


-- * Rendering

-- | Renderer state.
data RS = RS { indentationLevel :: Int
             , linkCount :: Int
             , lineNumber :: Int
             , bulleted :: Bool
             , ordered :: Maybe Int
             , columns :: Int
             } deriving (Show, Eq)

-- | This is what gets rendered.
data RendererOutput = RLink URI
                    | RLine (Colored String)
                    | RIdentifier String Int
                    deriving (Show, Eq)

-- | Extracts links.
rLinks :: [RendererOutput] -> [URI]
rLinks [] = []
rLinks ((RLink l):xs) = l : rLinks xs
rLinks (_:xs) = rLinks xs

-- | Extracts text lines.
rLines :: [RendererOutput] -> [Colored String]
rLines [] = []
rLines ((RLine l):xs) = l : rLines xs
rLines (_:xs) = rLines xs

-- | Extracts identifiers.
rIdentifiers :: [RendererOutput] -> [(String, Int)]
rIdentifiers [] = []
rIdentifiers ((RIdentifier s i):xs) = (s, i) : rIdentifiers xs
rIdentifiers (_:xs) = rIdentifiers xs


-- | Used to render 'Pandoc' docs by writing text lines and collected
-- links using 'Writer'.
type Renderer a = WriterT [RendererOutput] (State RS) a

-- | Runs a 'Renderer'.
runRenderer :: Int
            -- ^ Column count (line width).
            -> Int
            -- ^ Link number to start with.
            -> Int
            -- ^ Line number to start with.
            -> Renderer a
            -- ^ A renderer.
            -> [RendererOutput]
            -- ^ Collected links and text lines.
runRenderer cols ls ln r = snd $ fst $ runState (runWriterT r)
  (RS 0 ls ln False Nothing cols)

-- | Stores a link, increasing the counter
storeLink :: URI -> Renderer Int
storeLink u = do
  tell [RLink u]
  st <- get
  put (st { linkCount = linkCount st + 1 })
  pure $ linkCount st

-- | Stores text lines.
storeLines :: [Colored String] -> Renderer ()
storeLines l = do
  modify (\s -> s { lineNumber = lineNumber s + length l })
  tell $ map RLine l

-- | Stores attributes (identifier and line number).
storeAttr :: P.Attr -> Renderer ()
storeAttr ("", _, _) = pure ()
storeAttr (i, _, _) = do
  l <- get
  tell [RIdentifier i (lineNumber l)]

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
      pad = map (fromString (replicate indent ' ') <>)
  case ( fitLines ((columns st) - indent) strings
       , bulleted st
       , ordered st) of
    ([], _, _) -> pure ()
    (x:xs, True, _) -> storeLines $
      (fromString (replicate (indentationLevel st) ' ') <> Fg Yellow "* " <> x)
      : pad xs
    (x:xs, _, Just n) -> do
      storeLines $ (mconcat [ fromString (replicate (indentationLevel st) ' ')
                            , Fg Yellow $ fromString (show n ++ ".")
                            , " "
                            , x])
        : pad xs
      modify (\s -> s { ordered = Just (n + 1) })
    (xs, _, _) -> storeLines $ pad xs

-- This may be unreliable, especially for resulting length estimation,
-- but usually works. Maybe improve someday.
-- | Returns a string as it would be shown on a dumb terminal.
uncolored :: Colored String -> String
uncolored s = showColoredS TermDumb s ""

-- todo: deal with non-breaking spaces
-- | Fits words into terminal lines of a given width.
fitLines :: Int
         -- ^ Line width.
         -> [Colored String]
         -- ^ Strings: usually words and similar short elements.
         -> [Colored String]
         -- ^ Fitted lines.
fitLines maxLen inlineBits =
  map mconcat $ map reverse $ fitWords [] 0 inlineBits
  where
    -- handle newline characters
    fitWords curLine _ ("\n":ws) = curLine : fitWords [] 0 ws
    -- a new line
    fitWords _ 0 (w:ws) = fitWords [w] (length $ uncolored w) ws
    -- add a word to a line
    fitWords curLine curLen (w:ws) = let wLen = length (uncolored w)
                                         spaceAhead = case ws of
                                           (" " : _) -> True
                                           _ -> False
      in if curLen + wLen <= maxLen
         then fitWords (w:curLine) (curLen + wLen) $
              -- if there's an unnecessary space ahead, skip it
              if (curLen + wLen + 1 > maxLen && spaceAhead)
              then tail ws
              else ws
         else curLine : fitWords [] 0 (w:ws)
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
readInline (P.Code attr s) = do
  storeAttr attr
  pure $ map fromString $ intersperse "\n" $ lines s
readInline P.Space = pure . pure $ fromString " "
readInline P.SoftBreak = pure . pure $ fromString " "
readInline P.LineBreak = pure . pure $ fromString "\n"
readInline (P.Math _ s) = pure . pure $ fromString s
readInline (P.RawInline _ s) = pure . pure $ fromString s
readInline (P.Link attr alt (url, title)) = do
  storeAttr attr
  case parseURIReference url of
    Just uri -> do
      a <- mapM readInline alt
      let t = case (title, a) of
            ("", []) -> [fromString url]
            ("", alt') -> concat alt'
            (title', []) -> [fromString title']
            (_, alt') -> concat alt'
      cnt <- storeLink uri
      let color = case uri of
            (URI "" Nothing "" "" ('#':_)) -> Magenta
            _ -> Cyan
      pure $ map (Fg color) t ++
        [Fg Blue (mconcat ["[", fromString $ show cnt, "]"])]
    Nothing -> pure . pure $ fromString title
readInline (P.Image attr alt (url, title)) =
  (Fg Red "(image) " :) <$> readInline (P.Link attr alt (url, title))
readInline (P.Note _) = pure $ pure "(note: todo)"
readInline (P.Span attr i) = do
  storeAttr attr
  concat <$> mapM readInline i

-- | Renders a block element.
renderBlock :: P.Block -> Renderer ()
renderBlock (P.Plain i) = indented =<< concat <$> mapM readInline i
renderBlock (P.Para i) = do
  indented =<< concat <$> mapM readInline i
  storeLines [""]
renderBlock (P.LineBlock i) =
  indented =<< concatMap mconcat <$> mapM (mapM readInline) i
renderBlock (P.CodeBlock attr s) = do
  storeAttr attr
  indented $ map fromString $ intersperse "\n" $ lines s
renderBlock (P.RawBlock _ s) =
  indented $ map fromString $ intersperse "\n" $ lines s
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
renderBlock (P.Header _ attr i) = do
  storeAttr attr
  strings <- concat <$> mapM readInline i
  indented $ "\n" : map (Fg Green . Style Bold . Style Underline) strings
renderBlock P.HorizontalRule = do
  st <- get
  indented [fromString $ replicate (columns st - indentationLevel st * 2) '-']
renderBlock (P.Table caption _ widths headers rows) = do
  -- todo: don't ignore alignments, improve relative widths
  -- calculation and handling.
  indented =<< concat <$> mapM readInline caption
  mapM_ (\r -> renderBlock P.HorizontalRule >> tableRow r) (headers : rows)
  renderBlock P.HorizontalRule
  where
    tableCell :: Int -> [P.Block] -> Renderer [Colored String]
    tableCell w blocks = do
      st <- get
      let l = runRenderer w (linkCount st) (lineNumber st) $
            mapM_ renderBlock blocks
      mapM_ storeLink $ rLinks l
      pure $ map
        (\x -> x <> Value (replicate (w - length (uncolored x)) ' '))
        $ rLines l
    tableRow :: [[P.Block]] -> Renderer ()
    tableRow cols = do
      st <- get
      let maxWidth = columns st - indentationLevel st
          widths' = map (\w -> floor (fromIntegral maxWidth * w - 3)) $
            if any (/= 0) widths
            then widths
            else replicate (length widths) (1 / fromIntegral (length widths))
      cells <- zipWithM tableCell widths' cols
      let maxLines = foldr (max . length) 0 cells
          padded = zipWith (\w c -> c ++ replicate (maxLines - length c)
                             (fromString $ replicate w ' ')) widths' cells
      indented $ map (mconcat . intersperse (Value " | ")) $ transpose padded
renderBlock (P.Div attr b) = do
  storeAttr attr
  renderBlocks b
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
                     -- ^ Enable pagination in non-embedded mode;
                     -- print everything at once otherwise.
                     , historyDepth :: Int
                     -- ^ The amount of history entries (into either
                     -- direction) to keep.
                     } deriving (Generic, Show)

-- | For configuration parsing.
instance FromJSON Config
-- | For configuration writing, particularly that of default
-- configuration if it is missing.
instance ToJSON Config
-- | The default configuration to use if user configuration is
-- missing.
instance Default Config where
  def = Config {
    commands = M.fromList
      [ ("ssh", "scp \"${URI_REGNAME}:${URI_PATH}\" /dev/stdout")
      , ("gopher", "curl \"${URI}\"")]
    , defaultCommand = "curl -4 -L \"${URI}\""
    , externalViewers = M.fromList $
      map (flip (,) "emacsclient -n \"${FILE}\"")
      ["hs", "cabal", "c", "h", "el", "scm", "idr"]
      ++ map (flip (,) "xdg-open \"${FILE}\"")
      [ "svg", "png", "jpg", "jpeg", "gif", "pdf", "ogg", "ogv"
      , "webm", "mp3", "mp4", "mkv", "mpeg", "wav" ]
    , shortcuts = M.fromList
      [ ("ddg", "https://duckduckgo.com/lite/?q=")
      , ("wp", "https://en.m.wikipedia.org/wiki/Special:Search?search=")
      , ("wt", "https://en.m.wiktionary.org/w/index.php?search=")
      , ("gp", "gopher://gopherpedia.com:70/7/lookup?")
      , ("vs", "gopher://gopher.floodgap.com/7/v2/vs?")]
    , paginate = True
    , historyDepth = 100
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
        Nothing -> putErrLn "Failed to read the configuration, using defaults"
          >> pure def
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
type Sliding a = ([a], [a])

-- | Main event loop's state.
data LoopState = LS { history :: Sliding (URI, P.Pandoc)
                    , position :: Int
                    , rendered :: [RendererOutput]
                    , conf :: Config
                    , embedded :: Bool
                    } deriving (Show)

-- | Prints rendered lines.
showLines :: MonadIO m => [Colored String] -> StateT LoopState m ()
showLines ls = liftIO $ do
  term <- getTerm
  mapM_ (\s -> printColoredS term s >> putChar '\n') ls

-- | Shows a list of strings as an s-expression
list :: [String] -> String
list l = "(" ++ intercalate " " l ++ ")"

-- | Prints a list of strings as an s-expression.
putSexpLn :: MonadIO m => [String] -> StateT LoopState m ()
putSexpLn s = liftIO $ do
  putStrLn $ list s
  SIO.hFlush SIO.stdout

-- | Prints rendered lines as s-expressions.
showSexps :: MonadIO m => [RendererOutput] -> StateT LoopState m ()
showSexps ro =
  -- would be nicer to use some library for this, but they tend to be
  -- abandoned, and the task is simple enough to do it here
  putSexpLn [ "render"
            , list $ "lines" : map (list . pure . showSexp) (rLines ro)
            , list $ "identifiers"
              : map (\(i, l) -> list [encodeStr i, show l]) (rIdentifiers ro)
            , list $ "links"
              : map (\uri -> encodeStr $ uriToString id uri "") (rLinks ro)]
  where
    encodeStr s = concat ["\"", concatMap escape s, "\""]
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape other = pure other
    showSexp :: Colored String -> String
    -- no need for nils since the pairs are flattened
    showSexp Nil = ""
    showSexp (Value x) = encodeStr x
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
      l = runRenderer cols 0 1 $ mapM_ renderBlock blocks
      textLines = rLines l
  modify (\s -> s { rendered = l })
  if embedded st
    then showSexps l
    else do
    let rows = maybe 25 id (TI.getCapability term TI.termLines) - 1
    showLines $ if paginate (conf st)
                then take rows textLines
                else textLines
    modify (\s -> s { position = rows })

-- | Decides what to do with a given URI; either returns a document or
-- runs an external viewer. Used by both 'GoTo' and 'Reload'.
loadDocument :: MonadIO m => URI -> StateT LoopState m (URI, Maybe P.Pandoc)
loadDocument u' = do
  st <- get
  let ddg = isPrefixOf "/l/?kh=-1&uddg=" $ uriToString id u' ""
      u = case (ddg, uriIsAbsolute u', history st) of
            -- fix DDG links (that's rather hacky, todo: improve)
            (True, _, _) -> maybe u' id $
              parseAbsoluteURI (unEscapeString $ drop 12 (uriQuery u'))
            -- handle relative URIs
            (_, False, ((cur, _):_, _)) -> relativeTo u' cur
            _ -> u'
      cmd = maybe (defaultCommand $ conf st) id $
        M.lookup (init $ uriScheme u) (commands $ conf st)
      ext = case takeExtension $ uriPath u of
        ('.':xs) -> map toLower xs
        other -> other
  d <- liftIO $ do
    case M.lookup ext (externalViewers $ conf st) of
      Nothing -> do
        doc <- readDoc cmd u
        case doc of
          Left err -> do
            putErrLn $ show err
            pure mzero
          Right r -> pure $ pure r
      Just ev -> do
        d <- retrieve cmd u
        dir <- getXdgDirectory XdgCache "pancake"
        let tmpPath = dir </> (takeFileName $ uriPath u)
        handle
          (\(e :: SomeException) ->
             putErrLn (concat ["Failed to open `", tmpPath, "` with `"
                              , cmd, "`: ", show e])) $ do
          createDirectoryIfMissing True dir
          BS.writeFile tmpPath d
          curEnv <- getEnvironment
          ec <- withCreateProcess
            ((shell ev) { env = Just (("FILE", tmpPath) : curEnv) }) $
            \_ _ _ p -> waitForProcess p
          when (ec /= ExitSuccess) $
            putErrLn $ concat ["An error occured. Exit code: ", show ec]
        pure mzero
  pure (u, d)

-- | Visits an URI, updates history accordingly.
goTo :: MonadIO m => URI -> StateT LoopState m ()
goTo u' = do
  (u, d) <- loadDocument u'
  case d of
    Nothing -> pure ()
    Just doc -> do
      renderDoc doc
      modify $ \s ->
        let (prev, _) = history s
        in s { history = (take (historyDepth $ conf s) $ (u, doc) : prev, []) }

-- | Evaluates user commands.
command :: MonadIO m => Command -> StateT LoopState m ()
command (GoTo u@(URI _ _ _ _ ('#':xs))) = do
  -- follow an URI first, if it's not just a fragment
  case u of
    (URI "" Nothing "" "" _) -> pure ()
    _ -> goTo u
  -- get to the fragment
  st <- get
  case (lookup xs (rIdentifiers $ rendered st), embedded st) of
    (Nothing, _) -> putErrLn $ "Unknown identifier: " ++ xs
    (Just x, False) -> do
      term <- liftIO TI.setupTermFromEnv
      let lineCount = maybe 25 id (TI.getCapability term TI.termLines)
      when (x + lineCount - 2 > position st) $ do
        -- scroll to the given position without skipping anything
        showLines $ take (x - position st + lineCount - 2) $
          drop (position st) (rLines $ rendered st)
        modify (\s -> s { position = x + lineCount - 2 })
    (Just x, True) -> putSexpLn ["goto", show x]
command (GoTo u) = goTo u
command (Follow i) = do
  st <- get
  if length (rLinks $ rendered st) > i
    then command (GoTo $ rLinks (rendered st) !! i)
    else liftIO $ putErrLn "No such link"
command Back = do
  st <- get
  case history st of
    (cur:p@(_, d):prev, next) -> do
      renderDoc d
      modify $ \s ->
        s { history = (p:prev, take (historyDepth $ conf s) $ cur : next) }
    _ -> liftIO $ putErrLn "There's nothing back there"
command Forward = do
  st <- get
  case history st of
    (prev, n@(_, d):next) -> do
      renderDoc d
      modify $ \s ->
        s { history = (take (historyDepth $ conf s) $ n : prev, next) }
    _ -> liftIO $ putErrLn "Nowhere to go"
command More = do
  st <- get
  term <- liftIO TI.setupTermFromEnv
  let lineCount' = maybe 25 id (TI.getCapability term TI.termLines)
      lineCount = lineCount' - div lineCount' 3
  showLines $ take lineCount $ drop (position st) (rLines $ rendered st)
  modify (\s -> s { position = position st + lineCount })
  pure ()
command Reload = do
  st <- get
  case history st of
    ((u, _):prev, next) -> do
      (_, d) <- loadDocument u
      case d of
        Nothing -> pure ()
        Just doc -> do
          renderDoc doc
          modify $ \s -> s { history = ( (u, doc):prev, next ) }
    _ -> putErrLn "There's nothing to reload"
command Help = do
  st <- get
  liftIO $ do
    putErrLn "[q]uit, [b]ack, [f]orward, [h]elp, [r]eload, [re]load config"
    putErrLn "type a number to follow a link, \"<number>?\" to print its URI"
    putErrLn "type an URI (absolute or relative) to open it"
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
    LS ([],[]) 0 [] def (isJust insideEmacs || "--embedded" `elem` args)
  pure ()
