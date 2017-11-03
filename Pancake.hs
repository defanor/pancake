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
import System.Console.Terminfo
import System.Environment
import Data.Yaml
import GHC.Generics
import qualified Data.Map as M
import System.Directory
import System.Exit
import GHC.IO.Handle
import Control.Exception
import Text.Pandoc.Readers.Plain
import Text.Pandoc.Readers.Gopher
import Control.Applicative
import qualified System.IO as SIO
import Data.Char
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import System.IO.Error


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
  term <- setupTermFromEnv
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
      cols = maybe 80 id $ getCapability term termColumns
      opts = def { P.readerColumns = cols }
  P.runIO $ case reader of
    (P.TextReader f, _) -> f opts $ case decodeUtf8' out of
                                      Left _ -> T.empty
                                      Right r -> r
    (P.ByteStringReader f, _) -> f opts $ BL.fromStrict out
  where
    http ext = byExtension ext <|> html
    html = P.getReader "html"
    plain = (P.TextReader . const $ readPlain, P.emptyExtensions)
    gopher = pure (P.TextReader . const $ readGopher, P.emptyExtensions)
    byExtension "" = Left "No extension"
    byExtension ".md" = P.getReader "markdown"
    byExtension ".htm" = html
    byExtension ".ltx" = P.getReader "latex"
    byExtension ".tex" = P.getReader "latex"
    byExtension ".txt" = pure plain
    byExtension ext = P.getReader $ tail ext


-- * Rendering

-- | The type of a list item that should be rendered next.
data Listing = Bulleted
             | Ordered Int
             deriving (Show, Eq)

-- | Renderer state.
data RS = RS { indentationLevel :: Int
             , linkCount :: Int
             , lineNumber :: Int
             , listing :: Maybe Listing
             , columns :: Int
             } deriving (Show, Eq)

-- | A styled string.
data Styled = Plain String
            | Underline Styled
            | Bold Styled
            | Emph Styled
            | Fg Color Styled
            deriving (Show, Eq)

-- | Just for convenience.
instance IsString Styled where
  fromString = Plain

-- | A line of styled elements.
type StyledLine = [Styled]

-- | This is what gets rendered.
data RendererOutput = RLink URI
                    | RLine StyledLine
                    | RIdentifier String Int
                    deriving (Show, Eq)

-- | Extracts links.
rLinks :: [RendererOutput] -> [URI]
rLinks [] = []
rLinks ((RLink l):xs) = l : rLinks xs
rLinks (_:xs) = rLinks xs

-- | Extracts text lines.
rLines :: [RendererOutput] -> [StyledLine]
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
  (RS 0 ls ln Nothing cols)

-- | Stores a link, increasing the counter
storeLink :: URI -> Renderer Int
storeLink u = do
  tell [RLink u]
  st <- get
  put (st { linkCount = linkCount st + 1 })
  pure $ linkCount st

-- | Stores text lines.
storeLines :: [StyledLine] -> Renderer ()
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

-- | Reads indentation level, runs a renderer, restores the original
-- indentation level.
keepIndent :: Renderer a -> Renderer a
keepIndent r = do
  st <- get
  ret <- r
  modify $ \s -> s { indentationLevel = indentationLevel st }
  pure ret

-- | Renders indented (with the current indent level) lines.
indented :: [StyledLine] -> Renderer ()
indented slines = do
  st <- get
  -- The following blocks of the same list item should not be marked.
  modify $ \s -> s { listing = Nothing }
  let il = indentationLevel st
      prefix = case listing st of
        Nothing -> ""
        (Just Bulleted) -> Fg Yellow "* "
        (Just (Ordered n)) -> Fg Yellow $ fromString $ show n ++ ". "
      prefixLen = length $ unstyled [prefix]
      indent = il + prefixLen
      fittedLines = fitLines (columns st - indent) slines
      pad = (fromString (replicate indent ' ') :)
      padFirst = (\x -> fromString (replicate il ' ') : prefix : x)
  -- The following blocks of the same list item should be indented
  -- with the same level. This should be reset to the original value
  -- where the listing type is getting set.
  modify $ \s -> s { indentationLevel = indent }
  case fittedLines of
    [] -> pure ()
    (l:ls) -> storeLines $ padFirst l : map pad ls

-- This may be unreliable, especially for resulting length estimation,
-- but usually works. Maybe improve someday.
-- | Returns a string as it would be shown on a dumb terminal.
unstyled :: StyledLine -> String
unstyled = concatMap unstyled'
  where
    unstyled' (Plain s) = s
    unstyled' (Main.Underline s) = unstyled' s
    unstyled' (Main.Bold s) = unstyled' s
    unstyled' (Main.Emph s) = unstyled' s
    unstyled' (Fg _ s) = unstyled' s

-- | Fits words into terminal lines of a given width.
fitLines :: Int
         -- ^ Line width.
         -> [[Styled]]
         -- ^ Strings: usually words and similar short elements.
         -> [StyledLine]
         -- ^ Fitted lines.
fitLines maxLen inlineBits = concatMap (map reverse . fitWords [] 0) inlineBits
  where
    fitWords :: [Styled] -> Int -> [Styled] -> [StyledLine]
    -- fitWords curLine curLen (w:ws) = [[fromString $ show (w:ws)]]
    fitWords curLine curLen (w:ws)
      -- handle newline characters
      | unstyled [w] == "\n" = curLine : fitWords [] 0 ws
      -- a new line
      | curLen == 0 = fitWords [w] (length $ unstyled [w]) ws
      -- add a word to a line
      | otherwise = let wLen = length (unstyled [w])
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
wrappedInlines :: Styled
               -- ^ String on the left.
               -> Styled
               -- ^ String on the right.
               -> [P.Inline]
               -- ^ Inlines to wrap.
               -> Renderer [Styled]
               -- ^ Resulting inlines.
wrappedInlines s e r = do
  r' <- concat <$> mapM readInline r
  pure $ s : r' ++ [e]

-- | Reads an inline element, producing styled strings. Doesn't render
-- them (i.e., using 'Writer') on its own, but collects links.
readInline :: P.Inline -> Renderer [Styled]
readInline (P.Str s)
  | all isSpace s = pure []
  | otherwise = pure [fromString s]
readInline (P.Emph s) = concatMap (map Main.Emph) <$> mapM readInline s
readInline (P.Strong s) = concatMap (map Main.Bold) <$> mapM readInline s
readInline (P.Strikeout s) = wrappedInlines "-" "-" s
readInline (P.Superscript s) = wrappedInlines "^{" "}" s
readInline (P.Subscript s) = wrappedInlines "_{" "}" s
readInline (P.SmallCaps s) = wrappedInlines "\\sc{" "}" s
readInline (P.Quoted P.SingleQuote s) = wrappedInlines "‘" "’" s
readInline (P.Quoted P.DoubleQuote s) = wrappedInlines "“" "”" s
readInline (P.Cite _ s) = concat <$> mapM readInline s
readInline (P.Code attr s) = do
  storeAttr attr
  pure . map fromString $ intersperse "\n" $ lines s
readInline P.Space = pure [" "]
readInline P.SoftBreak = pure [" "]
readInline P.LineBreak = pure ["\n"]
readInline (P.Math _ s) = pure [fromString s]
readInline (P.RawInline _ s) = pure [fromString s]
readInline (P.Link attr alt (url, title)) = do
  storeAttr attr
  case parseURIReference url of
    Just uri -> do
      a <- mapM readInline alt
      let t = case (title, concat a) of
            ("", []) -> [fromString url]
            ("", alt') -> alt'
            (title', []) -> [fromString title']
            (_, alt') -> alt'
      cnt <- storeLink uri
      let color = case uri of
            (URI "" Nothing "" "" ('#':_)) -> Magenta
            _ -> Cyan
      pure $ (map $ Fg color) t ++
        [Fg Blue $ fromString (concat ["[", show cnt, "]"])]
    Nothing -> pure [fromString title]
readInline (P.Image attr alt (url, title)) = do
  storeAttr attr
  (Fg Red "img:" :) <$> case parseURIReference url of
    Nothing -> pure [fromString title]
    Just uri -> do
      a <- mapM readInline alt
      let t = case (title, concat a) of
            ("", []) -> [fromString $ takeFileName $ uriPath uri]
            ("", alt') -> alt'
            (title', []) -> [fromString title']
            (_, alt') -> alt'
      cnt <- storeLink uri
      pure $ (map $ Fg Cyan) t ++
        [Fg Blue $ fromString (concat ["[", show cnt, "]"])]
readInline (P.Note _) = pure . pure $ "(note: todo)"
readInline (P.Span attr i) = do
  storeAttr attr
  concat <$> mapM readInline i

-- | Reads lines of inline elements.
readInlines :: [P.Inline] -> Renderer [StyledLine]
readInlines i = pure . concat <$> mapM readInline i

-- | Renders a block element.
renderBlock :: P.Block -> Renderer ()
renderBlock (P.Plain i) = indented =<< readInlines i
renderBlock (P.Para i) = (indented =<< readInlines i) >> storeLines [[""]]
renderBlock (P.LineBlock i) =
  indented =<< concat <$> mapM (mapM readInline) i
renderBlock (P.CodeBlock attr s) = do
  storeAttr attr
  indented $ map (pure . fromString) $ lines s
renderBlock (P.RawBlock _ s) =
  indented $ map (pure . fromString) $ lines s
renderBlock (P.BlockQuote bs) = renderBlocks bs
renderBlock (P.OrderedList _ bs) = do
  zipWithM_ (\b n -> modify (\s -> s { listing = Just (Ordered n) })
                     >> keepIndent (mapM_ renderBlock b)) bs [1..]
  modify $ \s -> s { listing = Nothing }
renderBlock (P.BulletList bs) = do
  mapM_ (\b -> modify (\s -> s { listing = Just Bulleted })
               >> keepIndent (mapM_ renderBlock b)) bs
  modify $ \s -> s { listing = Nothing }
renderBlock (P.DefinitionList dl) =
  let renderDefinition (term, definition) = do
        indented =<< readInlines term
        mapM_ renderBlocks definition
  in mapM_ renderDefinition dl
renderBlock (P.Header level attr i) = do
  storeAttr attr
  strings <- readInlines i
  storeLines [[""]]
  indented $ map (map (Fg Green) . ([fromString (replicate level '#'), " "] ++)
                  . (map (Main.Bold . Main.Underline))) strings
  storeLines [[""]]
renderBlock P.HorizontalRule = do
  st <- get
  indented [[Fg Black $
             fromString $ replicate (columns st - indentationLevel st * 2) '-']]
renderBlock (P.Table caption _ widths headers rows) = do
  -- todo: don't ignore alignments
  indented =<< readInlines caption
  -- Use pandoc-provided widths if they are set, calculate them
  -- otherwise.
  let widthsAreSet = case widths of
        [] -> False
        w -> minimum w /= maximum w
  ws <- if widthsAreSet then pure widths else do
    lens <- map sum . transpose <$>
      mapM (mapM (\c -> (length . unstyled . concat) <$> tableCell 80 c)) rows
    pure $ map (\l -> fromIntegral l / fromIntegral (sum lens)) lens
  mapM_ (\r -> renderBlock P.HorizontalRule >> tableRow ws r) (headers : rows)
  renderBlock P.HorizontalRule
  where
    tableCell :: Int -> [P.Block] -> Renderer [StyledLine]
    tableCell w blocks = do
      st <- get
      let l = runRenderer w (linkCount st) (lineNumber st) $
            mapM_ renderBlock blocks
      mapM_ storeLink $ rLinks l
      pure $ map
        (\x -> x ++ [fromString (replicate (w - length (unstyled x)) ' ')])
        $ rLines l
    tableRow :: [Double] -> [[P.Block]] -> Renderer ()
    tableRow ws cols = do
      st <- get
      let maxWidth = columns st - indentationLevel st - ((length cols - 1) * 3)
          widths' = map (\w -> floor (fromIntegral maxWidth * w)) ws
      cells <- zipWithM tableCell widths' cols
      let maxLines = foldr (max . length) 0 cells
          padded = zipWith (\w c -> c ++ replicate (maxLines - length c)
                             [fromString $ replicate w ' ']) widths' cells
      indented $ map (mconcat . intersperse (pure $ Fg Black " | "))
        $ transpose padded
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

-- | Propertizes a styled string for a given terminal.
propertize :: Terminal -> Styled -> TermOutput
propertize _ (Plain s) = termText s
propertize t (Fg clr s) = maybe id (\f -> f clr)
  (getCapability t withForegroundColor) $ propertize t s
propertize t (Main.Bold s) =
  maybe id id (getCapability t withBold) $ propertize t s
propertize t (Main.Emph s) =
  maybe id id (getCapability t withStandout) $ propertize t s
propertize t (Main.Underline s) =
  maybe id id (getCapability t withUnderline) $ propertize t s

-- | Prints rendered lines.
showLines :: MonadIO m => [StyledLine] -> StateT LoopState m ()
showLines ls = liftIO $ do
  term <- setupTermFromEnv
  let nl = maybe (termText "\n") id $ getCapability term newline
  runTermOutput term . mconcat $
    map (\l -> mconcat (map (propertize term) l) <#> nl) ls

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
            , list $ "lines" :
              map (list . pure . concat . intersperse " " . map showSexp)
              (rLines ro)
            , list $ "identifiers"
              : map (\(i, l) -> list [encodeStr i, show l]) (rIdentifiers ro)
            , list $ "links"
              : map (\uri -> encodeStr $ uriToString id uri "") (rLinks ro)]
  where
    encodeStr s = concat ["\"", concatMap escape s, "\""]
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape other = pure other
    showSexp :: Styled -> String
    showSexp (Plain s) = encodeStr s
    showSexp (Fg clr s) = list ["fg", show clr, showSexp s]
    showSexp (Bold s) = list ["style", "bold", showSexp s]
    showSexp (Underline s) = list ["style", "underline", showSexp s]
    showSexp (Emph s) = list ["style", "italic", showSexp s]

-- | Renders a parsed document.
renderDoc :: MonadIO m => P.Pandoc -> StateT LoopState m ()
renderDoc (P.Pandoc _ blocks) = do
  term <- liftIO setupTermFromEnv
  st <- get
  let cols = maybe 80 id $ getCapability term termColumns
      l = runRenderer cols 0 1 $ mapM_ renderBlock blocks
      textLines = rLines l
  modify (\s -> s { rendered = l })
  if embedded st
    then showSexps l
    else do
    let rows = maybe 25 id (getCapability term termLines) - 1
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
      uScheme = case uriScheme u of
        [] -> "unknown"
        (_:s) -> s
      cmd = maybe (defaultCommand $ conf st) id $
        M.lookup uScheme (commands $ conf st)
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
      term <- liftIO setupTermFromEnv
      let lineCount = maybe 25 id (getCapability term termLines)
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
  term <- liftIO setupTermFromEnv
  let lineCount' = maybe 25 id (getCapability term termLines)
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
  cmd' <- liftIO $ try getLine
  let onErr e = unless (isEOFError e)
                (putErrLn ("Unexpected error: " ++ show e))
                >> pure Quit
  st <- get
  c <- flip (either onErr) cmd' $ \cmd -> pure $
    case cmd of
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
  _ <- runStateT (loadConfig >> eventLoop) $
    LS ([],[]) 0 [] def ("--embedded" `elem` args)
  pure ()
