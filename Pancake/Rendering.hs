{- |
Module      :  Pancake.Rendering
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Document rendering: conversion from 'Pandoc' to 'RendererOutput'.
-}

{-# LANGUAGE OverloadedStrings #-}

module Pancake.Rendering ( Styled(..)
                         , StyledLine
                         , RendererOutput(..)
                         , rLinks
                         , rLines
                         , rIdentifiers
                         , renderDoc
                         ) where

import qualified Text.Pandoc as P
import Network.URI
import Data.List
import System.Console.Terminfo.Color
import Data.String
import Control.Monad.Writer
import Control.Monad.State
import System.FilePath
import Data.Char


-- | The type of a list item that should be rendered next.
data Listing = Bulleted
             | Ordered Int
             deriving (Show, Eq)

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

-- | Renderer state.
data RS = RS { indentationLevel :: Int
             , linkCount :: Int
             , lineNumber :: Int
             , listing :: Maybe Listing
             , columns :: Int
             } deriving (Show, Eq)

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
    unstyled' (Underline s) = unstyled' s
    unstyled' (Bold s) = unstyled' s
    unstyled' (Emph s) = unstyled' s
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
readInline (P.Emph s) = concatMap (map Emph) <$> mapM readInline s
readInline (P.Strong s) = concatMap (map Bold) <$> mapM readInline s
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
                  . (map (Bold . Underline))) strings
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
      mapM (mapM (\c -> (length . unstyled . concat . rLines)
                   <$> renderCell 80 c)) rows
    pure $ map (\l -> fromIntegral l / fromIntegral (sum lens)) lens
  mapM_ (\r -> renderBlock P.HorizontalRule >> tableRow ws r) (headers : rows)
  renderBlock P.HorizontalRule
  where
    renderCell :: Int -> [P.Block] -> Renderer [RendererOutput]
    renderCell w blocks = do
      st <- get
      pure $ runRenderer w (linkCount st) (lineNumber st) $
        mapM_ renderBlock blocks
    tableCell :: Int -> [P.Block] -> Renderer [StyledLine]
    tableCell w blocks = do
      l <- renderCell w blocks
      mapM_ storeLink $ rLinks l
      tell $ map (\(s, i) -> RIdentifier s i) $ rIdentifiers l
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

-- | Renders a document.
renderDoc :: Int
          -- ^ Number of columns.
          -> P.Pandoc
          -- ^ Document to render.
          -> [RendererOutput]
          -- ^ Rendered document.
renderDoc cols (P.Pandoc _ blocks) =
  runRenderer cols 0 1 $ mapM_ renderBlock blocks
