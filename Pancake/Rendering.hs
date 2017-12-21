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
Module      :  Pancake.Rendering
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Document rendering: conversion from 'Pandoc' to 'RendererOutput'.
-}

{-# LANGUAGE OverloadedStrings #-}

module Pancake.Rendering ( Denotation(..)
                         , Styled(..)
                         , StyledLine
                         , RendererOutput(..)
                         , rLinks
                         , rLines
                         , rIdentifiers
                         , rBlocks
                         , rNotes
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
import Numeric

import Pancake.Configuration


-- | The type of a list item that should be rendered next.
data Listing = Bulleted
             | Ordered Int
             deriving (Show, Eq)

-- | Denotations: information that can be ignored, but can also be
-- used to improve the UI.
data Denotation = Link URI
                | Image URI
                | Math String
                | Heading Int
                deriving (Show, Eq)

-- | A styled string.
data Styled = Plain String
            | Underline Styled
            | Bold Styled
            | Emph Styled
            | Strikethrough Styled
            | Subscript Styled
            | Superscript Styled
            | Fg Color Styled
            | Denote Denotation Styled
            deriving (Show, Eq)

-- | Just for convenience.
instance IsString Styled where
  fromString = Plain

-- | A line of styled elements.
type StyledLine = [Styled]

-- | Renderer state.
data RS = RS { indentationLevel :: Int
             , linkCount :: Int
             , noteCount :: Int
             , lineNumber :: Int
             , listing :: Maybe Listing
             , columns :: Int
             , rsConf :: Config
             } deriving (Show, Eq)

-- | This is what gets rendered.
data RendererOutput = RLink URI
                    -- ^ An URI reference.
                    | RNote [RendererOutput]
                    -- ^ A note.
                    | RLine StyledLine
                    -- ^ A line to render.
                    | RIdentifier String Int
                    -- ^ An identifier.
                    | RBlock Int Int
                    -- ^ A fixed block's position (start line and end
                    -- line). These blocks reflect semantics and don't
                    -- vary as the terminal width changes, so they are
                    -- safe to rely on for position retention.
                    deriving (Show, Eq)

-- | Show a reference.
showRef :: String -> Int -> String
showRef digits n = showIntAtBase (length digits) (digits !!) n ""

-- | Extracts links.
rLinks :: [RendererOutput] -> [URI]
rLinks [] = []
rLinks (RLink l:xs) = l : rLinks xs
rLinks (_:xs) = rLinks xs

-- | Extracts text lines.
rLines :: [RendererOutput] -> [StyledLine]
rLines [] = []
rLines (RLine l:xs) = l : rLines xs
rLines (_:xs) = rLines xs

-- | Extracts identifiers.
rIdentifiers :: [RendererOutput] -> [(String, Int)]
rIdentifiers [] = []
rIdentifiers (RIdentifier s i:xs) = (s, i) : rIdentifiers xs
rIdentifiers (_:xs) = rIdentifiers xs

-- | Extracts fixed block positions.
rBlocks :: [RendererOutput] -> [(Int, Int)]
rBlocks [] = []
rBlocks (RBlock s e:xs) = (s, e) : rBlocks xs
rBlocks (_:xs) = rBlocks xs

-- | Extracts notes.
rNotes :: [RendererOutput] -> [[RendererOutput]]
rNotes [] = []
rNotes (RNote l:xs) = l : rNotes xs
rNotes (_:xs) = rNotes xs

-- | Used to render 'Pandoc' docs by writing text lines and collected
-- links using 'Writer'.
type Renderer a = WriterT [RendererOutput] (State RS) a

-- | Runs a 'Renderer'.
runRenderer :: Int
            -- ^ Column count (line width).
            -> Int
            -- ^ Link number to start with.
            -> Int
            -- ^ Note number to start with.
            -> Int
            -- ^ Line number to start with.
            -> Config
            -- ^ Configuration.
            -> Renderer a
            -- ^ A renderer.
            -> [RendererOutput]
            -- ^ Collected links and text lines.
runRenderer cols ls ns ln cnf r =
  let o = snd $ evalState (runWriterT r)
          (RS 0 ls ns ln Nothing cols cnf)
  in o ++ concatMap (map RLine . rLines) (rNotes o)

-- | Stores a link, increasing the counter.
storeLink :: URI -> Renderer Int
storeLink u = do
  tell [RLink u]
  st <- get
  put (st { linkCount = linkCount st + 1 })
  pure $ linkCount st

-- | Stores a note, increasing the counter.
storeNote :: [RendererOutput] -> Renderer Int
storeNote ro = do
  st <- get
  put $ st { noteCount = noteCount st + 1 }
  mapM_ storeLink $ rLinks ro
  let cnt = noteCount st
      mark = Superscript . Fg Red . fromString $ "note " ++ show cnt
      ro' = case ro of
        (RLine l:rest) -> RLine (mark:l):rest
        _ -> RLine [mark] : ro
  tell [RNote ro']
  pure cnt

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
      padFirst x = fromString (replicate il ' ') : prefix : x
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
    unstyled' (Strikethrough s) = unstyled' s
    unstyled' (Subscript s) = unstyled' s
    unstyled' (Superscript s) = unstyled' s
    unstyled' (Fg _ s) = unstyled' s
    unstyled' (Denote _ s) = unstyled' s

-- | Fits words into terminal lines of a given width.
fitLines :: Int
         -- ^ Line width.
         -> [[Styled]]
         -- ^ Strings: usually words and similar short elements.
         -> [StyledLine]
         -- ^ Fitted lines.
fitLines 0 _ = []
fitLines maxLen inlineBits = concatMap (map reverse . fitWords') inlineBits
  where
    splitStyled :: Styled -> [Styled]
    splitStyled (Plain s)
      | length s > maxLen =
        case reverse (takeWhile (<= maxLen) (findIndices isSpace s)) of
          (n:_) -> let (t, _:d) = splitAt n s
                   in Plain t : splitStyled (Plain d)
          [] -> let (t, d) = splitAt maxLen s
                in Plain t : splitStyled (Plain d)
      | otherwise = [Plain s]
    splitStyled (Underline s) = map Underline $ splitStyled s
    splitStyled (Bold s) = map Bold $ splitStyled s
    splitStyled (Emph s) = map Emph $ splitStyled s
    splitStyled (Strikethrough s) = map Strikethrough $ splitStyled s
    splitStyled (Subscript s) = map Subscript $ splitStyled s
    splitStyled (Superscript s) = map Superscript $ splitStyled s
    splitStyled (Fg c s) = map (Fg c) $ splitStyled s
    splitStyled (Denote d s) = map (Denote d) $ splitStyled s
    fitWords' :: [Styled] -> [StyledLine]
    fitWords' ws
      -- handle empty lines
      | null (unstyled ws) = [[]]
      | otherwise = fitWords [] 0 ws
    fitWords :: [Styled] -> Int -> [Styled] -> [StyledLine]
    fitWords curLine curLen (w:ws)
      -- handle newline characters
      | unstyled [w] == "\n" = curLine : fitWords [] 0 ws
      -- a new line
      | curLen == 0 = if length (unstyled [w]) <= maxLen
                      then fitWords [w] (length $ unstyled [w]) ws
                      else map pure (splitStyled w) ++ fitWords [] 0 ws
      -- add a word to a line
      | otherwise = let wLen = length (unstyled [w])
                        spaceAhead = case ws of
                                       (" " : _) -> True
                                       _ -> False
        in if curLen + wLen <= maxLen
           then fitWords (w:curLine) (curLen + wLen) $
                -- if there's an unnecessary space ahead, skip it
                if curLen + wLen + 1 > maxLen && spaceAhead
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
readInline (P.Strikeout s) = map Strikethrough <$> wrappedInlines "-" "-" s
readInline (P.Superscript s) = map Superscript <$> wrappedInlines "^{" "}" s
readInline (P.Subscript s) = map Subscript <$> wrappedInlines "_{" "}" s
readInline (P.SmallCaps s) = wrappedInlines "\\sc{" "}" s
readInline (P.Quoted P.SingleQuote s) = wrappedInlines "‘" "’" s
readInline (P.Quoted P.DoubleQuote s) = wrappedInlines "“" "”" s
readInline (P.Cite _ s) = concat <$> mapM readInline s
readInline (P.Code attr s) = do
  storeAttr attr
  pure . map (Fg Green . fromString) $ intersperse "\n" $ lines s
readInline P.Space = pure [" "]
readInline P.SoftBreak = pure [" "]
readInline P.LineBreak = pure ["\n"]
readInline (P.Math _ s) = pure [Denote (Math s) $ fromString s]
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
      st <- get
      pure $ map (Denote (Link uri) . Fg color) t ++
        [Fg Blue $ fromString
         (concat ["[", showRef (referenceDigits $ rsConf st) cnt, "]"])]
    Nothing -> pure [fromString title]
readInline (P.Image attr alt (url, title)) = do
  storeAttr attr
  case parseURIReference url of
    Nothing -> pure [Fg Red "i", fromString title]
    Just uri -> do
      a <- mapM readInline alt
      let t = case (title, concat a) of
            ("", []) -> [fromString $ takeFileName $ uriPath uri]
            ("", alt') -> alt'
            (title', []) -> [fromString title']
            (_, alt') -> alt'
      cnt <- storeLink uri
      st <- get
      pure $ Denote (Image uri) (Fg Red "i") :
        map (Denote (Link uri) . Fg Cyan) t ++
        [Fg Blue $ fromString
         (concat ["[", showRef (referenceDigits $ rsConf st) cnt, "]"])]
readInline (P.Note bs) = do
  -- Minor issues are quite likely with this.
  st <- get
  -- 12 is somewhat arbitrary, but narrowing the rendered notes so
  -- that "^{note xxx}" could be added without overflow.
  let ro = runRenderer (columns st - 12) (linkCount st) (noteCount st) 0
           (rsConf st) (renderBlocks bs)
  cnt <- storeNote ro
  pure [Superscript . Fg Red . fromString $ "[" ++ show cnt ++ "]"]
readInline (P.Span attr i) = do
  storeAttr attr
  concat <$> mapM readInline i

-- | Reads lines of inline elements.
readInlines :: [P.Inline] -> Renderer [StyledLine]
readInlines i = do
  inlineBits <- concat <$> mapM readInline i
  pure $ pure inlineBits

-- | Akin to 'lines', but for 'Inline' elements.
inlines :: [P.Inline] -> [[P.Inline]]
inlines = flip inlines' []
  where inlines' :: [P.Inline] -> [P.Inline] -> [[P.Inline]]
        inlines' [] [] = []
        inlines' [] curLine = [curLine]
        inlines' (P.LineBreak : xs) curLine = curLine : inlines' xs []
        inlines' (other : xs) curLine = inlines' xs (curLine ++ [other])

-- | Renders a block element.
renderBlock :: P.Block -> Renderer ()
renderBlock (P.Plain i) =
  mapM_ (\l-> fixed $
              (pure . concat <$> mapM readInline l) >>= indented)
  (inlines i)
renderBlock (P.Para i) = indented =<< readInlines i
renderBlock (P.LineBlock i) =
  mapM_ (\l -> fixed $
               (pure . concat <$> mapM readInline l) >>= indented)
  i
renderBlock (P.CodeBlock attr s) = do
  storeAttr attr
  mapM_ (fixed . indented . pure)
    (map (pure . Fg Green . fromString) $ lines s)
renderBlock (P.RawBlock _ s) =
  indented $ map (pure . fromString) $ lines s
renderBlock (P.BlockQuote bs) = withIndent $ renderBlocks bs
renderBlock (P.OrderedList _ bs) = do
  zipWithM_ (\b n -> modify (\s -> s { listing = Just (Ordered n) })
                     >> fixed (keepIndent (renderBlocks b)))
    bs [1..]
  modify $ \s -> s { listing = Nothing }
renderBlock (P.BulletList bs) = do
  mapM_ (\b -> modify (\s -> s { listing = Just Bulleted })
               >> fixed (keepIndent (renderBlocks b)))
    bs
  modify $ \s -> s { listing = Nothing }
renderBlock (P.DefinitionList dl) =
  let renderDefinition (term, definition) = do
        indented =<< map (map (Fg Yellow)) <$> readInlines term
        withIndent $ mapM_ renderBlocks definition
  in mapM_ (fixed . renderDefinition) dl
renderBlock (P.Header level attr i) = do
  storeAttr attr
  indented =<< map (map (Denote (Heading level) . Bold . Fg Green)
                    . ([fromString (replicate level '#'), " "] ++))
    <$> readInlines i
renderBlock P.HorizontalRule = do
  st <- get
  indented [[Fg Black $
             fromString $ replicate (columns st - indentationLevel st * 2) '-']]
renderBlock (P.Table caption aligns widths headers rows) = do
  indented =<< readInlines caption
  -- Use pandoc-provided widths if they are set, calculate them
  -- otherwise.
  let widthsAreSet = case widths of
        [] -> False
        w -> minimum w /= maximum w
  ws <- if widthsAreSet then pure widths else do
    lens <- map sum . transpose <$> mapM
      (mapM (fmap (length . unstyled . concat . rLines) . renderCell 80)) rows
    pure $ map (\l -> if sum lens == 0
                      then 0
                      else fromIntegral l / fromIntegral (sum lens) * 0.7
                           + 1 / fromIntegral (length lens) * 0.3) lens
  let withHead = if all null headers then id else (headers :)
  mapM_
    (\r -> (fixed (renderBlock P.HorizontalRule)
            >> fixed (tableRow ws r)))
    (withHead rows)
  fixed $ renderBlock P.HorizontalRule
  where
    renderCell :: Int -> [P.Block] -> Renderer [RendererOutput]
    renderCell w blocks = do
      st <- get
      pure $ runRenderer w (linkCount st) (noteCount st) (lineNumber st)
        (rsConf st) $ renderBlocks blocks
    tableCell :: (P.Alignment, Int, [P.Block]) -> Renderer [StyledLine]
    tableCell (a, w, blocks) = do
      l <- renderCell w blocks
      mapM_ storeLink $ rLinks l
      modify (\s -> s { noteCount = noteCount s + length (rNotes l) })
      tell $ map (uncurry RIdentifier) $ rIdentifiers l
      pure $ map (padCell a w) $ rLines l
    padCell :: P.Alignment -> Int -> StyledLine -> StyledLine
    padCell a w x =
      let pLen = w - length (unstyled x)
          halfLen :: Rational
          halfLen = fromIntegral pLen / 2
          (lPad, rPad) = case a of
            P.AlignRight -> (pLen, 0)
            P.AlignCenter -> ( ceiling halfLen, floor halfLen )
            _ -> (0, pLen)
          mkPadding l = [fromString (replicate l ' ')]
      in concat [mkPadding lPad, x, mkPadding rPad]
    tableRow :: [Double] -> [[P.Block]] -> Renderer ()
    tableRow ws cols = do
      st <- get
      let maxWidth = columns st - indentationLevel st - ((length cols - 1) * 3)
          widths' = map (\w -> floor (fromIntegral maxWidth * w)) ws
      cells <- mapM tableCell $ zip3 aligns widths' cols
      let maxLines = foldr (max . length) 0 cells
          padded = zipWith (\w c -> c ++ replicate (maxLines - length c)
                             [fromString $ replicate w ' ']) widths' cells
      indented $ map (mconcat . intersperse (pure $ Fg Black " | "))
        $ transpose padded
renderBlock (P.Div attr b) = do
  storeAttr attr
  st <- get
  let i = if indentDivs $ rsConf st
          then withIndent
          else id
  i $ renderBlocks b
renderBlock P.Null = pure ()

-- | Checks whether a block is a list (ordered-, bullet-, or
-- definition-).
isList :: P.Block -> Bool
isList P.OrderedList {} = True
isList P.BulletList {} = True
isList P.DefinitionList {} = True
isList _ = False

-- | Determines whether a line should be skipped before a block.
skipBefore :: P.Block -> Bool
skipBefore P.Header {} = True
skipBefore P.Para {} = True
skipBefore (P.Div _ (b:_)) = skipBefore b
skipBefore _ = False

-- | Determines whether a line should be skipped after a block.
skipAfter :: P.Block -> Bool
skipAfter P.Header {} = True
skipAfter P.Para {} = True
skipAfter (P.Div _ bs@(_:_)) = skipAfter $ last bs
skipAfter b = isList b

-- | Stores an element position for fixed elements.
fixed :: Renderer a -> Renderer a
fixed r = do
  st <- get
  ret <- r
  st' <- get
  tell [RBlock (lineNumber st) (lineNumber st')]
  pure ret

-- | Renders block elements with empty lines between some of them.
renderBlocks :: [P.Block] -> Renderer ()
renderBlocks [] = pure ()
renderBlocks [b] = fixed $ renderBlock b
renderBlocks (b1:bs@(b2:_)) = do
  fixed $ renderBlock b1
  when (skipAfter b1 || skipBefore b2) $
    fixed $ storeLines [[]]
  renderBlocks bs

-- | Renders a document.
renderDoc :: Int
          -- ^ Number of columns.
          -> Config
          -- ^ Configuration.
          -> P.Pandoc
          -- ^ Document to render.
          -> [RendererOutput]
          -- ^ Rendered document.
renderDoc cols cnf (P.Pandoc _ blocks) =
  runRenderer cols 0 0 1 cnf $ renderBlocks blocks
