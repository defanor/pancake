{- |
Module      :  Text.Pandoc.Readers.Gopher
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Loosely based on <https://www.ietf.org/rfc/rfc1436.txt RFC 1436>, but
since the commonly found in the wild directories tend to differ from
that, there are some adjustments.
-}

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.Gopher ( readGopher ) where

import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Parsec
import Text.Parsec.String

-- | Translates a text line into a list of 'Inline' elements suitable
-- for further processing.
lineToInlines :: String -> [Inline]
lineToInlines [] = []
lineToInlines (' ':rest) = Space : lineToInlines rest
lineToInlines s = let (cur, next) = break (== ' ') s
                  in Str cur : lineToInlines next

-- | UNASCII   ::= ASCII - [Tab CR-LF NUL].
unascii :: Parser Char
unascii = noneOf ['\t', '\n', '\r', '\0']

-- | An informational directory entry.
pInfo :: Parser [Inline]
pInfo = do
  _ <- char 'i'
  info <- manyTill unascii tab
  _ <- manyTill unascii tab
  _ <- manyTill unascii tab
  _ <- many1 digit
  pure $ lineToInlines info ++ [LineBreak]

-- | A file\/link (i.e., any other than informational) directory
-- entry.
pLink :: Parser [Inline]
pLink = do
  t <- alphaNum
  name <- manyTill unascii tab
  selector <- manyTill unascii tab
  host <- manyTill unascii tab
  port <- many1 digit
  let uri = concat ["gopher://", host, ":", port, "/", [t], selector]
  pure [Link (name, [], []) (lineToInlines name) (uri, ""), LineBreak]

-- | Parses last line, with adjustments for what's used in the wild.
pLastLine :: Parser ()
-- Sometimes there's additional newline, sometimes there's no dot, and
-- sometimes LF is used instead of CRLF.
pLastLine = optional (optional endOfLine *> char '.' *> endOfLine) *> eof

pDirEntries :: Parser [Inline]
pDirEntries = concat <$> manyTill (choice [pInfo, pLink] <* endOfLine) pLastLine

-- | Reads Gopher directory entries, falls back to plain text on
-- failure.
readGopher :: String -> Either PandocError Pandoc
readGopher s = Right . Pandoc mempty . pure . Plain $
  case parse pDirEntries "directory entry" s of
    -- fallback to plain text
    Left _ -> concatMap (\l -> (lineToInlines l) ++ [LineBreak]) $ lines s
    Right r -> r
