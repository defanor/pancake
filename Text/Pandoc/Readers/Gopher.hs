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
import Text.Parsec
import Text.Parsec.Text
import Text.Pandoc.Readers.Plain
import Text.Pandoc.Class
import qualified Data.Text as T


-- | UNASCII   ::= ASCII - [Tab CR-LF NUL].
unascii :: Parser Char
unascii = noneOf ['\t', '\n', '\r', '\0']

-- | Creates a type prefix for directory entries.
mkPrefix :: String -> [Inline]
mkPrefix s = replicate (6 - length s) Space ++ [Str s, Space]

-- | An informational directory entry.
pInfo :: Parser [Inline]
pInfo = do
  _ <- char 'i'
  info <- manyTill unascii tab
  _ <- manyTill unascii tab
  _ <- manyTill unascii tab
  _ <- many1 digit
  pure $ mkPrefix "" ++ lineToInlines info ++ [LineBreak]

-- | A file\/link (i.e., any other than informational) directory
-- entry.
pLink :: Parser [Inline]
pLink = do
  t <- anyChar
  name <- manyTill unascii tab
  selector <- manyTill unascii tab
  host <- manyTill unascii tab
  port <- many1 digit
  let uri = concat ["gopher://", host, ":", port, "/", [t], selector]
      prefix = mkPrefix $ case t of
        '0' -> "(text)"
        '1' -> "(dir)"
        'h' -> "(html)"
        '9' -> "(bin)"
        'I' -> "(img)"
        's' -> "(snd)"
        '7' -> "(srch)"
        _   -> "(?)"
  pure [Link (name, [], []) (prefix ++ lineToInlines name) (uri, ""), LineBreak]

-- | Parses last line, with adjustments for what's used in the wild.
pLastLine :: Parser ()
-- Sometimes there's additional newline, sometimes there's no dot, and
-- sometimes LF is used instead of CRLF.
pLastLine = optional (optional endOfLine *> char '.' *> endOfLine) *> eof

-- | Parses end-of-line, skipping Gopher+ extensions if present.
pEOL :: Parser ()
pEOL = (endOfLine *> pure ())
  <|> (tab >> char '+' >> manyTill anyChar endOfLine *> pure ())

-- | Parses a directory.
pDirEntries :: Parser [Inline]
pDirEntries = concat <$> manyTill (choice [pInfo, pLink] <* pEOL) pLastLine

-- | Reads Gopher directory entries, falls back to plain text on
-- failure.
readGopher :: PandocMonad m => T.Text -> m Pandoc
readGopher s = pure . Pandoc mempty . pure . Plain $
  case parse pDirEntries "directory entry" s of
    -- fallback to plain text
    Left _ ->
      concatMap (\l -> (lineToInlines $ T.unpack l) ++ [LineBreak]) $ T.lines s
    Right r -> r
