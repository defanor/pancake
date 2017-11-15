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
import Control.Monad.Except (throwError)
import Text.Pandoc.Error


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
        '3' -> "(err)"
        'h' -> "(html)"
        '9' -> "(bin)"
        'g' -> "(gif)"
        'I' -> "(img)"
        's' -> "(snd)"
        '7' -> "(srch)"
        _   -> "(?)"
      line = case t of
        '3' -> prefix ++ lineToInlines name
        _ -> [Link (name, [], []) (prefix ++ lineToInlines name) (uri, "")]
  pure $ line ++ [LineBreak]

-- | An erroneous directory entry. Still parsing it, since there is a
-- lot of broken directories out there -- but marking as an error.
pError :: Parser [Inline]
pError = do
  line <- manyTill anyChar (lookAhead $ try pEOL)
  pure $ [Strong $ mkPrefix "error"] ++ lineToInlines line ++ [LineBreak]

-- | Parses last line, with adjustments for what's used in the wild.
pLastLine :: Parser ()
-- Sometimes there's additional newline, sometimes there's no dot or
-- multiple dots, and sometimes LF is used instead of CRLF.
pLastLine = optional (try $ optional endOfLine *> char '.' *> endOfLine) *> eof

-- | Parses end-of-line, skipping Gopher+ extensions if present.
pEOL :: Parser ()
pEOL = (endOfLine *> pure ())
  <|> (tab >> char '+' >> manyTill anyChar endOfLine *> pure ())

-- | Parses a directory.
pDirEntries :: Parser [Inline]
pDirEntries = concat <$>
  manyTill (choice ([ try pInfo <?> "info entry"
                    , try pLink <?> "link entry"
                    , pError <?> "erroneous entry"])
             <* pEOL)
  pLastLine

-- | Reads Gopher directory entries.
readGopher :: PandocMonad m => T.Text -> m Pandoc
readGopher s =
  case parse pDirEntries "directory entry" s of
    Left err -> throwError $ PandocParseError $ show err
    Right r -> pure . Pandoc mempty . pure $ Plain r
