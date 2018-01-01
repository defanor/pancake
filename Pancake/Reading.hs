{-
Copyright (C) 2017-2018 defanor <defanor@uberspace.net>

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
Module      :  Pancake.Reading
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

Document retrieval and parsing.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Pancake.Reading ( retrieve
                       , readDoc
                       ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Network.URI
import qualified Text.Pandoc as P
import System.Process
import Control.Exception (handle, SomeException)
import Control.Applicative ((<|>))
import Data.Text.Encoding (decodeUtf8', decodeLatin1)
import Data.Default
import System.Console.Terminfo (setupTermFromEnv, getCapability, termColumns)
import System.FilePath
import Data.Char
import System.Exit
import System.Environment
import GHC.IO.Handle
import Text.Parsec hiding ((<|>))
import Text.Parsec.ByteString
import Data.Maybe
import Data.Version

import Text.Pandoc.Readers.Plain
import Text.Pandoc.Readers.Gopher
import Pancake.Common
import Paths_pancake


-- | Metadata (header, URI, document type) parser.
pMeta :: Parser (Maybe URI, Maybe String)
pMeta = do
  _ <- newline
  _ <- string "-pancake-"
  _ <- newline
  u <- optionMaybe $ do
    _ <- string "uri: "
    u <- manyTill anyToken newline
    maybe (fail "Failed to parse URI") pure $ parseURI u
  t <- option Nothing $ do
    _ <- string "type: "
    optional $ try $ manyTill alphaNum (char '/')
    t <- optionMaybe $ many1 $ choice [alphaNum, char '-']
    _ <- manyTill anyToken newline
    pure t
  eof
  pure (u, t)

-- | Document body + metadata parser.
pWithMeta :: Parser (BS.ByteString, (Maybe URI, Maybe String))
pWithMeta = (,) . BS.pack <$> manyTill anyToken (try $ lookAhead pMeta)
            <*> pMeta

-- | Retrieves a document. Prints an error message and returns an
-- empty string on failure.
retrieve :: String
         -- ^ Shell command to use for retrieval.
         -> URI
         -- ^ Document URI.
         -> IO (Maybe (BS.ByteString, Maybe URI, Maybe String))
         -- ^ File contents, effective URI, type.
retrieve cmd uri = do
  putErrLn $ "Retrieving " ++ show uri
  curEnv <- getEnvironment
  let envAuthority = maybe [] (\x -> [ ("URI_USERINFO", uriUserInfo x)
                                     , ("URI_REGNAME", uriRegName x)
                                     , ("URI_PORT", uriPort x) ])
        (uriAuthority uri)
      uriStr = uriToString id uri ""
      environment = ("URI", uriStr)
                    : ("URI_ESCAPED", escapeURIString isUnreserved uriStr)
                    : ("URI_SCHEME", uriScheme uri)
                    : ("URI_PATH", uriPath uri)
                    : ("URI_QUERY", uriQuery uri)
                    : ("URI_FRAGMENT", uriFragment uri)
                    : ("PANCAKE", showVersion version)
                    : curEnv
                    ++ envAuthority
  handle (\(e :: SomeException) ->
            putErrLn (concat ["Failed to run `", cmd, "`: ", show e])
            >> pure Nothing) $
    withCreateProcess ((shell cmd) { env = Just environment
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe
                                   , delegate_ctlc = True }) $
    \_ stdout stderr ph -> case stdout of
      Nothing -> putErrLn "No stdout" >> pure Nothing
      Just stdout' -> do
        hSetBinaryMode stdout' True
        out <- BS.hGetContents stdout'
        ec <- waitForProcess ph
        if ec /= ExitSuccess
          then do
          putErrLn $ "An error occurred. Exit code: " ++ show ec
          case stderr of
            Nothing -> pure ()
            Just stderr' -> do
              err <- BS.hGetContents stderr'
              putErrLn $ "stderr:\n" ++ BS.unpack err
          else putErrLn $ show uri
        case parse pWithMeta (uriToString id uri "") out of
          Left _ -> pure $ Just (out, Nothing, Nothing)
          Right (bs, (u, t)) -> pure $ Just (bs, u, t)

-- | An Emacs file variable parser. Extracts a mode if it's set.
pEmacsMode :: Parser String
pEmacsMode = do
  _ <- manyTill (noneOf "\r\n") (string "-*-")
  spaces
  vs <- try fileVariable `sepEndBy` (char ';' >> spaces)
  spaces
  _ <- string "-*-"
  maybe (fail "no mode variable found") pure (lookup "mode" vs)
  where
    fileVariable :: Parser (String, String)
    fileVariable = do
      -- this is restrictive, but should suffice for idiomatic names
      name <- many1 (choice [alphaNum, char '-'])
      char ':' >> spaces
      val <- reverse . dropWhile isSpace . reverse <$> manyTill anyChar
        (choice $ map (try . lookAhead . string) [";", "\n", "-*-"])
      pure (name, val)

-- | Parses a document into a Pandoc structure. The parser is chosen
-- depending on the document type (if one is provided) or its URI.
readDoc :: BS.ByteString
        -- ^ Raw document data.
        -> Maybe String
        -- ^ Document type.
        -> URI
        -- ^ Document URI.
        -> IO (Either P.PandocError P.Pandoc)
        -- ^ A parsed document.
readDoc out dt uri = do
  term <- setupTermFromEnv
  let (reader, exts) = either (const plain) id $
        maybe (Left "no type suggestions") byExtension dt
        <|> case (uriScheme uri, map toLower $ takeExtension $ uriPath uri) of
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
                ('/':'0':_) -> byExtension' ext
                -- unknown or unrecognized item type
                _ -> byExtension' ext <|> gopher
              (_, ext) -> byExtension' ext
        <|> either (Left . show) byExtension
        (parse pEmacsMode (uriToString id uri "") out)
      cols = fromMaybe 80 $ getCapability term termColumns
      opts = def { P.readerColumns = cols, P.readerExtensions = exts }
  case reader of
    P.TextReader f -> case decodeUtf8' out of
      Left err -> do
        putErrLn $ show err
        P.runIO $ f opts $ decodeLatin1 out
      Right r -> P.runIO $ f opts r
    P.ByteStringReader f -> P.runIO $ f opts $ BL.fromStrict out
  where
    http ext = byExtension' ext <|> html
    html = P.getReader "html"
    plain = (P.TextReader . const $ readPlain, P.emptyExtensions)
    gopher = pure (P.TextReader . const $ readGopher, P.emptyExtensions)
    byExtension' ext = byExtension $ dropWhile (== '.') ext
    byExtension "md" = P.getReader "markdown"
    byExtension "htm" = html
    byExtension "ltx" = P.getReader "latex"
    byExtension "tex" = P.getReader "latex"
    byExtension "txt" = pure plain
    byExtension "plain" = pure plain
    byExtension ext = P.getReader ext
