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

import Text.Pandoc.Readers.Plain
import Text.Pandoc.Readers.Gopher
import Pancake.Common


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
    t <- optionMaybe $ many1 alphaNum
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
          putErrLn $ "An error occured. Exit code: " ++ show ec
          case stderr of
            Nothing -> pure ()
            Just stderr' -> do
              err <- BS.hGetContents stderr'
              putErrLn $ "stderr:\n" ++ BS.unpack err
          else putErrLn $ show uri
        case parse pWithMeta (uriToString id uri "") out of
          Left _ -> pure $ Just (out, Nothing, Nothing)
          Right (bs, (u, t)) -> pure $ Just (bs, u, t)

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
        maybe (Left "no type suggestions") (byExtension . ('.':)) dt
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
                 ('/':'0':_) -> byExtension ext
                 -- unknown or unrecognized item type
                 _ -> byExtension ext <|> gopher
               (_, ext) -> byExtension ext
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
    byExtension ".plain" = pure plain
    byExtension ext = P.getReader $ tail ext
