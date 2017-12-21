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
Module      :  Pancake.Unclutter
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

An XSLT-based data extraction module.
-}

{-# LANGUAGE TupleSections #-}

module Pancake.Unclutter ( tryUnclutter
                         , prepareUnclutter
                         ) where

import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import qualified Data.Map as M
import Network.URI
import Data.Either
import System.Directory
import System.FilePath
import Data.List
import Text.Regex.TDFA.String
import Text.Regex.Base.RegexLike
import Text.XML.HXT.Core ( writeDocumentToString, readString
                         , withParseHTML, withWarnings, runX, (>>>), yes, no)
import Text.XML.HXT.XSLT (xsltApplyStylesheetFromURI)
import Data.Text.Encoding (decodeUtf8', decodeLatin1, encodeUtf8)
import qualified Data.Text as T
import Control.Exception


import Pancake.Common
import Pancake.Configuration

-- | Tries to unclutter a document by applying an XSLT if it's
-- available.
tryUnclutter :: MonadIO m
             => [(Regex, String)]
             -- ^ Obtained with 'prepareUnclutter'.
             -> URI
             -- ^ Document URI.
             -> BS.ByteString
             -- ^ Raw document.
             -> m BS.ByteString
tryUnclutter rs u d = liftIO $ handle err $ do
  let matches (r, _) = case execute r uStr of
        Right (Just _) -> True
        _ -> False
  case find matches rs of
    Just (_, fn) -> do
      dir <- getXdgDirectory XdgConfig "pancake"
      let src = dir </> "unclutter" </> fn <.> "xsl"
      exists <- doesFileExist src
      if exists
        then do
        let txt = T.unpack $ either (const $ decodeLatin1 d) id $ decodeUtf8' d
            doc = readString [withParseHTML yes, withWarnings no] txt
        rc <- runX $ doc
              >>> xsltApplyStylesheetFromURI src
              >>> writeDocumentToString []
        pure $ case rc of
          [str] -> encodeUtf8 $ T.pack str
          _ -> d
        else pure d
    Nothing -> pure d
  where
    uStr = uriToString id u ""
    err :: SomeException -> IO BS.ByteString
    err e = putErrLn (show e) >> pure d

-- | Compiles regexps for uncluttering.
prepareUnclutter :: MonadIO m => Config -> m [(Regex, String)]
prepareUnclutter c = do
  let re = map
           (\(f, r) -> fmap (, f) (compile defaultCompOpt defaultExecOpt r))
           $ M.toList $ unclutter c
      errs = lefts re
      compiled = rights re
  mapM_ putErrLn errs
  pure compiled
