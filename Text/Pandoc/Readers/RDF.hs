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
Module      :  Text.Pandoc.Readers.RDF
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable

This module is for RDF documents reading. It's not strictly a parser,
since it requests additional external documents to read predicate
labels from, and looks up for alternate versions, and decides how to
render those.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Pandoc.Readers.RDF ( readRDF ) where

import Text.Pandoc.Definition
import Text.Pandoc.Class (PandocMonad)
-- import Data.RDF
import qualified Data.Text as T
import Network.URI (URI, parseURIReference, relativeFrom, uriToString)
import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.XML.HXT.Core ( readString, withParseHTML, withWarnings,
                           runX, (>>>), yes, no, XNode(..))
import Data.Tree.NTree.TypeDefs (NTree(..))
import Text.XML.HXT.XPath.Arrows (getXPathTreesInDoc)
import Control.Monad.Error.Class (throwError)
import Text.Pandoc.Error (PandocError(..))
import Control.Exception
import System.Directory

import Foreign
import Redland.MidLevel
import Redland.LowLevel
import Redland.Util


detectAlternateVersion :: T.Text -> IO (Maybe URI)
detectAlternateVersion t = do
  let doc = readString [withParseHTML yes, withWarnings no] (T.unpack t)
  rc <- liftIO $ runX $ doc
    >>> getXPathTreesInDoc
    "//link[@rel=\"alternate\" and @type=\"application/rdf+xml\"]/@href/text()"
  pure $ case rc of
    [NTree (XText uri) []] -> parseURIReference uri
    _ -> Nothing


readRDF :: (MonadIO m, PandocMonad m)
        => URI
        -- ^ Base (source) URI.
        -> (URI -> m T.Text)
        -- ^ Retrieval function.
        -> T.Text
        -- ^ Document to parse.
        -> m Pandoc
readRDF bu rf t = do
  alt <- liftIO $ detectAlternateVersion t
  case alt of
    Nothing -> do
      r <- liftIO $ handle handleRE parseDoc
      case r of
        Left e -> throwError $ PandocParseError e
        Right r' -> pure r'
    Just alt' -> do
      newDoc <- rf alt'
      readRDF alt' rf newDoc
  where
    handleRE :: RedlandException -> IO (Either String Pandoc)
    handleRE e = pure $ Left $ show e
    parseDoc :: IO (Either String Pandoc)
    parseDoc = Right . Pandoc mempty . pure . LineBlock <$> do
      withWSMU "memory" [] "temporary" "" (uriToString id bu "") $
        \world _ model uri -> do
          tryParseStringIntoModel world ["turtle", "rdfxml", "n-triples"]
            model uri (T.unpack t)
          withQuery world model "sparql" "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
            (Just uri) $ \results -> do
            cacheDir <- getXdgDirectory XdgCache "pancake"
            createDirectoryIfMissing True cacheDir
            withWSMU "hashes" [("hash-type", "bdb"), ("dir", cacheDir)]
              "rdf-cache" "" (uriToString id bu "") $ \world' _ model' uri' ->
              mapM (printTriple (world', model', uri')) results
    -- todo: should probably use statements instead, need to complete
    -- that part of the bindings.
    printTriple :: ( ForeignPtr RedlandWorld
                   , ForeignPtr RedlandModel
                   , ForeignPtr RedlandURI)
                -> [(String, Node)]
                -> IO [Inline]
    printTriple wmu l =
      concat . intersperse [Space] <$> mapM (printNode wmu . snd) l
    printNode :: ( ForeignPtr RedlandWorld
                 , ForeignPtr RedlandModel
                 , ForeignPtr RedlandURI)
              -> Node
              -> IO [Inline]
    printNode _ (BlankNode s) = pure [Str s]
    printNode _ (LiteralNode s) = pure [Str s]
    printNode (w, m, u) (ResourceNode s) =
      let su = showURI s
          q = "SELECT ?label WHERE { <" ++ s ++
            "> <http://www.w3.org/2000/01/rdf-schema#label> ?label }"
      in do
        l <- withQuery w m "sparql" q (Just u) $ \r ->
          case r of
            ([("label", LiteralNode label)]:_) -> pure label
            _ -> pure su
        pure [Link (su, [], []) [] (su, l)]
    showURI :: String -> String
    showURI u = case parseURIReference u of
                  Just u' -> uriToString id (relativeFrom u' bu) ""
                  Nothing -> u

-- rdfproc rdf-cache parse http://xmlns.com/foaf/0.1/
-- rdfproc rdf-cache parse http://www.w3.org/1999/02/22-rdf-syntax-ns
