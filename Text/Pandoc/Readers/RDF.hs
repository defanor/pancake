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

import Redland

-- | Detects an alternate "rdf+xml" version of a document.
detectAlternateVersion :: T.Text -> IO (Maybe URI)
detectAlternateVersion t = do
  let doc = readString [withParseHTML yes, withWarnings no] (T.unpack t)
  rc <- liftIO $ runX $ doc
    >>> getXPathTreesInDoc
    "//link[@rel=\"alternate\" and @type=\"application/rdf+xml\"]/@href/text()"
  pure $ case rc of
    [NTree (XText uri) []] -> parseURIReference uri
    _ -> Nothing

-- | Shows an URI, relative to a base URI.
showURI :: URI
        -- ^ base URI
        -> String
        -- ^ URI to show
        -> String
showURI bu u = case parseURIReference u of
  Just u' -> uriToString id (relativeFrom u' bu) ""
  Nothing -> u

-- | Reads a node.
readNode :: URI
          -- ^ base URI
          -> ( ForeignPtr RedlandWorld, ForeignPtr RedlandModel)
          -> Maybe Node
          -> IO [Inline]
readNode _ _ Nothing = pure [Str "-"]
readNode _ _ (Just (BlankNode s)) = pure [Str s]
readNode _ _ (Just (LiteralNode s)) = pure [Str s]
readNode bu (w, m) n@(Just (ResourceNode s)) = do
  let su = showURI bu s
      labelURI = "http://www.w3.org/2000/01/rdf-schema#label"
  l <- withStatements w m
    (Triple n (Just (ResourceNode labelURI)) Nothing) $ \r ->
    case r of
      (Triple _ _ (Just (LiteralNode label)):_) -> pure label
      _ -> pure su
  pure [Link (su, [], []) [] (su, l)]

-- | Prepares triples for conversion into Pandoc: excludes repeating
-- subjects and predicates.
prepareTriples :: [Triple] -> [Triple]
prepareTriples = prepareTriples' (Triple Nothing Nothing Nothing)
  where
    prepareTriples' :: Triple -> [Triple] -> [Triple]
    prepareTriples' _ [] = []
    prepareTriples' prev (cur:ts) = cur { subject = changedOnly subject
                                        , predicate = changedOnly predicate }
                                    : prepareTriples' cur ts
      where changedOnly :: (Triple -> Maybe Node) -> Maybe Node
            changedOnly f | f prev == f cur = Nothing
                          | otherwise = f cur

-- | Reads an RDF document.
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
          guessingParseStringIntoModel world model uri (T.unpack t)
          withStatements world model (Triple Nothing Nothing Nothing) $
            \triples -> do
              cacheDir <- getXdgDirectory XdgCache "pancake"
              createDirectoryIfMissing True cacheDir
              withWSMU "hashes" [("hash-type", "bdb"), ("dir", cacheDir)]
                "rdf-cache" "" (uriToString id bu "") $ \world' _ model' _ ->
                mapM (readTriple (world', model')) $ prepareTriples triples
    readTriple :: ( ForeignPtr RedlandWorld
                   , ForeignPtr RedlandModel)
                -> Triple
                -> IO [Inline]
    readTriple wm triple =
      concat . intersperse [Space] <$>
      mapM (readNode bu wm) [subject triple, predicate triple, object triple]

-- rdfproc rdf-cache parse http://xmlns.com/foaf/0.1/
-- rdfproc rdf-cache parse http://www.w3.org/1999/02/22-rdf-syntax-ns
