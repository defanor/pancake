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
labels from, and generally controls how those would be rendered.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Pandoc.Readers.RDF ( readRDF ) where

import Text.Pandoc.Definition
import Text.Pandoc.Class (PandocMonad)
import Data.RDF
import qualified Data.Text as T
import Network.URI (URI, uriFragment, parseURIReference, relativeFrom,
                    uriToString)
import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.XML.HXT.Core ( readString, withParseHTML, withWarnings,
                           runX, (>>>), yes, no, XNode(..))
import Data.Tree.NTree.TypeDefs (NTree(..))
import Text.XML.HXT.XPath.Arrows (getXPathTreesInDoc)
import Control.Monad.Error.Class (throwError, catchError)
import Text.Pandoc.Error (PandocError(..))
import Control.Concurrent.STM.TVar (TVar, readTVarIO, modifyTVar)
import Control.Monad.STM (atomically)
import qualified Data.Map as M


-- | Reads a literal value.
readLiteral :: LValue -> [Inline]
readLiteral (PlainL t) = pure . Str $ T.unpack t
readLiteral (PlainLL t _) = pure . Str $ T.unpack t
readLiteral (TypedL t _) = pure . Str $ T.unpack t

-- | Reads an arbitrary 'Node', used mostly for objects and subjects.
readNode :: URI -> Node -> [Inline]
readNode _ (LNode l) = readLiteral l
readNode bu (UNode u) = case parseURIReference (T.unpack u) of
  Just u' -> let u'' = relativeFrom u' bu
                 us = uriToString id u'' ""
             in [Link (us, [], []) [] (us, us)]
  Nothing -> let s = (T.unpack u) in [Link (s, [], []) [] (s, s)]
readNode _ (BNode t) = let s = T.unpack t
                       in [Link (s, [], []) [] (s, s)]
readNode _ (BNodeGen i) = let s = show i
                          in [Link (s, [], []) [] (s, s)]

-- | Reads a predicate, looks up its label in external documents using
-- the provided retrieval function.
readPredicate :: (MonadIO m, PandocMonad m)
              => TVar (M.Map String String)
              -- ^ RDF cache
              -> URI
              -- ^ Base URI.
              -> (URI -> m T.Text)
              -- ^ Retrieval function.
              -> Node
              -- ^ A node to read.
              -> m [Inline]
readPredicate rdfc _ rf (UNode u) = do
  rdfc' <- liftIO $ readTVarIO rdfc
  l <- case M.lookup uriStr rdfc' of
    Just cl -> pure cl
    Nothing -> do
      (u', doc) <- case parseURIReference uriStr of
        Just r -> do
          ret <- rf r
          pure (r, ret)
        Nothing -> throwError $ PandocParseError $
                   "Failed to parse an URI reference"
      -- use URIs when failing to read a label
      catchError (labelFromRDF u' doc) $ const $ pure uriStr
  pure [Link (uriStr, [], []) [] (uriStr, l)]
  where
    uriStr = T.unpack u
    labelFromRDF u' doc = do
      rdf <- parseRDF u' rf doc
      let label = (UNode "http://www.w3.org/2000/01/rdf-schema#label")
      -- 'query' doesn't expand triples, so filtering manually
      l <- case filter
                (\x -> subjectOf x == (UNode u) && predicateOf x == label)
                (expandTriples rdf) of
             -- todo: there could be multiple labels in different
             -- languages, handle that.
             [sl] -> case objectOf sl of
               (LNode (PlainL sl')) -> pure $ T.unpack sl'
               _ -> pure $ show sl
             _ -> pure uriStr
      liftIO $ atomically $ modifyTVar rdfc $ M.insert uriStr l
      pure l
readPredicate _ bu _ n = pure $ readNode bu n

-- | Reads a triple.
readTriple :: (MonadIO m, PandocMonad m)
           => TVar (M.Map String String)
           -- ^ RDF cache
           -> URI
           -- ^ Base (source) URI.
           -> (URI -> m T.Text)
           -- ^ Retrieval function.
           -> Triple
           -- ^ A triple to read.
           -> m [Inline]
readTriple rdfc bu rf t = do
  p <- readPredicate rdfc bu rf $ predicateOf t
  pure $ intersperse Space $
    concat [readNode bu $ subjectOf t, p, readNode bu $ objectOf t]

-- | Parses an RDF (XML/RDF or Turtle). The provided document may also
-- be an HTML document with an alternate version that is RDF;
-- retrieves it with the provided retrieval function in such a case.
parseRDF :: (MonadIO m, PandocMonad m)
         => URI
         -- ^ Base (source) URI.
         -> (URI -> m T.Text)
         -- ^ Retrieval function.
         -> T.Text
         -- ^ Document to parse.
         -> m (RDF AdjHashMap)
parseRDF bu rf t = do
  let baseURI = uriToString id bu { uriFragment = "" } ""
  -- check link rel
  let doc = readString [withParseHTML yes, withWarnings no] (T.unpack t)
  rc <- liftIO $ runX $ doc
    >>> getXPathTreesInDoc
    "//link[@rel=\"alternate\" and @type=\"application/rdf+xml\"]/@href/text()"
  case rc of
    [NTree (XText uri) []] -> case parseURIReference uri of
      Nothing -> throwError $ PandocParseError $
        "Failed to parse an alternate URI"
      Just u' ->
        if u' /= bu
        then do
          t' <- rf u'
          parseRDF u' rf t'
        else throwError $ PandocSomeError $
             "A loop is detected in alternate document versions."
    _ -> do
      let burl = T.pack baseURI
          tryParse :: (Rdf a, RdfParser p) => p -> Either ParseFailure (RDF a)
          tryParse p = parseString p t
          parsed :: Either ParseFailure (RDF AdjHashMap)
          parsed = -- todo: alternatives should be used here
            either
            (const $ either (const $ tryParse NTriplesParser) pure
              (tryParse $ TurtleParser (Just (BaseUrl burl)) (Just burl)))
            pure
            (tryParse $ XmlParser (Just (BaseUrl burl)) (Just burl))
      case parsed of
        Left err -> throwError $ PandocParseError $ show err
        Right x -> pure x

readRDF :: (MonadIO m, PandocMonad m)
        => TVar (M.Map String String)
        -- ^ RDF cache
        -> URI
        -- ^ Base (source) URI.
        -> (URI -> m T.Text)
        -- ^ Retrieval function.
        -> T.Text
        -- ^ Document to parse.
        -> m Pandoc
readRDF rdfc bu rf t = do
  rdf <- parseRDF bu rf t
  Pandoc mempty . pure . LineBlock
    <$> mapM (readTriple rdfc bu rf) (expandTriples rdf)
