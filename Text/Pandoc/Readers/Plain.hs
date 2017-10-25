{- |
Module      :  Text.Pandoc.Readers.Plain
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable
-}

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.Plain ( readPlain ) where

import Text.Pandoc.Definition
import Text.Pandoc.Error
import Data.List

-- | Reads plain text, always succeeding and producing a single
-- 'Plain' block.
readPlain :: String -> Either PandocError Pandoc
readPlain = Right . Pandoc mempty . pure . Plain .
  concatMap (\l -> (intersperse Space $ map Str $ words l) ++ [LineBreak]) . lines
  -- or Right . Pandoc mempty . pure . RawBlock "plain"
