{- |
Module      :  Text.Pandoc.Readers.Plain
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable
-}

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.Plain ( readPlain
                                 , lineToInlines
                                 ) where

import Text.Pandoc.Definition
import Text.Pandoc.Error


-- | Translates a text line into a list of 'Inline' elements suitable
-- for further processing.
lineToInlines :: String -> [Inline]
lineToInlines [] = []
lineToInlines (' ':rest) = Space : lineToInlines rest
lineToInlines s = let (cur, next) = break (== ' ') s
                  in Str cur : lineToInlines next

-- | Reads plain text, always succeeding and producing a single
-- 'Plain' block.
readPlain :: String -> Either PandocError Pandoc
readPlain = Right . Pandoc mempty . pure . Plain .
  concatMap (\l -> (lineToInlines l) ++ [LineBreak]) . lines
