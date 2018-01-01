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
import Text.Pandoc.Class
import qualified Data.Text as T


-- | Translates a text line into a list of 'Inline' elements suitable
-- for further processing.
lineToInlines :: String -> [Inline]
lineToInlines [] = []
lineToInlines (' ':rest) = Space : lineToInlines rest
lineToInlines s = let (cur, next) = break (== ' ') s
                  in Str cur : lineToInlines next

-- | Reads plain text, always succeeding and producing a single
-- 'Plain' block.
readPlain :: PandocMonad m => T.Text -> m Pandoc
readPlain = pure . Pandoc mempty . pure . LineBlock
  . map (\l -> (lineToInlines $ T.unpack l))
  . T.lines . T.filter (/= '\r')
