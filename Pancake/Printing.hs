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
Module      :  Pancake.Printing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Renderer output printing facilities.
-}

module Pancake.Printing ( showLines
                        , putSexpLn
                        , encodeSexpStr
                        , showSexps
                        ) where

import Control.Monad.State
import System.IO
import System.Console.Terminfo
import Network.URI
import Data.Maybe

import Pancake.Rendering


-- | Propertizes a styled string for a given terminal.
propertize :: Terminal -> Styled -> TermOutput
propertize _ (Plain s) = termText s
propertize t (Fg clr s) = maybe id (\f -> f clr)
  (getCapability t withForegroundColor) $ propertize t s
propertize t (Bold s) =
  fromMaybe id (getCapability t withBold) $ propertize t s
propertize t (Emph s) =
  fromMaybe id (getCapability t withStandout) $ propertize t s
propertize t (Strikethrough s) = propertize t s
propertize t (Subscript s) = propertize t s
propertize t (Superscript s) = propertize t s
propertize t (Underline s) =
  fromMaybe id (getCapability t withUnderline) $ propertize t s
propertize t (Denote _ s) = propertize t s

-- | Prints rendered lines.
showLines :: MonadIO m => [StyledLine] -> m ()
showLines ls = liftIO $ do
  term <- setupTermFromEnv
  let nl = fromMaybe (termText "\n") $ getCapability term newline
  runTermOutput term . mconcat $
    map (\l -> mconcat (map (propertize term) l) <#> nl) ls

-- | Shows a list of strings as an s-expression
list :: [String] -> String
list l = "(" ++ unwords l ++ ")"

-- | Prints a list of strings as an s-expression.
putSexpLn :: MonadIO m => [String] -> m ()
putSexpLn s = liftIO $ do
  putStrLn $ list s
  hFlush stdout

-- | Encodes a string for use in s-expressions.
encodeSexpStr :: String -> String
encodeSexpStr s = concat ["\"", concatMap escape s, "\""]
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape other = pure other

-- | Prints rendered lines as s-expressions.
showSexps :: MonadIO m => URI -> [RendererOutput] -> m ()
showSexps uri ro =
  -- would be nicer to use some library for this, but they tend to be
  -- abandoned, and the task is simple enough to do it here
  putSexpLn [ "render"
            , list $ "lines" :
              map (list . pure . unwords . map showSexp . mergeStyled)
              (rLines ro)
            , list $ "identifiers"
              : map (\(i, l) -> list [encodeSexpStr i, show l]) (rIdentifiers ro)
            , list $ "links"
              : map (\u -> encodeSexpStr $ uriToString id u "") (rLinks ro)
            , list ["uri", ".", encodeSexpStr $ uriToString id uri ""]]
  where
    showSexp :: Styled -> String
    showSexp (Plain s) = encodeSexpStr s
    showSexp (Fg clr s) = list ["fg", show clr, showSexp s]
    showSexp (Bold s) = list ["style", "bold", showSexp s]
    showSexp (Underline s) = list ["style", "underline", showSexp s]
    showSexp (Emph s) = list ["style", "italic", showSexp s]
    showSexp (Strikethrough s) = list ["style", "strikethrough", showSexp s]
    showSexp (Subscript s) = list ["subscript", showSexp s]
    showSexp (Superscript s) = list ["superscript", showSexp s]
    showSexp (Denote d s) = list [ "denotation"
                                 , showDenotation d
                                 , showSexp s]
    showDenotation :: Denotation -> String
    showDenotation (Link u) = list ["link", ".", encodeSexpStr $ show u]
    showDenotation (Image u) = list ["image", ".", encodeSexpStr $ show u]
    showDenotation (Math m) = list ["math", ".", encodeSexpStr m]
    showDenotation (Heading l) = list ["heading", ".", show l]

-- | Merge elements with the same styling.
mergeStyled :: [Styled] -> [Styled]
mergeStyled = foldr mergeStyled' []
  where
    mergeStyled' :: Styled -> [Styled] -> [Styled]
    mergeStyled' s [] = [s]
    mergeStyled' s (s':rest) = maybe (s:s':rest) (: rest) (tryMerge s s')

-- | Merge two elements if their styles are the same. Used by
-- 'mergeStyled'.
tryMerge :: Styled -> Styled -> Maybe Styled
tryMerge (Plain s) (Plain s') = pure $ Plain $ s ++ s'
tryMerge (Fg clr s) (Fg clr' s')
  | clr == clr' = Fg clr <$> tryMerge s s'
  | otherwise = mzero
tryMerge (Bold s) (Bold s') = Bold <$> tryMerge s s'
tryMerge (Underline s) (Underline s') = Underline <$> tryMerge s s'
tryMerge (Emph s) (Emph s') = Emph <$> tryMerge s s'
tryMerge (Denote d s) (Denote d' s')
  | d == d' = Denote d <$> tryMerge s s'
  | otherwise = mzero
tryMerge _ _ = mzero
