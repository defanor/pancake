{- |
Module      :  Pancake.Printing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Renderer output printing facilities.
-}

module Pancake.Printing ( showLines
                        , putSexpLn
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
propertize t (Strikethrough s) =
  mconcat [termText "-", propertize t s, termText "-"]
propertize t (Subscript s) =
  mconcat [termText "_{", propertize t s, termText "}"]
propertize t (Superscript s) =
  mconcat [termText "^{", propertize t s, termText "}"]
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
              : map (\(i, l) -> list [encodeStr i, show l]) (rIdentifiers ro)
            , list $ "links"
              : map (\u -> encodeStr $ uriToString id u "") (rLinks ro)
            , list ["uri", ".", encodeStr $ uriToString id uri ""]]
  where
    encodeStr s = concat ["\"", concatMap escape s, "\""]
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape other = pure other
    showSexp :: Styled -> String
    showSexp (Plain s) = encodeStr s
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
    showDenotation (Link u) = list ["link", ".", encodeStr $ show u]
    showDenotation (Math m) = list ["math", ".", encodeStr m]
    showDenotation (Heading l) = list ["heading", ".", show l]

mergeStyled :: [Styled] -> [Styled]
mergeStyled = foldr mergeStyled' []
  where
    mergeStyled' :: Styled -> [Styled] -> [Styled]
    mergeStyled' s [] = [s]
    mergeStyled' s (s':rest) = maybe (s:s':rest) (: rest) (tryMerge s s')

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
