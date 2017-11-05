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
import Data.List
import Pancake.Rendering


-- | Propertizes a styled string for a given terminal.
propertize :: Terminal -> Styled -> TermOutput
propertize _ (Plain s) = termText s
propertize t (Fg clr s) = maybe id (\f -> f clr)
  (getCapability t withForegroundColor) $ propertize t s
propertize t (Pancake.Rendering.Bold s) =
  maybe id id (getCapability t withBold) $ propertize t s
propertize t (Pancake.Rendering.Emph s) =
  maybe id id (getCapability t withStandout) $ propertize t s
propertize t (Pancake.Rendering.Underline s) =
  maybe id id (getCapability t withUnderline) $ propertize t s

-- | Prints rendered lines.
showLines :: MonadIO m => [StyledLine] -> m ()
showLines ls = liftIO $ do
  term <- setupTermFromEnv
  let nl = maybe (termText "\n") id $ getCapability term newline
  runTermOutput term . mconcat $
    map (\l -> mconcat (map (propertize term) l) <#> nl) ls

-- | Shows a list of strings as an s-expression
list :: [String] -> String
list l = "(" ++ intercalate " " l ++ ")"

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
              map (list . pure . concat . intersperse " " . map showSexp)
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
