{- |
Module      :  Pancake.Command
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

User command parsing.
-}

module Pancake.Command ( Command(..)
                       , parseCommand
                       ) where

import Network.URI
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as M
import Numeric
import Data.List
import Data.Maybe

import Pancake.Configuration


-- | Interactive user command.
data Command = Quit
             | Interrupt
             | Follow Int
             | More
             | GoTo (Maybe String) URI
             -- ^ Document type, URI
             | Reload
             | Back
             | Forward
             | Help
             | Show Int
             | ShowCurrent
             | Shortcut String String
             | ReloadConfig
             deriving (Show, Eq)

-- | Parses a user command.
parseCommand :: Config -> String -> Command
parseCommand c = either (const Help) id . parse (command c) "user input"

-- | Basic (constant) command parser.
basicCommand :: Parser Command
basicCommand = choice . map (\(s, c) -> try (string s <* eof) *> pure c) $
  [ ("q", Quit)
  , ("b", Back)
  , ("f", Forward)
  , ("r", Reload)
  , ("re", ReloadConfig)
  , ("h", Help)
  , ("?", ShowCurrent)
  , ("", More)]

pReference :: String -> Parser Int
pReference digits = do
  ds <- many1 (choice $ map char digits)
  pure . fst . head $ readInt (length digits)
    (`elem` digits) (fromJust . flip elemIndex digits) ds

-- | 'Follow' command parser.
followRef :: String -> Parser Command
followRef digits = Follow <$> (optional (char '.') *> pReference digits <* eof)

-- | 'Show' command parser.
showRef :: String -> Parser Command
showRef digits = Show <$> (char '?' *> pReference digits <* eof)

-- | 'GoTo' command parser.
goTo :: Parser Command
goTo = do
  f <- optionMaybe (try (many1 alphaNum <* space)) <?> "type"
  s <- manyTill anyChar eof <?> "URI"
  maybe (fail "Failed to parse URI") (pure . GoTo f) $ parseURIReference s

-- | 'Shortcut' command parser.
shortcut :: M.Map String String -> Parser Command
shortcut m = do
  s <- choice $ map (\k -> try (string k <* space)) $ M.keys m
  case M.lookup s m of
    Nothing -> fail $ "No such shortcut: " ++ s
    Just u -> do
      q <- manyTill anyChar eof
      pure $ Shortcut u q

-- | Command parser.
command :: Config -> Parser Command
command c =
  choice (map try
          [ basicCommand <?> "basic command"
          , followRef (referenceDigits c) <?> "follow ref"
          , showRef (referenceDigits c) <?> "show ref"
          , shortcut (shortcuts c) <?> "shortcut"
          , goTo <?> "go to"
          ])
