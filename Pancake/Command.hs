{- |
Module      :  Pancake.Command
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

User command parsing.
-}

module Pancake.Command ( Command(..)
                       , Reference(..)
                       , parseCommand
                       ) where

import Network.URI
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as M
import Numeric
import Data.List
import Data.Maybe
import System.FilePath

import Pancake.Configuration

-- | The ways for a user to point to a document.
data Reference = RURI URI
               | RNumber Int
               | RCurrent
               deriving (Show, Eq)

-- | Interactive user command.
data Command = Quit
             | Interrupt
             | More
             | GoTo (Maybe String) Reference
             -- ^ Document type, reference
             | Save Reference (Maybe FilePath)
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
  , (",", GoTo Nothing RCurrent)
  , ("reload config", ReloadConfig)
  , ("help", Help)
  , ("?", ShowCurrent)
  , ("", More)]

-- | Link number parser.
pNumber :: String -> Parser Int
pNumber digits = do
  optional (char ',')
  ds <- many1 (choice $ map char digits)
  pure . fst . head $ readInt (length digits)
    (`elem` digits) (fromJust . flip elemIndex digits) ds

-- | 'GoTo' command parser for 'RNumber'.
followRef :: String -> Parser Command
followRef digits = GoTo Nothing . RNumber <$> (pNumber digits <* eof)

-- | 'Show' command parser.
showRef :: String -> Parser Command
showRef digits = Show <$> (char '?' *> pNumber digits <* eof)

-- | 'URI' parser.
pURI :: Parser URI
pURI = do
  s <- many1 (satisfy isAllowedInURI) <?> "URI"
  maybe (fail "Failed to parse URI") pure $ parseURIReference s

-- | 'FilePath' parser.
pFilePath :: Parser FilePath
pFilePath = do
  p <- many1 anyChar
  if isValid p then pure p else fail ("Invalid file path: " ++ p)

-- | 'Save' command parser for 'RURI'.
save :: Parser Command
save = Save
       <$> (string "save" *> spaces *> (RURI <$> pURI))
       <*> (spaces *> optionMaybe pFilePath)
       <* eof

-- | 'Save' command parser for 'RNumber'.
saveRef :: String -> Parser Command
saveRef digits = Save
                 <$> (string "save" *> spaces *> (RNumber <$> pNumber digits))
                 <*> (spaces *> optionMaybe pFilePath) <* eof

-- | 'Save' command parser for 'RCurrent'.
saveCurrent :: Parser Command
saveCurrent = Save RCurrent <$> (string "save" *> spaces *> char ','
                                 *> spaces *> optionMaybe pFilePath <* eof)

-- | 'GoTo' command parser for 'RURI'.
goTo :: Parser Command
goTo = GoTo
       <$> (optionMaybe (try (many1 alphaNum <* space)) <?> "type")
       <*> (RURI <$> pURI)
       <* eof

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
          , saveRef (referenceDigits c) <?> "save ref"
          , saveCurrent <?> "save current"
          , save <?> "save"
          , goTo <?> "follow uri"
          ])
