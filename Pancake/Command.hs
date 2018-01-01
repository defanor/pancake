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
             | LoadConfig (Maybe FilePath)
             | SetWidth (Maybe Int)
             | SetPos (Maybe Int)
             | Redisplay
             deriving (Show, Eq)

-- | Parses a user command.
parseCommand :: Config -> String -> Command
parseCommand c = either (const Help) id . parse (command c) "user input"

-- | Basic (constant) command parser.
basicCommand :: Parser Command
basicCommand = choice . map (\(s, c) -> try (string s <* eof) *> pure c) $
  [ ("quit", Quit)
  , ("[", Back)
  , ("]", Forward)
  , (",", GoTo Nothing RCurrent)
  , ("help", Help)
  , ("?", ShowCurrent)
  , ("redisplay", Redisplay)
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
       <$> (string "save" *> space *> (RURI <$> pURI))
       <*> optionMaybe (space *> pFilePath)
       <* eof

-- | 'Save' command parser for 'RNumber'.
saveRef :: String -> Parser Command
saveRef digits = Save
                 <$> (string "save" *> space *> (RNumber <$> pNumber digits))
                 <*> optionMaybe (space *> pFilePath) <* eof

-- | 'Save' command parser for 'RCurrent'.
saveCurrent :: Parser Command
saveCurrent = Save RCurrent <$> (string "save" *> space *> char ','
                                 *> optionMaybe (space *> pFilePath) <* eof)

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

-- | A natural numbers parser.
pNat :: Read i => Parser i
pNat = read <$> many1 digit

-- | 'SetWidth' command parser.
setWidth :: Parser Command
setWidth = string "set width"
           *> (SetWidth <$> optionMaybe (space *> pNat))
           <* eof

-- | 'SetPos' command parser.
setPos :: Parser Command
setPos = string "set position"
         *> (SetPos <$> optionMaybe (space *> pNat))
         <* eof

-- | 'LoadConfig' command parser.
loadConf :: Parser Command
loadConf = string "load config"
           *> (LoadConfig <$> optionMaybe (space *> many1 anyChar))
           <* eof

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
          , setWidth <?> "set width"
          , setPos <?> "set position"
          , loadConf <?> "load config"
          , goTo <?> "follow uri"
          ])
