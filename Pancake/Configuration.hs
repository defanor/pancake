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
Module      :  Pancake.Configuration
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

Pancake configuration facilities.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Pancake.Configuration ( Config(..)
                             , loadConfig
                             ) where

import Data.Yaml
import Data.Default
import Control.Monad.State
import System.Directory
import System.FilePath
import qualified Data.Map as M
import GHC.Generics

import Pancake.Common


-- | Application configuration.
data Config = Config { commands :: M.Map String String
                     -- ^ URI schemes and corresponding shell commands
                     -- for downloading.
                     , defaultCommand :: String
                     -- ^ A command to use if no other command
                     -- applies.
                     , externalViewers :: M.Map String String
                     -- ^ File extensions and corresponding external
                     -- applications.
                     , shortcuts :: M.Map String String
                     -- ^ Shortcuts to use (search engines,
                     -- dictionaries, etc).
                     , paginate :: Bool
                     -- ^ Enable pagination in non-embedded mode;
                     -- print everything at once otherwise.
                     , historyDepth :: Int
                     -- ^ The amount of history entries (into either
                     -- direction) to keep.
                     , referenceDigits :: String
                     -- ^ Digits to use for reference numbering, must
                     -- be unique.
                     , indentDivs :: Bool
                     -- ^ Whether to add indentation for elements
                     -- inside divs.
                     , unclutter :: M.Map String String
                     -- ^ XSLT file and URI regex.
                     , pandocTimeout :: Int
                     -- ^ Maximum amount of time (in seconds) allowed
                     -- for document parsing.
                     } deriving (Generic, Show, Eq)

-- | For configuration parsing.
instance FromJSON Config
-- | For configuration writing, particularly that of default
-- configuration if it is missing.
instance ToJSON Config
-- | The default configuration to use if user configuration is
-- missing.
instance Default Config where
  def = Config {
    commands = M.fromList
      [ ("ssh", "scp \"${URI_REGNAME}:${URI_PATH}\" /dev/stdout"
                ++ " && echo -e '\n-pancake-'")
      , ("gopher", "curl \"${URI}\""
          ++ " -w \"\n-pancake-\n\"")
      , ("web-archive", concat
          [ curl
          , "\"$(curl \"https://archive.org/wayback/available${URI_QUERY}"
          , "\" | jq -r '.archived_snapshots.closest.url')\"" ])]
    , defaultCommand = curl ++ "\"${URI}\""
    , externalViewers = M.fromList $
      map (flip (,) "emacsclient -n \"${FILE}\"")
      ["hs", "cabal", "c", "h", "el", "scm", "idr"]
      ++ map (flip (,) "xdg-open \"${FILE}\"")
      [ "svg", "png", "jpg", "jpeg", "gif", "pdf", "ogg", "ogv"
      , "webm", "mp3", "mp4", "mkv", "mpeg", "wav", "xspf", "m3u" ]
    , shortcuts = M.fromList
      [ ("ddg", "https://duckduckgo.com/lite/?q=")
      , ("wp", "https://en.m.wikipedia.org/wiki/Special:Search?search=")
      , ("wt", "https://en.m.wiktionary.org/w/index.php?search=")
      , ("g", "https://m.gutenberg.org/ebooks/search.mobile/?query=")
      , ("xiph", "http://dir.xiph.org/search?search=")
      , ("cs", "http://citeseerx.ist.psu.edu/search?q=")
      , ("gp", "gopher://gopherpedia.com:70/7/lookup%09")
      , ("vs", "gopher://gopher.floodgap.com/7/v2/vs%09")
      , ("wa", "web-archive:///?url=")]
    , paginate = True
    , historyDepth = 100
    , referenceDigits = "0123456789"
    , indentDivs = False
    , unclutter = M.fromList
      [ ("duckduckgo", "^https://duckduckgo\\.com/lite/\\?q=")
      , ("lobsters", "^https://lobste\\.rs/((page|recent|newest).*)?$")
      , ("hacker-news",
         "^https://news\\.ycombinator\\.com/((news|show|ask).*)?$")
      , ("mediawiki", "^https://en\\.(m.)?(wiktionary|wikipedia)\\.org/wiki/")
      , ("github", "^https://github\\.com/")]
    , pandocTimeout = 60
    }
    where
      curl = "curl -A \"pancake/${PANCAKE}\" --compressed -4 -L " ++
        "-w \"\n-pancake-\nuri: %{url_effective}\ntype: %{content_type}\n\" "

-- | Loads configuration from a given 'FilePath', or from an XDG
-- config directory. Writes a default one if it doesn't exist.
loadConfig :: MonadIO m => Maybe FilePath -> m Config
loadConfig mp = liftIO $ do
  configPath <- case mp of
    Nothing -> do
      dir <- getXdgDirectory XdgConfig "pancake"
      createDirectoryIfMissing True dir
      pure $ dir </> "config.yaml"
    Just p -> pure p
  exists <- doesFileExist configPath
  if exists
    then do
    c <- decodeFile configPath
    case c of
      Just config -> pure config
      Nothing -> putErrLn "Failed to read the configuration, using defaults"
        >> pure def
    else encodeFile configPath (def :: Config) >> pure def
