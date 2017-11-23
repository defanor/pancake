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
                     } deriving (Generic, Show)

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
          ++ " -w \"\n-pancake-\n\"")]
    , defaultCommand = "curl -4 -L \"${URI}\""
      ++ " -w \"\n-pancake-\nuri: %{url_effective}\ntype: %{content_type}\n\""
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
      , ("gp", "gopher://gopherpedia.com:70/7/lookup?")
      , ("vs", "gopher://gopher.floodgap.com/7/v2/vs?")]
    , paginate = True
    , historyDepth = 100
    , referenceDigits = "0123456789"
    }

-- | Loads configuration from an XDG config directory.
loadConfig :: MonadIO m => m Config
loadConfig = liftIO $ do
  dir <- getXdgDirectory XdgConfig "pancake"
  createDirectoryIfMissing True dir
  let configPath = dir </> "config.yaml"
  exists <- doesFileExist configPath
  if exists
    then do
    c <- decodeFile configPath
    case c of
      Just config -> pure config
      Nothing -> putErrLn "Failed to read the configuration, using defaults"
        >> pure def
    else encodeFile configPath (def :: Config) >> pure def
