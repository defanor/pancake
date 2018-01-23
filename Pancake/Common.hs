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
Module      :  Pancake.Common
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Utility functions.
-}

module Pancake.Common ( putErrLn, escapeURI ) where
import System.IO
import Control.Monad.IO.Class
import Network.URI
import System.FilePath


-- | Prints a line into stderr.
putErrLn :: MonadIO m => String -> m ()
putErrLn s = liftIO $ do
  hPutStrLn stderr s
  hFlush stderr

-- | Escapes an URI for use as a file name.
escapeURI :: URI -> FilePath
escapeURI u = map escapeChar $ uriToString id u ""
  where escapeChar c
          | isPathSeparator c = '-'
          | otherwise = c
