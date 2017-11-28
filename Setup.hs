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

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.Setup
import System.FilePath
import System.Directory

main = defaultMainWithHooks simpleUserHooks { postCopy = installManPage }

installManPage :: Args
               -> CopyFlags
               -> PackageDescription
               -> LocalBuildInfo
               -> IO ()
installManPage _ cf pd lbi = do
  let dirs = absoluteInstallDirs pd lbi (fromFlag $ copyDest cf)
      man1 = mandir dirs </> "man1"
      fname = "pancake.1"
      target = man1 </> fname
  createDirectoryIfMissing True man1
  installOrdinaryFile (fromFlag $ copyVerbosity cf) fname target
