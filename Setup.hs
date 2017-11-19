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
