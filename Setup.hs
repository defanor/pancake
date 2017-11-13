import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.Setup
import System.FilePath

main = defaultMainWithHooks simpleUserHooks { postCopy = installManPage }

installManPage :: Args
               -> CopyFlags
               -> PackageDescription
               -> LocalBuildInfo
               -> IO ()
installManPage _ cf pd lbi =
  let dirs = absoluteInstallDirs pd lbi (fromFlag $ copyDest cf)
      man1 = mandir dirs </> "man1"
      fname = "pancake.1"
      target = man1 </> fname
  in installOrdinaryFile (fromFlag $ copyVerbosity cf) fname target
