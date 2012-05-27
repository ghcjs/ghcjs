import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, hookedPrograms, UserHooks(..))
import Distribution.Simple.Program.Types (simpleProgram)
import Distribution.Simple.Setup (CopyDest(..), installVerbosity, copyVerbosity, fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, installOrdinaryFile)
import Distribution.Simple.LocalBuildInfo (buildDir, absoluteInstallDirs, InstallDirs(..), LocalBuildInfo(..))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..), Executable(..))
import System.FilePath ((</>))
import System.Process (rawSystem)
import Control.Monad (when)
import Data.Maybe (maybe, listToMaybe)

main = defaultMainWithHooks simpleUserHooks
         { hookedPrograms = [simpleProgram "java"]
         , postInst       = ghcjsPostInst
         }

ghcjsPostInst _ _ pkgDesc lbi = when doBoot (autoboot >> return ())
  where
    ghcjsexe = listToMaybe $ filter (\(Executable s _ _) -> s == "ghcjs") 
                 (executables . localPkgDescr $ lbi)
    doBoot   = maybe False (any (==("x-boot", "True")).customFieldsBI.buildInfo) ghcjsexe

autoboot = rawSystem "ghcjs-boot" ["--auto"] -- fixme make sure that the one from the current install is being run

