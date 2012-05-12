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
         , instHook       = ghcjsInstHook
         , postInst       = ghcjsPostInst
         , copyHook       = ghcjsCopyHook
         }

ghcjsPostInst _ _ pkgDesc lbi = when doBoot (autoboot >> return ())
  where
    ghcjsexe = listToMaybe $ filter (\(Executable s _ _) -> s == "ghcjs") 
                 (executables . localPkgDescr $ lbi)
    doBoot   = maybe False (any (==("x-boot", "True")).customFieldsBI.buildInfo) ghcjsexe

autoboot = rawSystem "ghcjs-boot" ["--auto"] -- fixme make sure that the one from the current install is being run

ghcjsInstHook pkg_descr lbi hooks flags = do
  instHook simpleUserHooks pkg_descr lbi hooks flags
  copyRts pkg_descr lbi $ fromFlag (installVerbosity flags)

ghcjsCopyHook pkg_descr lbi hooks flags = do
  copyHook simpleUserHooks pkg_descr lbi hooks flags
  copyRts pkg_descr lbi $ fromFlag (copyVerbosity flags)

copyRts pkg_descr lbi verbosity = do
  let lib         = libdir $absoluteInstallDirs pkg_descr lbi NoCopyDest
      destination = lib </> "rts.jso"
      copy n      = installOrdinaryFile verbosity ("rts" </> n) (destination </> n)

  createDirectoryIfMissingVerbose verbosity True destination
  copy "rts-options.js"
  copy "rts-common.js"
  copy "rts-plain.js"
  copy "rts-trampoline.js"

