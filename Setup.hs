import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, hookedPrograms, instHook, copyHook)
import Distribution.Simple.Program.Types (simpleProgram)
import Distribution.Simple.Setup (CopyDest(..), installVerbosity, copyVerbosity, fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, installOrdinaryFile)
import Distribution.Simple.LocalBuildInfo (buildDir, absoluteInstallDirs, InstallDirs(..))
import System.FilePath ((</>))

main = defaultMainWithHooks simpleUserHooks
         { hookedPrograms = [simpleProgram "java"]
         , instHook = ghcjsInstHook
         , copyHook = ghcjsCopyHook
         }

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

