import           Control.Applicative   ((<$>), (<*>), pure)
import qualified Control.Exception     as Ex
import           Control.Monad         (when)

import           Data.Char             (isSpace)
import           Data.List             (isPrefixOf, isSuffixOf)
import           Data.Maybe            (maybe, listToMaybe)
import           Data.Version          (showVersion)

import           Distribution.PackageDescription hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.System
import           Distribution.Verbosity

import           System.Exit           (ExitCode(..))
import           System.Directory      (doesFileExist, removeFile, renameFile)
import           System.FilePath       ((</>), (<.>), splitExtensions, dropExtensions)
import           System.IO
import           System.IO.Error       (IOError, isDoesNotExistError)

{-
    add all executables that are not wrapped (or require an .options file on Windows) here
 -}
notWrapped :: [String]
notWrapped = ["ghcjs-boot", "ghcjs-run"]

main :: IO ()
main = defaultMainWithHooks ghcjsHooks

ghcjsHooks :: UserHooks
ghcjsHooks = simpleUserHooks { preSDist = ghcjsSDist
                             , postCopy = ghcjsPostCopy
                             , postInst = \args -> ghcjsPostCopy args . installFlagsToCopyFlags
                             }

{- |
    Build tar cache archives for ghcjs-boot libraries, shims (the runtime system) and the test suite
    for the source distribution. These are required for doing a release installation (no --dev flag)
    with ghcjs-boot (at least with the default boot.yaml).
 -}
ghcjsSDist :: Args -> SDistFlags -> IO HookedBuildInfo
ghcjsSDist as flags = do
  rawSystemExit (fromFlagOrDefault normal $ sDistVerbosity flags) "bash" ["utils/update_archives.sh"]
  return emptyHookedBuildInfo

-- Necessary because postCopy isn't invoked when install is run.
-- Copied from https://github.com/haskell/cabal/blob/589cc887c4ef10f514174e0875d7df1963bdcf71/Cabal/Distribution/Simple.hs#L689
installFlagsToCopyFlags :: InstallFlags -> CopyFlags
installFlagsToCopyFlags flags = defaultCopyFlags
  { copyDistPref = installDistPref flags
  , copyDest = toFlag NoCopyDest
  , copyVerbosity = installVerbosity flags
  }

ghcjsPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
ghcjsPostCopy args flags descr lbi
  | (FlagName "no-wrapper-install", True) `elem` configConfigurationsFlags (configFlags lbi) =
        return () -- User has opted to skip wrapper script installation. Let's hope they know what they're doing.
                  -- Executables will keep their original names, e.g. ghcjs.bin, ghcjs-pkg.bin
  | otherwise = do
        wrapperEnv <- getWrapperEnv verbosity descr (copyDest flags) installDirs exes
        mapM_ (copyWrapper verbosity wrapperEnv descr installDirs) exes
    where
      exes        = executables descr
      copyWrapper = if buildOS == Windows
                       then copyWrapperW
                       else copyWrapperU
      verbosity   = fromFlagOrDefault normal (copyVerbosity flags)
      installDirs = absoluteInstallDirs descr lbi (fromFlag $ copyDest flags)

data WrapperEnv = WrapperEnv { weTopDir     :: FilePath
                             , weBinDir     :: FilePath
                             , weVersion    :: String
                             , weGhcVersion :: String
                             }

verSuff :: WrapperEnv -> String
verSuff env = weVersion env ++ "-" ++ weGhcVersion env

requiresWrapper :: String -> Bool
requiresWrapper exe = exe `notElem` notWrapped

getWrapperEnv :: Verbosity -> PackageDescription -> Flag CopyDest -> InstallDirs FilePath -> [Executable] -> IO WrapperEnv
getWrapperEnv v descr copyDest' installDirs exes
  | [Executable name _ bi] <- filter ((=="ghcjs").exeName) exes =
     let ghcjsVal xs =
           trim <$> rawSystemStdout v (bindir installDirs </> "ghcjs") ["--ghcjs-setup-print", xs]
     in  WrapperEnv <$> ghcjsVal "--print-default-topdir"
                    <*> pure (dropPrefix copyDest' $ bindir installDirs)
                    <*> pure (showVersion . pkgVersion . package $ descr)
                    <*> ghcjsVal "--numeric-ghc-version"
  | otherwise = error "cannot find ghcjs executable in package"

dropPrefix (Flag (CopyTo pre)) s | isPrefixOf pre s = drop (length pre) s
dropPrefix (Flag (CopyTo pre)) s = error $ "dropPrefix - " ++ show pre ++ " not a prefix of " ++ show s
dropPrefix _ s = s

{- |
     on Windows we can't run shell scripts, so we don't install wrappers
     just copy program.exe to program-{version}-{ghcversion}.exe

     the programs read a program-{version}-{ghcversion}.exe.options file from the
     same directory, which contains the command line arguments to prepend

     installation does not overwrite existing .options files
 -}
copyWrapperW :: Verbosity
             -> WrapperEnv
             -> PackageDescription
             -> InstallDirs FilePath
             -> Executable
             -> IO ()
copyWrapperW v env descr installDirs exe = do
      installExecutableFile v srcExe destExeVer -- always make a versioned copy
      when (requiresWrapper e) $ do             -- we need a wrapper
        optionsExists <- doesFileExist destOptions
        when (not optionsExists) $ do
          options <- replacePlaceholders env <$> readFile srcOptions
          withTempFile b "ghcjs-options-XXXXXX.tmp" $ \tmp h -> do
            hPutStr h options
            hClose h
            installOrdinaryFile v tmp destOptions
    where
      e            = exeName exe
      e'           = dropExtensions e
      b            = bindir installDirs
      srcExe       = b </> e' <.> "exe"                           -- ex: bin\ghcjs.exe
      destExeVer   = b </> e' ++ "-" ++ verSuff env <.> "exe"     -- ex: bin\ghcjs-0.1.0-7.8.3.exe         (copy of srcExe)
      srcOptions   = datadir installDirs </> "lib" </> "bin" </>  -- ex: lib\ghcjs.exe.options
                        e' <.> "exe" <.> "options"
      destOptions  = destExeVer <.> "options"                     -- ex: bin\ghcjs-0.1.0-7.8.3.exe.options (created, existing files not overwritten)

{- |
     on non-Windows we copy shell scripts that pass the -B flag to ghcjs, ghcjs-pkg etc

     the ghcjs.bin executable is renamed to ghcjs-{version}-{ghcversion}.bin
     the wrapper shell script is named ghcjs-{version}-{ghcversion}, with a
     an unversioned symlink pointing to it.

     installation updates the symlink, but does not overwrite the wrapper scripts
     if they already exist
 -}
copyWrapperU :: Verbosity
             -> WrapperEnv
             -> PackageDescription
             -> InstallDirs FilePath
             -> Executable
             -> IO ()
copyWrapperU v env descr installDirs exe
  | requiresWrapper e = do
      installExecutableFile v (b </> srcExe) (b </> destExe)
      removeFile (b </> srcExe)
      wrapperExists <- doesFileExist (b </> destWrapperVer)
      when (not wrapperExists) $ do
        script <- replacePlaceholders env <$> readFile srcWrapper
        withTempFile (bindir installDirs) "ghcjs-wrapper-XXXXXX.tmp" $ \tmp h -> do
          hPutStr h script
          hClose h
          installExecutableFile v tmp (b </> destWrapperVer)
      linkFileU v b destWrapperVer destWrapper
  | otherwise = do
      installExecutableFile v (b </> srcExe) (b </> srcExe ++ "-" ++ verSuff env)
      removeFile (b </> srcExe)
      linkFileU v b (srcExe ++ "-" ++ verSuff env) srcExe
  where
    e                = exeName exe
    e'               = dropExtensions e
    b                = bindir installDirs
    srcExe           = e                                                       -- ex: bin/ghcjs                 (removed, replaced with symlink to destExe if there are no wrappers)
    destExe          = e' ++ "-" ++ verSuff env <.> ".bin"                     -- ex: bin/ghcjs-0.1.0-7.8.3.bin (copy of srcExe)
    srcWrapper       = datadir installDirs </> "lib" </> "bin" </> e' <.> "sh" -- ex: etc/ghcjs.sh
    destWrapper      = e'                                                      -- ex: bin/ghcjs                 (symlink to destWrapperVer, existing files/links overwritten)
    destWrapperVer   = e' ++ "-" ++ verSuff env                                -- ex: bin/ghcjs-0.1.0-7.8.3     (created if not exists, existing files not overwritten)

{- |
     create a symlink, overwriting the target. unix only.

     it looks like the Cabal library does not have this functionality,
     and since we shouldn't use system-specific libraries here,
     we use a shell command instead.

     the symlink is relative if both files are in the same directory
  -}
linkFileU :: Verbosity -> FilePath -> FilePath -> FilePath -> IO ()
linkFileU v workingDir src dest = do
  let ignoreDoesNotExist :: IOError -> IO ()
      ignoreDoesNotExist e | isDoesNotExistError e = return ()
                           | otherwise             = Ex.throw e
  removeFile (workingDir </> dest) `Ex.catch` ignoreDoesNotExist
  exitCode <- rawSystemIOWithEnv v "/usr/bin/env" ["ln", "-s", src, dest] (Just workingDir) Nothing Nothing Nothing Nothing
  when (exitCode /= ExitSuccess) (error  $ "could not create symlink " ++ src ++ " -> " ++ dest ++ " in " ++ workingDir)

-- | replace placeholders in a wrapper script or options file
replacePlaceholders :: WrapperEnv -> String -> String
replacePlaceholders env xs =
  foldl (\ys (p,r) -> replace p (r env) ys) xs
    [ ("{topdir}",     weTopDir)
    , ("{bindir}",     weBinDir)
    , ("{version}",    weVersion)
    , ("{ghcversion}", weGhcVersion)
    ]

replace :: String -> String -> String -> String
replace from to xxs@(x:xs)
  | from `isPrefixOf` xxs = to ++ replace from to (drop (length from) xxs)
  | otherwise             = x : replace from to xs
replace _ _ [] = []

trim :: String -> String
trim = let f = dropWhile isSpace . reverse in f . f
