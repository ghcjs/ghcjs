{-# LANGUAGE NamedFieldPuns #-}
import           Control.Applicative   ((<$>), (<*>), pure)
import qualified Control.Exception     as Ex
import           Control.Monad         (when, forM_)

import           Data.Char             (isSpace)
import           Data.List             (isPrefixOf, isSuffixOf, partition)
import           Data.Maybe            (maybe, listToMaybe, fromMaybe)
import           Data.Version          (makeVersion, showVersion)

import           Distribution.PackageDescription hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.System
import           Distribution.Verbosity
import           Distribution.Version
import           Distribution.Types.ExecutableScope
import           Distribution.Types.PackageDescription
import           Distribution.Types.UnqualComponentName

import           Distribution.Simple.Install
import           Distribution.Simple.Register

import           System.Exit           (ExitCode(..))
import           System.Directory
  (createDirectoryIfMissing, doesFileExist, removeFile, renameFile, exeExtension)
import           System.FilePath
  ((</>), (<.>), splitExtensions, dropExtensions)
import           System.IO
import           System.IO.Error       (IOError, isDoesNotExistError)

main :: IO ()
main = defaultMainWithHooks ghcjsHooks

ghcjsHooks :: UserHooks
ghcjsHooks = simpleUserHooks
  { preSDist = ghcjsSDist
  , postCopy = ghcjsPostCopy
  , postInst = \args -> ghcjsPostCopy args . installFlagsToCopyFlags
  }

{- |
  check that we have a decent looking boot.tar file
 -}
ghcjsSDist :: Args -> SDistFlags -> IO HookedBuildInfo
ghcjsSDist as flags = do
  size <- withFile "data/boot.tar" ReadMode hFileSize
  if size < 1000000
    then error "aborting because data/boot.tar looks suspiciously small"
    else return emptyHookedBuildInfo

-- Necessary because postCopy isn't invoked when install is run.
-- Copied from https://github.com/haskell/cabal/blob/589cc887c4ef10f514174e0875d7df1963bdcf71/Cabal/Distribution/Simple.hs#L689
installFlagsToCopyFlags :: InstallFlags -> CopyFlags
installFlagsToCopyFlags flags = defaultCopyFlags
  { copyDistPref = installDistPref flags
  , copyDest = toFlag NoCopyDest
  , copyVerbosity = installVerbosity flags
  }

ghcjsPostCopy :: Args
              -> CopyFlags
              -> PackageDescription
              -> LocalBuildInfo
              -> IO ()
ghcjsPostCopy args flags descr lbi
  | any (\(flag, enabled) -> unFlagName flag == "no-wrapper-install" && enabled)
        (unFlagAssignment $ configConfigurationsFlags (configFlags lbi)) =
        return () {- User has opted to skip wrapper script installation.
                     Let's hope they know what they're doing.

                     Executables are "private", in the libexec directory
                   -}
  | otherwise = do
        wrapperEnv <- getWrapperEnv verbosity
                                    descr
                                    (copyDest flags)
                                    installDirs
                                    exes
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
                             , weLibexecDir :: FilePath
                             , weVersion    :: String
                             , weGhcVersion :: String
                             } deriving (Show)

verSuff :: WrapperEnv -> String
verSuff env = weVersion env ++ "-" ++ weGhcVersion env

getWrapperEnv :: Verbosity
              -> PackageDescription
              -> Flag CopyDest
              -> InstallDirs FilePath
              -> [Executable]
              -> IO WrapperEnv
getWrapperEnv v descr copyDest' installDirs exes
  | [Executable name _ _ bi] <-
       filter ((=="ghcjs").unUnqualComponentName.exeName) exes =
     let ghcjsVal xs =
           trim <$> rawSystemStdout v
                                    (libexecdir installDirs </> "ghcjs")
                                    ["--ghcjs-setup-print", xs]
     in  WrapperEnv <$> ghcjsVal "--print-default-topdir"
                    <*> pure ( dropPrefix copyDest' $ bindir installDirs)
                    <*> pure ( dropPrefix copyDest' $ libexecdir installDirs)
                    <*> pure ( Data.Version.showVersion
                             . makeVersion
                             . versionNumbers
                             . pkgVersion
                             . package
                             $ descr
                             )
                    <*> ghcjsVal "--numeric-ghc-version"
  | otherwise = error "cannot find ghcjs executable in package"

dropPrefix (Flag (CopyTo pre)) s | isPrefixOf pre s = drop (length pre) s
dropPrefix (Flag (CopyTo pre)) s = error $ "dropPrefix: " ++ show pre ++
                                           " not a prefix of " ++ show s
dropPrefix _ s = s

{- |
     on Windows we can't run shell scripts, so we don't install wrappers
     just copy program.exe to program-{version}-{ghcversion}.exe

     the programs read a program-{version}-{ghcversion}.exe.options file from
     the same directory, which contains the command line arguments to prepend

     installation does not overwrite existing .options files
 -}
copyWrapperW :: Verbosity
             -> WrapperEnv
             -> PackageDescription
             -> InstallDirs FilePath
             -> Executable
             -> IO ()
copyWrapperW v env descr installDirs exe
  | exeScope exe /= ExecutablePrivate = pure ()
  | otherwise = do
  createDirectoryIfMissing False b
  installExecutableFile v srcExe destExe
  installExecutableFile v srcExe destExeVer -- always make a versioned copy
  requiresOptions <- doesFileExist srcOptions
  when requiresOptions $ do             -- we need a wrapper
    optionsExists <- doesFileExist destOptions
    when (not optionsExists) $ do
      options <- replacePlaceholders env <$> readFile srcOptions
      createDirectoryIfMissing False b
      withTempFile b "ghcjs-options-XXXXXX.tmp" $ \tmp h -> do
        hPutStr h options
        hClose h
        installOrdinaryFile v tmp destOptions
    where
      e            = unUnqualComponentName . exeName $ exe
      b            = bindir installDirs
      -- example: libexec\ghcjs.exe
      srcExe       = libexecdir installDirs </> e <.> "exe"
      -- example: bin\ghcjs.exe
      --    (copy of srcExe)
      destExe      = b </> e <.> "exe"
      -- example: bin\ghcjs-8.2.0.1-8.2.2.exe
      --    (copy of srcExe)
      destExeVer   = b </> e ++ "-" ++ verSuff env <.> "exe"
      -- example: lib\ghcjs.exe.options
      srcOptions   = datadir installDirs </> "bin" </>
                        e <.> "exe" <.> "options"
      -- example: bin\ghcjs-8.2.0.1-8.2.2.exe.options
      --    (created, existing files not overwritten)
      destOptions  = destExeVer <.> "options"

{- |
     on non-Windows we copy shell scripts that pass the -B flag to ghcjs,
     ghcjs-pkg etc

     installation updates the symlink, but does not overwrite the wrapper
     scripts if they already exist

     if no wrapper is required, we simply symlink to the executable in the
     libexec directory
 -}
copyWrapperU :: Verbosity
             -> WrapperEnv
             -> PackageDescription
             -> InstallDirs FilePath
             -> Executable
             -> IO ()
copyWrapperU v env descr installDirs exe
  | exeScope exe /= ExecutablePrivate = pure ()
  | otherwise = do
  requiresWrapper <- doesFileExist srcWrapper
  if requiresWrapper
    then do
      -- install wrapper, but do not overwrite existing files
      wrapperExists <- doesFileExist (b </> destWrapperVer)
      when (not wrapperExists) $ do
        wrapperScript <- replacePlaceholders env <$> readFile srcWrapper
        createDirectoryIfMissing False b
        withTempFile b "ghcjs-wrapper-XXXXXX.tmp" $
          \tmp h -> do hPutStr h wrapperScript
                       hClose h
                       installExecutableFile v tmp (b </> destWrapperVer)
    else
      -- just create symlink
      linkFileU v b srcExe destWrapperVer
  linkFileU v b destWrapperVer destWrapper
  where
    e                = unUnqualComponentName . exeName $ exe
    e'               = dropExtensions e
    b                = bindir installDirs
    -- example: libexec/ghcjs
    --   (replaced with symlink to destExe if there are no wrappers)
    srcExe           = libexecdir installDirs </> e
    -- example: data/bin/ghcjs.sh
    srcWrapper       = datadir installDirs </> "bin" </> e' <.> "sh"
    -- example: bin/ghcjs
    --   (symlink to destWrapperVer, existing files/links overwritten)
    destWrapper      = e
    -- example: bin/ghcjs-8.2.0.1-8.2.2
    --   (created if not exists, existing files not overwritten)
    destWrapperVer   = e ++ "-" ++ verSuff env

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
  exitCode <- rawSystemIOWithEnv v
                                 "/usr/bin/env"
                                 ["ln", "-s", src, dest]
                                 (Just workingDir)
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
  when (exitCode /= ExitSuccess)
       (error  $ "could not create symlink " ++
                 src ++ " -> " ++ dest ++ " in " ++ workingDir)

-- | replace placeholders in a wrapper script or options file
replacePlaceholders :: WrapperEnv -> String -> String
replacePlaceholders env xs =
  foldl (\ys (p,r) -> replace p (r env) ys) xs
    [ ("{topdir}",     weTopDir)
    , ("{bindir}",     weBinDir)
    , ("{libexecdir}", weLibexecDir)
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
