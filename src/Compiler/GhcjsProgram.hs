{-# LANGUAGE ScopedTypeVariables #-}
{-
  The GHCJS-specific parts of the frontend (ghcjs program)

  Our main frontend is copied from GHC, Compiler.Program
 -}

module Compiler.GhcjsProgram where

import           GHC
import           DynFlags
import           PackageConfig
import           UniqFM
import           Packages
import           PrimOp
import           PrelInfo
import           IfaceEnv
import           HscTypes
import           DsMeta
import           LoadIface
import           ErrUtils (fatalErrorMsg'')
import           Panic (handleGhcException)
import           Exception

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.IORef
import           Data.List (isSuffixOf, isPrefixOf, partition,)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import           Distribution.System (buildOS, OS(..))
import           Distribution.Verbosity (deafening, intToVerbosity)
import           Distribution.Package (PackageName(..))
import           Distribution.Simple.BuildPaths (exeExtension)
import           Distribution.Simple.Utils (installExecutableFile, installDirectoryContents)
import           Distribution.Simple.Program (runProgramInvocation, simpleProgramInvocation)

import           Options.Applicative

import           System.Directory (doesFileExist, doesDirectoryExist)
import           System.Environment (getArgs)
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

import           Compiler.GhcjsPlatform
import           Compiler.Info
import           Compiler.Settings
import           Compiler.Utils

-- fixme, make frontend independent of backend
import qualified Gen2.Object      as Object
import qualified Gen2.ClosureInfo as Gen2
import qualified Gen2.PrimIface   as Gen2
import qualified Gen2.Shim        as Gen2
import qualified Gen2.Rts         as Gen2
import qualified Gen2.RtsTypes    as Gen2


{- |
  Check if we're building a Cabal Setup script, in which case automatically
  switch to building a native executable and skip building all JS

  Detection is a bit tricky:
  - compilation mode is --make
  - Cabal doesn't manage dependencies for Setup.hs, so -hide-all-packages is not used
  - There is one Haskell source file, named Setup.hs or Setup.lhs
  - executable is named setup or setup.exe in a setup subdir
  - object and hi output dirs end with a setup subdir
-}
buildingCabalSetup :: [FilePath] -> DynFlags -> Bool
buildingCabalSetup [hs_src] dflags
  = ghcMode dflags == CompManager         &&
    not (gopt Opt_HideAllPackages dflags) &&
    isSetupOutput (outputFile dflags)     &&
    isSetupDir    (objectDir  dflags)     &&
    isSetupDir    (hiDir      dflags)     &&
    isSetupSource hs_src
  where
    forwardSlashes = map (\x -> if x == '\\' then '/' else x)
    isSetupOutput  = maybe False (("/setup/setup" `isSuffixOf`) . forwardSlashes . dropExtension)
    isSetupDir     = maybe False (("/setup" `isSuffixOf`) . forwardSlashes)
    isSetupSource  = (`elem` ["setup.hs", "Setup.hs", "Setup.lhs"]) . takeFileName
buildingCabalSetup _ _ = False

getGhcjsSettings :: [Located String] -> IO ([Located String], GhcjsSettings)
getGhcjsSettings args =
  case p of
    Left failure -> do
      hPutStrLn stderr =<< errMessage failure "ghcjs"
      exitWith (errExitCode failure)
    Right gs1 -> do
      gs2 <- envSettings
      return (args', gs1 <> gs2)
  where
    (ga,args') = partition (\a -> any (`isPrefixOf` unLoc a) as) args
    p = execParserPure (prefs mempty) optParser' (map unLoc ga)
    as = [ "--native-executables"
         , "--no-native"
         , "--no-js-executables"
         , "--strip-program="
         , "--log-commandline="
         , "--with-ghc="
         , "--only-out"
         , "--no-rts"
         , "--no-stats"
         , "--generate-base="
         , "--use-base="
         ]
    envSettings = GhcjsSettings <$> getEnvOpt "GHCJS_NATIVE_EXECUTABLES"
                                <*> getEnvOpt "GHCJS_NO_NATIVE"
                                <*> pure False
                                <*> pure Nothing
                                <*> getEnvMay "GHCJS_LOG_COMMANDLINE_NAME"
                                <*> getEnvMay "GHCJS_WITH_GHC"
                                <*> pure False
                                <*> pure False
                                <*> pure False
                                <*> pure Nothing
                                <*> pure Nothing

optParser' :: ParserInfo GhcjsSettings
optParser' = info (helper <*> optParser) fullDesc

optParser :: Parser GhcjsSettings
optParser = GhcjsSettings
            <$> switch ( long "native-executables" )
            <*> switch ( long "no-native" )
            <*> switch ( long "no-js-executables" )
            <*> optStr ( long "strip-program" )
            <*> optStr ( long "log-commandline" )
            <*> optStr ( long "with-ghc" )
            <*> switch ( long "only-out" )
            <*> switch ( long "no-rts" )
            <*> switch ( long "no-stats" )
            <*> optStr ( long "generate-base" )
            <*> optStr ( long "use-base" )

optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m = nullOption $ value Nothing <> reader (pure . str)  <> m

printVersion :: IO ()
printVersion = putStrLn $
  "The Glorious Glasgow Haskell Compilation System for JavaScript, version " ++
     getCompilerVersion ++ " (GHC " ++ getGhcCompilerVersion ++ ")"

printNumericVersion :: IO ()
printNumericVersion = putStrLn getCompilerVersion

printRts :: DynFlags -> IO ()
printRts dflags = TL.putStrLn (Gen2.rtsText $ Gen2.dfCgSettings dflags) >> exitSuccess

printDeps :: FilePath -> IO ()
printDeps = Object.readDepsFile >=> TL.putStrLn . Object.showDeps

printObj :: FilePath -> IO ()
printObj = Object.readObjectFile >=> TL.putStrLn . Object.showObject

-- replace primops in the name cache so that we get our correctly typed primops
fixNameCache :: GhcMonad m => m ()
fixNameCache = do
  sess <- getSession
  liftIO $ modifyIORef (hsc_NC sess) $ \(NameCache u _) ->
    (initNameCache u knownNames)
    where
      knownNames = map getName (filter (not.isPrimOp) wiredInThings) ++
                      basicKnownKeyNames ++
                      templateHaskellNames ++
                      map (getName . AnId . Gen2.mkGhcjsPrimOpId) allThePrimOps
      isPrimOp (AnId i) = isPrimOpId i
      isPrimOp _        = False

checkIsBooted :: IO ()
checkIsBooted = do
  base <- getGlobalPackageBase
  let settingsFile = base </> "settings"
  e <- doesFileExist settingsFile
  when (not e) $ do
    hPutStrLn stderr $ "cannot find `" ++ settingsFile ++ "'\n" ++
                       "please install the GHCJS core libraries. See README for details\n" ++
                       "(running `ghcjs-boot --init' might fix this)"
    exitWith (ExitFailure 87)

-- | when booting GHCJS, we pretend to have the Cabal lib installed
--   call GHC to compile our Setup.hs
bootstrapFallback :: IO ()
bootstrapFallback = do
    ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_FALLBACK_GHC"
    getArgs >>= rawSystem ghc >>= exitWith -- run without GHCJS package args

installExecutable :: DynFlags -> GhcjsSettings -> [String] -> IO ()
installExecutable dflags settings srcs = do
    case (srcs, outputFile dflags) of
        ([from], Just to) -> do
          let v = fromMaybe deafening . intToVerbosity $ verbosity dflags
          nativeExists <- doesFileExist $ from <.> exeExtension
          when nativeExists $ do
            installExecutableFile v (from <.> exeExtension) (to <.> exeExtension)
            let stripFlags = if buildOS == OSX then ["-x"] else []
            case gsStripProgram settings of
                Just strip -> runProgramInvocation v . simpleProgramInvocation strip $
                                stripFlags ++ [to <.> exeExtension]
                Nothing -> return ()
          jsExists <- doesDirectoryExist $ from <.> jsexeExtension
          when jsExists $ installDirectoryContents v (from <.> jsexeExtension) (to <.> jsexeExtension)
          unless (nativeExists || jsExists) $ do
            hPutStrLn stderr $ "No executable found to install at " ++ from
            exitFailure
        _ -> do
            hPutStrLn stderr "Usage: ghcjs --install-executable <from> -o <to>"
            exitFailure

-- we might generate .hi files for a different bitness than native GHC,
-- make sure we can show then
printIface :: [String] -> IO ()
printIface ["--show-iface", iface] = do
     (argsS, _) <- parseStaticFlags $ map noLoc []
     runGhcSession Nothing $ do
       sdflags <- getSessionDynFlags
       base <- liftIO ghcjsDataDir
       jsEnv <- liftIO newGhcjsEnv
       setSessionDynFlags $ setGhcjsPlatform mempty jsEnv [] base sdflags
       env <- getSession
       liftIO $ showIface env iface
printIface _                       = putStrLn "usage: ghcjs --show-iface hifile"

{-
  Generate lib.js and lib1.js for the latest version of all installed
  packages

  fixme: make this variant-aware?
 -}

generateLib :: GhcjsSettings -> Ghc ()
generateLib settings = do
  dflags1 <- getSessionDynFlags
  liftIO $ do
    (dflags2, _) <- initPackages dflags1
    let pkgs =  map sourcePackageId . eltsUFM . pkgIdMap . pkgState $ dflags2
    base <- (</> "shims") <$> getGlobalPackageBase
    let convertPkg p = let PackageName n = pkgName p
                           v = map fromIntegral (versionBranch $ pkgVersion p)
                       in (T.pack n, v)
        pkgs' = M.toList $ M.fromListWith max (map convertPkg pkgs)
    ((before, _), (after, _)) <- Gen2.collectShims dflags2 settings base pkgs'
    T.writeFile "lib.js" before
    T.writeFile "lib1.js" after
    putStrLn "generated lib.js and lib1.js for:"
    mapM_ (\(p,v) -> putStrLn $ "    " ++ T.unpack p ++
      if null v then "" else ("-" ++ L.intercalate "." (map show v))) pkgs'

-- | Sets up GHCJS package databases, requires a call to initPackages
addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  let replaceConf GlobalPkgConf = PkgConfFile db1
      replaceConf UserPkgConf   = PkgConfFile db2
      replaceConf x             = x
  return $ df { extraPkgConfs = map replaceConf . extraPkgConfs df }


setGhcjsSuffixes :: Bool     -- oneshot option, -c
                 -> DynFlags
                 -> DynFlags
setGhcjsSuffixes oneshot df = df
    { objectSuf     = mkGhcjsSuf (objectSuf df)
    , dynObjectSuf  = mkGhcjsSuf (dynObjectSuf df)
    , hiSuf         = mkGhcjsSuf (hiSuf df)
    , dynHiSuf      = mkGhcjsSuf (dynHiSuf df)
    , outputFile    = fmap mkGhcjsOutput (outputFile df)
    , dynOutputFile = fmap mkGhcjsOutput (dynOutputFile df)
    , outputHi      = fmap mkGhcjsOutput (outputHi df)
    , ghcLink       = if oneshot then NoLink else ghcLink df
    }


-- | make sure we don't show panic messages with the "report GHC bug" text, since
--   those are probably our fault.
ghcjsErrorHandler :: (ExceptionMonad m, MonadIO m)
                    => FatalMessager -> FlushOut -> m a -> m a
ghcjsErrorHandler fm (FlushOut flushOut) inner =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  ghandle (\exception -> liftIO $ do
           flushOut
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fatalErrorMsg'' fm (show ioe)
                _ -> case fromException exception of
                     Just UserInterrupt ->
                         -- Important to let this one propagate out so our
                         -- calling process knows we were interrupted by ^C
                         liftIO $ throwIO UserInterrupt
                     Just StackOverflow ->
                         fatalErrorMsg'' fm "stack overflow: use +RTS -K<size> to increase it"
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> liftIO $ throwIO ex
                          _ -> case fromException exception of
                               -- don't panic!
                               Just (Panic str) -> fatalErrorMsg'' fm str
                               _                -> fatalErrorMsg'' fm (show exception)
           exitWith (ExitFailure 1)
         ) $

  -- error messages propagated as exceptions
  handleGhcException
            (\ge -> liftIO $ do
                flushOut
                case ge of
                     PhaseFailed _ code -> exitWith code
                     Signal _ -> exitWith (ExitFailure 1)
                     _ -> do fatalErrorMsg'' fm (show ge)
                             exitWith (ExitFailure 1)
            ) $
  inner

sourceErrorHandler :: GhcMonad m => m a -> m a
sourceErrorHandler m = handleSourceError (\e -> do
  GHC.printException e
  liftIO $ exitWith (ExitFailure 1)) m

runGhcjsSession :: Maybe FilePath  -- ^ Directory with library files,
                   -- like GHC's -B argument
                -> GhcjsSettings
                -> Ghc b           -- ^ Action to perform
                -> IO b
runGhcjsSession mbMinusB settings m = runGhcSession mbMinusB $ do
    base <- liftIO ghcjsDataDir
    dflags <- getSessionDynFlags
    jsEnv <- liftIO newGhcjsEnv
    _ <- setSessionDynFlags
         $ setGhcjsPlatform settings jsEnv [] base
         $ updateWays $ addWay' (WayCustom "js")
         $ setGhcjsSuffixes False dflags
    fixNameCache
    m

runGhcSession :: Maybe FilePath -> Ghc b -> IO b
runGhcSession mbMinusB a = do
    libDir <- getGlobalPackageBase
    runGhc (mbMinusB `mplus` Just libDir) a

