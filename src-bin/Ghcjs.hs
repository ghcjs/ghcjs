{-# LANGUAGE CPP,
             TypeFamilies,
             ScopedTypeVariables,
             TupleSections,
             OverloadedStrings #-}

module Main where

import           Config (cProjectVersion)
import           GHC
import           Hooks
import           HscMain
import           UniqFM (eltsUFM)
import           DynFlags
import           Platform
import           CorePrep (corePrepPgm)
import           DriverPhases (HscSource, Phase(..),
                               isHaskellSrcFilename, isHaskellUserSrcFilename,
                               isSourceFilename, startPhase)
import           DriverPipeline
import           DriverMkDepend ( doMkDependHS )
import           DsMeta (templateHaskellNames)
import           Exception
import           HscTypes (CgGuts(..), HscEnv(..), Dependencies(..),
                           NameCache (..), isBootSummary,
                           FindResult(..),
                           mkSOName, mkHsSOName )
import           IfaceEnv (initNameCache)
import           LoadIface
import           Outputable (showPpr)
import           Panic
import           Module
import           PrelInfo (wiredInThings)
import           PrelNames (basicKnownKeyNames)
import           PrimOp (allThePrimOps)
import           SysTools ( touch )
import           Packages
import           MkIface
import           GhcMonad
import           Digraph
import           Binary (fingerprintBinMem, openBinMem, put_)
import           Constants (hiVersion)
import           TcRnMonad (initIfaceCheck)
import           Util (looksLikeModuleName)
import           Outputable hiding ((<>))
import           MonadUtils (MonadIO(..))
import qualified SysTools
import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Monad

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8  as C8
import           Data.Char (toLower)
import           Data.IORef (modifyIORef, writeIORef)
import           Data.List (isSuffixOf, isPrefixOf, tails, partition, nub,
                            intercalate, foldl', isInfixOf, sort)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Encoding as T

import           Distribution.Package (PackageName(..), PackageIdentifier(..))

import           Options.Applicative
import           Options.Applicative.Types
import           Options.Applicative.Builder.Internal

import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory,
                                   doesFileExist, copyFile)
import           System.Environment (getArgs, getEnv, getEnvironment)
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process (rawSystem)

import           GHCJS

import           Compiler.Info
import           Compiler.Variants
import           Compiler.GhcjsHooks
import           Compiler.GhcjsPlatform
import           Compiler.Utils         as Util

import qualified Gen2.Utils     as Gen2
import qualified Gen2.Generator as Gen2
import qualified Gen2.Linker    as Gen2
import qualified Gen2.Rts       as Gen2
import qualified Gen2.PrimIface as Gen2
import qualified Gen2.Foreign   as Gen2
import qualified Gen2.Object    as Object

import           Finder (findImportedModule, cannotFindInterface)

data GhcjsSettings = GhcjsSettings { gsNativeExecutables :: Bool
                                   , gsNoNative          :: Bool
                                   , gsNoJSExecutables   :: Bool
                                   , gsLogCommandLine    :: Maybe FilePath
                                   , gsGhc               :: Maybe FilePath
                                   , gsDebug             :: Bool
                                   } deriving (Eq, Show)

instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False Nothing Nothing False
  mappend (GhcjsSettings ne1 nn1 nj1 lc1 gh1 dbg1)
          (GhcjsSettings ne2 nn2 nj2 lc2 gh2 dbg2) =
          GhcjsSettings (ne1 || ne2) (nn1 || nn2) (nj1 || nj2) (lc1 <> lc2) (gh1 <> gh2) (dbg1 || dbg2)

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
    isSetupOutput = maybe False (("/setup/setup" `isSuffixOf`) . dropExtension)
    isSetupDir    = maybe False ("/setup" `isSuffixOf`)
    isSetupSource = (`elem` ["Setup.hs", "Setup.lhs"]) . takeFileName
buildingCabalSetup _ _ = False


getGhcjsSettings :: [String] -> IO ([String], GhcjsSettings)
getGhcjsSettings args
  | Left failure <- p = do
     hPutStrLn stderr =<< errMessage failure "ghcjs"
     exitWith (errExitCode failure)
  | Right gs1 <- p = do
     gs2 <- envSettings
     return (args', gs1 <> gs2)
  where
    (ga,args') = partition (\a -> any (`isPrefixOf` a) as) args
    p = execParserPure (prefs mempty) optParser' ga
    as = [ "--native-executables"
         , "--no-native"
         , "--no-js-executables"
         , "--log-commandline="
         , "--with-ghc="
         , "--debug"
         ]
    envSettings = GhcjsSettings <$> getEnvOpt "GHCJS_NATIVE_EXECUTABLES"
                                <*> getEnvOpt "GHCJS_NO_NATIVE"
                                <*> pure False
                                <*> getEnvMay "GHCJS_LOG_COMMANDLINE_NAME"
                                <*> getEnvMay "GHCJS_WITH_GHC"
                                <*> getEnvOpt "GHCJS_DEBUG"

optParser' :: ParserInfo GhcjsSettings
optParser' = info (helper <*> optParser) fullDesc

optParser :: Parser GhcjsSettings
optParser = GhcjsSettings
            <$> switch ( long "native-executables" )
            <*> switch ( long "no-native" )
            <*> switch ( long "no-js-executables" )
            <*> optStr ( long "log-commandline" )
            <*> optStr ( long "with-ghc" )
            <*> switch ( long "debug" )

optStr :: Mod OptionFields (Maybe String) -> Parser (Maybe String)
optStr m = nullOption $ value Nothing <> reader (Right . str)  <> m

main :: IO ()
main =
  do args <- getArgs
     (args0, settings) <- getGhcjsSettings args
     logCmd <- (|| isJust (gsLogCommandLine settings)) <$>
         getEnvOpt "GHCJS_LOG_COMMANDLINE"
     when logCmd $ do
         dir <- getAppUserDataDirectory "ghcjs"
         createDirectoryIfMissing True dir
         let filename = fromMaybe "cmd.log" (gsLogCommandLine settings)
         appendFile (dir </> filename) (intercalate " " args0 ++ "\n")
     let noNative = gsNoNative settings
         (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
         oneshot = "-c" `elem` args1
         mbMinusB | null minusB_args = Nothing
                  | otherwise = Just . drop 2 . last $ minusB_args
     handleCommandline settings args1 mbMinusB
     when (isNothing mbMinusB) checkIsBooted
     (argsS, _) <- parseStaticFlags (map noLoc args1)
     when (not noNative) $ do
       skipJs <- generateNative settings oneshot argsS args1 mbMinusB
       when skipJs exitSuccess
     runGhcSession mbMinusB $
       do sdflags0 <- getSessionDynFlags
          let sdflags1 = sdflags0 { verbosity = 1 }
          (dflags0, fileish_args, _) <- parseDynamicFlags sdflags1 $ ignoreUnsupported argsS
          let (hs_srcs, non_hs_srcs, js_objs, objs) = partition_args_js fileish_args
          dflags1 <- liftIO $ if isJust mbMinusB then return dflags0 else addPkgConf dflags0
          (dflags2, pkgs) <- liftIO (initPackages dflags1)
          base <- liftIO ghcjsDataDir
          _ <- setSessionDynFlags
               $ setGhcjsPlatform (gsDebug settings) js_objs base
               $ updateWays $ addWay' (WayCustom "js")
               $ setGhcjsSuffixes oneshot dflags2
          dflags3 <- getSessionDynFlags
          fixNameCache
          if oneshot || null hs_srcs
            then sourceErrorHandler $ do
                setSessionDynFlags $ dflags3 { ghcMode = OneShot }
                env <- getSession
                liftIO $ ghcjsOneShot env StopLn hs_srcs
                return ()
            else sourceErrorHandler $ do
              liftIO (Util.compilationProgressMsg dflags3 "generating JavaScript")
              targets <- mapM (uncurry guessTarget) hs_srcs
              setTargets targets
              s <- load LoadAllTargets
              when (failed s) (throw $ ExitFailure 1)

ignoreUnsupported :: [Located String] -> [Located String]
ignoreUnsupported =
  removeBy (`elem` unsup) .
  removeBy (\x -> any (`isPrefixOf` x) unsupPre)
    where
      removeBy :: (a -> Bool) -> [Located a] -> [Located a]
      removeBy g = filter (not . g . unLoc)
      unsup    = ["--make", "-c", "-M"] -- remove these arguments
      unsupPre = ["-H"]           -- remove arguments that start with these


handleCommandline :: GhcjsSettings -> [String] -> Maybe String -> IO ()
handleCommandline settings args minusBargs
    | Just act <- lookupAct = act >> exitSuccess
    | otherwise             = do
        booting <- getEnvOpt "GHCJS_BOOTING"
        when (booting && any ("Cabal-" `isPrefixOf`) args) bootstrapFallback
   where
     lookupAct = getFirst . mconcat . map (First . (`lookup` acts)) $ args
     acts :: [(String, IO ())]
     acts = [ ("--supported-languages",
                mapM_ putStrLn (supportedLanguagesAndExtensions ++
                  ["JavaScriptFFI", "NoJavaScriptFFI"]))
            , ("--version", printVersion)
            , ("--numeric-ghcjs-version", printNumericVersion)
                 -- the GHC version this was compiled with
            , ("--numeric-ghc-version", putStrLn getGhcCompilerVersion)
                 -- the GHC version again, for better Cabal support
            , ("--numeric-version", putStrLn getGhcCompilerVersion)
            , ("--info", print =<< getCompilerInfo)
            , ("--print-libdir", putStrLn =<< getGlobalPackageInst)
            , ("--abi-hash", abiHash args minusBargs)
            , ("-M", generateDeps args minusBargs)
            , ("--print-rts", printRts)
            , ("--print-deps", printDeps args)
            , ("--print-obj", printObj args)
            , ("--show-iface", printIface args)
            , ("--install-executable", installExecutable args)
            ]

printVersion :: IO ()
printVersion = putStrLn $
  "The Glorious Glasgow Haskell Compilation System for JavaScript, version " ++
     getCompilerVersion ++ " (GHC " ++ getGhcCompilerVersion ++ ")"

printNumericVersion :: IO ()
printNumericVersion = putStrLn getCompilerVersion

modsumToInfo :: GhcMonad m => ModSummary -> m (Maybe ModuleInfo)
modsumToInfo ms = getModuleInfo (ms_mod ms)

collectDeps :: [ModuleInfo] -> [PackageId]
collectDeps mis = nub $ concatMap pkgs mis
    where
      pkgs mi = maybe [] (map fst . dep_pkgs . mi_deps) $ modInfoIface mi

printRts :: IO ()
printRts = TL.putStrLn (Gen2.rtsText False) >> exitSuccess


printDeps :: [String] -> IO ()
printDeps ["--print-deps", file] = Object.readDepsFile file >>= TL.putStrLn . Object.showDeps
printDeps _                    = putStrLn "usage: ghcjs --print-deps objfile" >> exitFailure

printObj :: [String] -> IO ()
printObj ["--print-obj", file] = Object.readObjectFile file >>= TL.putStrLn . Object.showObject
printObj _                     = putStrLn "usage: ghcjs --print-obj objfile" >> exitFailure

-- also generate native code, compile with regular GHC settings, but make sure
-- that generated files don't clash with ours
generateNative :: GhcjsSettings -> Bool -> [Located String] -> [String] -> Maybe String -> IO Bool
generateNative settings oneshot argsS args1 mbMinusB =
  runGhcSession mbMinusB $ do
      sdflags0 <- getSessionDynFlags
      let sdflags1 = sdflags0 { verbosity = 1 }
      (dflags0, fileish_args, _) <- parseDynamicFlags sdflags1 (ignoreUnsupported argsS)
      dflags1 <- liftIO $
                 if isJust mbMinusB
                 then return dflags0
                 else addPkgConf dflags0
      (dflags2, _) <- liftIO (initPackages dflags1)
      let (hs_srcs, non_hs_srcs, js_objs, objs) = partition_args_js fileish_args
          srcs     = hs_srcs ++ non_hs_srcs
          oneshot' = oneshot || null hs_srcs
          dflags3  = installNativeHooks $
              dflags2 { ldInputs = map (FileOption "") objs ++ ldInputs dflags2 }
          buildingSetup = buildingCabalSetup (map fst hs_srcs) dflags3
      if gsNativeExecutables settings || ghcLink dflags3 /= LinkBinary || buildingSetup
          then setSessionDynFlags dflags3
          else setSessionDynFlags $ dflags3 { ghcLink = NoLink
                                            , outputFile = Nothing
                                            }
      dfs <- getSessionDynFlags
      liftIO (writeIORef (canGenerateDynamicToo dfs) True)
      liftIO (Util.compilationProgressMsg dfs "generating native")
      if oneshot'
          then sourceErrorHandler $ do
            setSessionDynFlags $ dfs { ghcMode = OneShot }
            env <- getSession
            liftIO $ ghcjsOneShot env StopLn srcs
          else sourceErrorHandler $ do
            env <- getSession
            o_files <- mapM (\x -> liftIO $ compileFile env StopLn x) non_hs_srcs
            dflags4 <- GHC.getSessionDynFlags
            GHC.setSessionDynFlags $
                dflags4 { ldInputs = map (FileOption "") o_files ++ ldInputs dflags4 }
            targets <- mapM (uncurry guessTarget) hs_srcs
            setTargets targets
            success <- load LoadAllTargets
            when (failed success) (throw (ExitFailure 1))
      return buildingSetup

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

-- we might generate .hi files for a different bitness than native GHC,
-- make sure we can show then
printIface :: [String] -> IO ()
printIface ["--show-iface", iface] = do
     (argsS, _) <- parseStaticFlags $ map noLoc []
     runGhcSession Nothing $ do
       sdflags <- getSessionDynFlags
       base <- liftIO ghcjsDataDir
       setSessionDynFlags $ setGhcjsPlatform (gsDebug mempty) [] base sdflags
       env <- getSession
       liftIO $ showIface env iface
printIface _                       = putStrLn "usage: ghcjs --show-iface hifile"

-- | when booting GHCJS, we pretend to have the Cabal lib installed
--   call GHC to compile our Setup.hs
bootstrapFallback :: IO ()
bootstrapFallback = do
    ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_FALLBACK_GHC"
    getArgs >>= rawSystem ghc >>= exitWith -- run without GHCJS package args

generateDeps :: [String] -> Maybe String -> IO ()
generateDeps args mbMinusB = do
  (argsS, _) <- parseStaticFlags (map noLoc args)
  runGhcSession mbMinusB $
       do sdflags <- getSessionDynFlags
          (dflags0, fileish_args, _) <- parseDynamicFlags sdflags (ignoreUnsupported argsS)
          let dflags1 = dflags0 { ghcMode = MkDepend }
          dflags2 <- liftIO $
                        if isJust mbMinusB
                          then return dflags1
                          else addPkgConf dflags1
          (dflags3, _) <- liftIO (initPackages dflags2)
          let normal_fileish_paths = map (normalise . unLoc) fileish_args
              (srcs, _)            = partition_args normal_fileish_paths [] []
          setSessionDynFlags dflags3
          handleSourceError (\e -> do
            GHC.printException e
            liftIO $ exitWith (ExitFailure 1)) $ do
              doMkDependHS (map fst srcs)

abiHash :: [String] -> Maybe String -> IO ()
abiHash args minusB = do
  (argsS, _) <- parseStaticFlags $ map noLoc args
  runGhcSession minusB $ do
    sdflags <- getSessionDynFlags
    let sdflags' = sdflags { ghcMode = OneShot, ghcLink = LinkBinary }
    (dflags0, fileargs', _) <- parseDynamicFlags sdflags' $ ignoreUnsupported argsS
    dflags1 <- liftIO $ if isJust minusB then return dflags0 else addPkgConf dflags0
    (dflags2, pkgs) <- liftIO (initPackages dflags1)
    _ <- setSessionDynFlags (setDfOpts dflags2)
    abiHash' (map unLoc fileargs')

abiHash' :: [String] -> Ghc ()
abiHash' strs0 = do
 let strs = filter looksLikeModuleName strs0
 hsc_env <- getSession
 let dflags = hsc_dflags hsc_env

 liftIO $ do
  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname Nothing
         case r of
           Found _ m -> return m
           _error    -> throwGhcException $ CmdLineError $ showSDoc dflags $
                          cannotFindInterface dflags modname r

  mods <- mapM find_it strs
  let get_iface modl = loadUserInterface False (text "abiHash") modl
  ifaces <- initIfaceCheck hsc_env $ mapM get_iface mods
  bh <- openBinMem (3*1024) -- just less than a block
  put_ bh hiVersion
    -- package hashes change when the compiler version changes (for now)
    -- see #5328
  mapM_ (put_ bh . mi_mod_hash) ifaces
  f <- fingerprintBinMem bh

  putStrLn (showPpr dflags f)

installExecutable :: [String] -> IO ()
installExecutable ["--install-executable", "-from", from, "-to", to] =
                      putStrLn "installing executables not yet implemented"
installExecutable _ = putStrLn "usage: ghcjs --install-executable -from <from> -to <to>" >> exitFailure
