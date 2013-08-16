{-# LANGUAGE CPP,
             TypeFamilies,
             ScopedTypeVariables,
             TupleSections,
             OverloadedStrings #-}

module Main where

import           Config (cProjectVersion, cDYNAMIC_GHC_PROGRAMS)
import           GHC
import           Hooks
import           HscMain
import           TidyPgm (tidyProgram)
import           CoreToStg (coreToStg)
import           SimplStg (stg2stg)
import           UniqFM (eltsUFM)
import           DynFlags
import           Platform
import           ErrUtils (fatalErrorMsg'')
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
import           SysTools (touch, LinkDynLibHook(..))
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
import           Linker (locateLib', LocateLibHook(..), LibrarySpec(..))
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
import           System.Environment (getArgs, getEnv)
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process (rawSystem)

import           Compiler.Info
import           Compiler.Variants
import           Compiler.Hooks

import qualified Gen2.Utils     as Gen2
import qualified Gen2.Generator as Gen2
import qualified Gen2.Linker    as Gen2
import qualified Gen2.Rts       as Gen2
import qualified Gen2.PrimIface as Gen2
import qualified Gen2.Foreign   as Gen2
import qualified Gen2.Object    as Object

import           Finder (findImportedModule, cannotFindInterface)


import Debug.Trace

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
                                <*> pure False

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
     libDir <- getGlobalPackageBase
     when (isNothing mbMinusB) checkIsBooted
     (argsS, _) <- parseStaticFlags (map noLoc args1)
     booting <- getEnvOpt "GHCJS_BOOTING"
     when (not noNative || (booting && any (".c" `isSuffixOf`) args1)) $
       generateNative settings oneshot argsS args1 mbMinusB
     errorHandler
        fatalMessager
        defaultFlushOut
        $ runGhc (mbMinusB `mplus` Just libDir) $
       do sdflags0 <- getSessionDynFlags
          trace ("mbMinusB, libDdir = " ++ show (mbMinusB, libDir)) $ return ()
          let sdflags1 = sdflags0 { verbosity = 1 }
          (dflags0, fileish_args, _) <- parseDynamicFlags sdflags1 $ ignoreUnsupported argsS
          let normal_fileish_paths    = map (normalise . unLoc) fileish_args
              (srcs, objs0)           = partition_args normal_fileish_paths [] []
              (js_objs, objs)         = partition isJsFile objs0
              (hs_srcs, non_hs_srcs)  = partition haskellish srcs
          traceShow (srcs, objs0) $ return ()
          dflags1 <- liftIO $
                        if booting
                          then return (gopt_set dflags0 Opt_ForceRecomp)
                          else if isJust mbMinusB
                                 then return dflags0
                                 else addPkgConf dflags0
          (dflags2, pkgs) <- liftIO (initPackages dflags1)
          liftIO (doPackageFallback pkgs args1)
          base <- liftIO ghcjsDataDir
          trace ("ghcjsDataDir = " ++ base) $ return ()
          _ <- setSessionDynFlags $ setGhcjsPlatform settings js_objs base $ updateWays $ addWay' (WayCustom "js") $
               setGhcjsSuffixes oneshot dflags2
          dflags3 <- getSessionDynFlags
          fixNameCache
          if oneshot || null hs_srcs
            then sourceErrorHandler $ do
                setSessionDynFlags $ dflags3 { ghcMode = OneShot }
                env <- getSession
                liftIO $ oneShot env StopLn hs_srcs
                return ()
            else sourceErrorHandler $ do
              liftIO (Gen2.compilationProgressMsg dflags3 "generating JavaScript")
              targets <- mapM (uncurry guessTarget) hs_srcs
              setTargets targets
              s <- load LoadAllTargets
              when (failed s) (throw $ ExitFailure 1)

isJsFile :: FilePath -> Bool
isJsFile = (==".js") . takeExtension

addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  base <- getGlobalPackageBase
  return $ df { extraPkgConfs = const [PkgConfFile db1, PkgConfFile db2]
              , includePaths  = (base ++ "/include") : includePaths df -- fixme: shouldn't be necessary if builtin_rts has this in its include-dirs?
              }
  where
    isNotGlobal GlobalPkgConf = False
    isNotGlobal _ = True
    isNotUser UserPkgConf = False
    isNotUser _ = True

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
    | otherwise             = return ()
   where
     lookupAct = getFirst . mconcat . map (First . (`lookup` acts)) $ args
     acts :: [(String, IO ())]
     acts = [ ("--supported-languages",
                mapM_ putStrLn (supportedLanguagesAndExtensions ++
                  ["JavaScriptFFI", "NoJavaScriptFFI"]))
            , ("--version", printVersion)
            , ("--numeric-version", printNumericVersion)
                 -- the ghc version this was compiled with
            , ("--numeric-ghc-version", putStrLn getGhcCompilerVersion)
            , ("--info", print =<< getCompilerInfo)
            , ("--print-libdir", putStrLn =<< getGlobalPackageInst)
            , ("--abi-hash", abiHash args minusBargs)
            , ("-M", generateDeps args minusBargs)
            , ("--print-rts", printRts)
            , ("--print-deps", printDeps args)
            , ("--print-obj", printObj args)
            , ("--show-iface", printIface args)
            ]

printVersion :: IO ()
printVersion = putStrLn $
  "The Glorious Glasgow Haskell Compilation System for JavaScript, version " ++
     getCompilerVersion ++ " (GHC " ++ getGhcCompilerVersion ++ ")"

printNumericVersion :: IO ()
printNumericVersion = do
  booting <- getEnvOpt "GHCJS_BOOTING"
  if booting then putStrLn getGhcCompilerVersion
             else putStrLn getCompilerVersion

-- | make sure we don't show panic messages with the "report GHC bug" text, since
--   those are probably our fault.
errorHandler :: (ExceptionMonad m, MonadIO m)
                    => FatalMessager -> FlushOut -> m a -> m a
errorHandler fm (FlushOut flushOut) inner =
  ghandle (\exception -> liftIO $ do
           flushOut
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fatalErrorMsg'' fm (show ioe)
                _ -> case fromException exception of
                     Just UserInterrupt -> exitWith (ExitFailure 1)
                     Just StackOverflow ->
                         fatalErrorMsg'' fm "stack overflow: use +RTS -K<size> to increase it"
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> liftIO $ throwIO ex
                          _ -> case fromException exception of
                               Just (Panic str) -> fatalErrorMsg'' fm str
                               _ -> fatalErrorMsg'' fm (show exception)
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

sourceErrorHandler m = handleSourceError (\e -> do
  GHC.printException e
  liftIO $ exitWith (ExitFailure 1)) m

fatalMessager :: String -> IO ()
fatalMessager str = do
  hPutStrLn stderr str
  dumpArgs <- getEnvOpt "GHCJS_ERROR_ARGUMENTS"
  when dumpArgs $ do
    args <- getArgs
    hPutStrLn stderr (str ++ "\n--- arguments: \n" ++ unwords args ++ "\n---\n")
  exitWith (ExitFailure 1)

ghcjsCompileModule :: GhcjsSettings -> HscEnv -> CgGuts -> ModSummary
                   -> FilePath -> IO FilePath
ghcjsCompileModule settings env core mod output
  | WayDyn `elem` ways dflags = do
      B.writeFile output "GHCJS dummy output"
      return output
  | otherwise = do
      core_binds <- corePrepPgm dflags env (cg_binds core) (cg_tycons core)
      stg <- coreToStg dflags (cg_module core) core_binds
      (stg', _ccs) <- stg2stg dflags (cg_module core) stg
      let obj = variantRender gen2Variant (gsDebug settings) dflags stg' (cg_module core)
      B.writeFile output obj
      return output
    where
      dflags = hsc_dflags env

modsumToInfo :: GhcMonad m => ModSummary -> m (Maybe ModuleInfo)
modsumToInfo ms = getModuleInfo (ms_mod ms)

collectDeps :: [ModuleInfo] -> [PackageId]
collectDeps mis = nub $ concatMap pkgs mis
    where
      pkgs mi = maybe [] (map fst . dep_pkgs . mi_deps) $ modInfoIface mi

printRts :: IO ()
printRts = putStrLn Gen2.rtsStr >> exitSuccess


printDeps :: [String] -> IO ()
printDeps ["--print-deps", file] = Object.readDepsFile file >>= TL.putStrLn . Object.showDeps
printDeps _                    = putStrLn "usage: ghcjs --print-deps objfile"

printObj :: [String] -> IO ()
printObj ["--print-obj", file] = Object.readObjectFile file >>= TL.putStrLn . Object.showObject
printObj _                     = putStrLn "usage: ghcjs --print-obj objfile"


setOpt = gopt_set
unsetOpt = gopt_unset

-- add some configs
setDfOpts :: DynFlags -> DynFlags
setDfOpts df = foldl' setOpt (foldl' unsetOpt df unsetList) setList
  where
    setList = []
    unsetList = [Opt_SplitObjs]

-- | configure the GHC API for building 32 bit JavaScript code
setGhcjsPlatform :: GhcjsSettings -> [FilePath] -> FilePath -> DynFlags -> DynFlags
setGhcjsPlatform set js_objs basePath df
  = addPlatformDefines basePath
      $ setDfOpts
      $ setGhcjsHooks (gsDebug set) js_objs
      $ df { settings = settings' }
  where
    settings' = (settings df) { sTargetPlatform    = ghcjsPlatform
                              , sPlatformConstants = ghcjsPlatformConstants
                              }
    ghcjsPlatform = (sTargetPlatform (settings df))
       { platformArch     = ArchJavaScript
       , platformWordSize = 4
       }
    ghcjsPlatformConstants = (sPlatformConstants (settings df))
       { pc_WORD_SIZE       = 4
       , pc_DOUBLE_SIZE     = 8
       , pc_CINT_SIZE       = 4
       , pc_CLONG_SIZE      = 4
       , pc_CLONG_LONG_SIZE = 8
       , pc_WORDS_BIGENDIAN = False
       }


addLogActionFilter :: DynFlags -> DynFlags
addLogActionFilter df = df { log_action = act }
   where
     act :: LogAction
     act dfs severity span style doc
       | isSuppressed span severity (showSDocOneLine dfs doc) = return ()
       | otherwise = log_action df dfs severity span style doc

-- suppress some GHC API output where it would print the wrong thing
isSuppressed :: SrcSpan -> Severity -> String -> Bool
isSuppressed span _ _
  | span == Gen2.ghcjsSrcSpan = False -- do not suppress our own messages
isSuppressed _ SevOutput txt
  | "Linking " `isPrefixOf` txt = True -- would print our munged name
isSuppressed _ _ _ = False

installDriverHooks :: GhcjsSettings -> DynFlags -> DynFlags
installDriverHooks settings df = df { hooks = hooks' }
  where hooks' = insertHook GhcPrimIfaceHook Gen2.ghcjsPrimIface
               $ insertHook RunPhaseHook (runGhcjsPhase settings)
               $ hooks df


installNativeHooks :: GhcjsSettings -> DynFlags -> DynFlags
installNativeHooks settings df =
  Gen2.installForeignHooks False $ df { hooks = hooks' }
    where hooks' = insertHook PackageHsLibsHook ghcjsPackageHsLibs
                 $ insertHook LocateLibHook ghcjsLocateLib
                 $ hooks df

runGhcjsPhase :: GhcjsSettings
              -> PhasePlus -> FilePath -> DynFlags
              -> CompPipeline (PhasePlus, FilePath)

runGhcjsPhase settings (HscOut src_flavour mod_name result) _ dflags = do

        location <- getLocation src_flavour mod_name
        setModLocation location

        let o_file = ml_obj_file location -- The real object file
            hsc_lang = hscTarget dflags
            next_phase = hscPostBackendPhase dflags src_flavour hsc_lang

        case result of
            HscNotGeneratingCode ->
                return (RealPhase next_phase,
                        panic "No output filename from Hsc when no-code")
            HscUpToDate ->
                do liftIO $ touchObjectFile dflags o_file
                   -- The .o file must have a later modification date
                   -- than the source file (else we wouldn't get Nothing)
                   -- but we touch it anyway, to keep 'make' happy (we think).
                   return (RealPhase StopLn, o_file)
            HscUpdateBoot ->
                do -- In the case of hs-boot files, generate a dummy .o-boot
                   -- stamp file for the benefit of Make
                   liftIO $ touchObjectFile dflags o_file
                   return (RealPhase next_phase, o_file)
            HscRecomp cgguts mod_summary
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    outputFilename <- liftIO $ ghcjsCompileModule settings hsc_env' cgguts mod_summary output_fn

                    return (RealPhase next_phase, outputFilename)
-- skip these, but copy the result
runGhcjsPhase _ (RealPhase ph) input dflags
  | Just next <- lookup ph skipPhases = do
    output <- phaseOutputFilename next
    liftIO (copyFile input output)
    when (ph == As) (liftIO $ doFakeNative dflags (dropExtension output))
    return (RealPhase next, output)
  where
    skipPhases = [ (CmmCpp, Cmm), (Cmm, As), (As, StopLn) ]

-- otherwise use default
runGhcjsPhase _ p input dflags = runPhase p input dflags

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

-- ghcjs builds for a strange platform: like 32 bit
-- instead of letting autoconf doing the defines, we override them here
-- and try to get our own includes included instead of the library ones
addPlatformDefines :: FilePath -> DynFlags -> DynFlags
addPlatformDefines baseDir df = df { settings = settings1
                                   , includePaths = includeDir : includePaths df
                                   }
  where
    includeDir = baseDir ++ "/include"
    settings0 = settings df
    settings1 = settings0 { sOpt_P = ("-I" ++ includeDir) : map ("-D"++) defs ++ sOpt_P settings0 }
    defs = [ "__GHCJS__"
           , "__GHCAUTOCONF_H__=1"
           , "__GHCCONFIG_H__=1"
           , "SIZEOF_CHAR=1"
           , "ALIGNMENT_CHAR=1"
           , "SIZEOF_UNSIGNED_CHAR=1"
           , "ALIGNMENT_UNSIGNED_CHAR=1"
           , "SIZEOF_SHORT=2"
           , "ALIGNMENT_SHORT=2"
           , "SIZEOF_UNSIGNED_SHORT=2"
           , "ALIGNMENT_UNSIGNED_SHORT=2"
           , "SIZEOF_INT=4"
           , "ALIGNMENT_INT=4"
           , "SIZEOF_UNSIGNED_INT=4"
           , "ALIGNMENT_UNSIGNED_INT=4"
           , "SIZEOF_LONG=4"
           , "ALIGNMENT_LONG=4"
           , "SIZEOF_UNSIGNED_LONG=4"
           , "ALIGNMENT_UNSIGNED_LONG=4"
           , "HAVE_LONG_LONG=1"
           , "SIZEOF_LONG_LONG=8"
           , "ALIGNMENT_LONG_LONG=8"
           , "SIZEOF_UNSIGNED_LONG_LONG=8"
           , "ALIGNMENT_UNSIGNED_LONG_LONG=8"
           , "SIZEOF_VOID_P=4"
           , "ALIGNMENT_VOID_P=4"
           , "SIZEOF_DOUBLE=8"
           , "ALIGNMENT_DOUBLE=8"
           , "SIZEOF_FLOAT=4"
           , "ALIGNMENT_FLOAT=4"
           ]

runGhcSession mbMinusB a = do
     libDir <- getGlobalPackageBase
     errorHandler
        fatalMessager
        defaultFlushOut $
          runGhc (mbMinusB `mplus` Just libDir) $ a

-- also generate native code, compile with regular GHC settings, but make sure
-- that generated files don't clash with ours
generateNative :: GhcjsSettings -> Bool -> [Located String] -> [String] -> Maybe String -> IO ()
generateNative settings oneshot argsS args1 mbMinusB =
  runGhcSession mbMinusB $ do
       do   sdflags0 <- getSessionDynFlags
            let sdflags1 = sdflags0 { verbosity = 1 }
                oneshot = "-c" `elem` args1
            (dflags0, fileish_args, _) <- parseDynamicFlags sdflags1 (ignoreUnsupported argsS)
            dflags1 <- liftIO $
                          if isJust mbMinusB
                            then return dflags0
                            else addPkgConf dflags0
            (dflags2, _) <- liftIO (initPackages dflags1)
            let normal_fileish_paths   = map (normalise . unLoc) fileish_args
                (srcs, objs0)          = partition_args normal_fileish_paths [] []
                (js_objs, objs)        = partition isJsFile objs0
                (hs_srcs, non_hs_srcs) = partition haskellish srcs
                oneshot'               = oneshot || null hs_srcs
                dflags3 = installNativeHooks settings $
                   dflags2 { ldInputs = map (FileOption "") objs ++ ldInputs dflags2 }
            if gsNativeExecutables settings || ghcLink dflags3 /= LinkBinary
              then setSessionDynFlags dflags3
              else setSessionDynFlags $ dflags3 { ghcLink = NoLink
                                                , outputFile = Nothing
                                                }
            dfs <- getSessionDynFlags
            liftIO (writeIORef (canGenerateDynamicToo dfs) True)
            liftIO (Gen2.compilationProgressMsg dfs "generating native")
            if oneshot'
              then sourceErrorHandler $ do
                setSessionDynFlags $ dfs { ghcMode = OneShot }
                env <- getSession
                liftIO $ oneShot env StopLn srcs
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
                       "please install the GHCJS core libraries. See README for details"
    exitWith (ExitFailure 1)

-- we might generate .hi files for a different bitness than native GHC,
-- make sure we can show then
printIface :: [String] -> IO ()
printIface ["--show-iface", iface] = do
     (argsS, _) <- parseStaticFlags $ map noLoc []
     runGhcSession Nothing $ do
       sdflags <- getSessionDynFlags
       base <- liftIO ghcjsDataDir
       setSessionDynFlags $ setGhcjsPlatform mempty [] base sdflags
       env <- getSession
       liftIO $ showIface env iface
printIface _                       = putStrLn "usage: ghcjs --show-iface hifile"

mkGhcjsOutput :: String -> String
mkGhcjsOutput "" = ""
mkGhcjsOutput file = replaceExtension file ('.':mkGhcjsSuf ext)
  where
    ext = tail $ takeExtension file

mkGhcjsSuf :: String -> String
mkGhcjsSuf "o"      = "js_o"
mkGhcjsSuf "hi"     = "js_hi"
mkGhcjsSuf "dyn_o"  = "js_dyn_o"
mkGhcjsSuf "dyn_hi" = "js_dyn_hi"
mkGhcjsSuf xs       = "js_" ++ xs -- is this correct?

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

doFakeNative :: DynFlags -> FilePath -> IO ()
doFakeNative df base = do
  b <- getEnvOpt "GHCJS_FAKE_NATIVE"
  when b $ do
    mapM_ backupExt ["hi", "o", "dyn_hi", "dyn_o"]
    mapM_ touchExt  ["hi", "o", "dyn_hi", "dyn_o"]
  where
    backupExt ext = copyNoOverwrite (base ++ ".backup_" ++ ext) (base ++ "." ++ ext)
    touchExt  ext = touchFile df (base ++ "." ++ ext)

touchFile :: DynFlags -> FilePath -> IO ()
touchFile df file = do
--  putStrLn ("touchFile: " ++ file)
  e <- doesFileExist file
  when e (touch df "keep build system happy" file)

copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
--  putStrLn ("copyNoOverWrite: " ++ from ++ " -> " ++ to)
  ef <- doesFileExist from
  et <- doesFileExist to
  when (ef && not et) (copyFile from to)

-- | generate native code only from native compiler for these packages
--   used to support build system
doPackageFallback :: [PackageId] -> [String] -> IO ()
doPackageFallback pkgs args
  | any isFallbackPkg pkgs = do
    ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_FALLBACK_GHC"
    getArgs >>= rawSystem ghc >>= exitWith -- run without GHCJS package args
  | otherwise = return ()
  where
    isFallbackPkg pkgid =
      let pkgname = takeWhile (/='-') (packageIdString pkgid)
      in  pkgname `elem` ["Cabal"]


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


haskellish :: (String, Maybe Phase) -> Bool
haskellish (f,Nothing) =
  looksLikeModuleName f || isHaskellUserSrcFilename f || '.' `notElem` f
haskellish (_,Just phase) =
  phase `notElem` [As, Cc, Cobjc, Cobjcpp, CmmCpp, Cmm, StopLn]

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || '.' `notElem` m

