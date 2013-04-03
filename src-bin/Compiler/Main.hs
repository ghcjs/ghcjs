{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables, PackageImports, TupleSections #-}
module Main where

import           Paths_ghcjs

import qualified GHC.Paths
import           GHC
import           HscMain (hscSimplify)
import           TidyPgm (tidyProgram)
import           CoreToStg (coreToStg)
import           SimplStg (stg2stg)
import           DynFlags
import           Platform
import           CorePrep (corePrepPgm)
import           DriverPhases (HscSource (HsBootFile), Phase (..), isHaskellSrcFilename, isSourceFilename)
import           DriverPipeline (oneShot)
import           DsMeta (templateHaskellNames)
import           Exception
import           HscTypes (ModGuts, CgGuts (..), HscEnv (..), Dependencies (..), NameCache (..), isBootSummary, mkSrcErr, ModGuts(..))
import           IfaceEnv (initNameCache)
import           LoadIface
import           Outputable (showPpr)
import           Packages (initPackages)
import           Panic
import           Module
import           PrelInfo (wiredInThings)
import           PrelNames (basicKnownKeyNames)
import           PrimOp (allThePrimOps)
import           SysTools (touch)
import           MkIface
import           GhcMonad
import           Digraph
import           Binary (fingerprintBinMem, openBinMem, put_)
import           Constants (hiVersion)
import           TcRnMonad (initIfaceCheck)
import           Util (looksLikeModuleName)
import           Outputable

import           Control.Applicative
import           Control.Monad
import           Data.Char (toLower)
import           Data.IORef (modifyIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe

import           System.Environment (getArgs, getEnv)
import           System.Exit(ExitCode(..), exitWith)

#ifdef GHCJS_PACKAGE_IMPORT
#define GHCJS "ghcjs"
#else
#define GHCJS "ghcjs"
#endif

import           GHCJS Compiler.Info
import           GHCJS Compiler.Variants
import qualified GHCJS GHCJSMain
import           MonadUtils (MonadIO(..))
import           System.FilePath (takeExtension, dropExtension, addExtension, replaceExtension, (</>))
import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory, doesFileExist, copyFile)
import qualified Control.Exception as Ex

import           Control.Monad (when, mplus, forM, forM_)
import           System.Exit (exitSuccess)
import           System.Process (rawSystem)
import           System.IO
import           Data.Monoid (mconcat, First(..))
import           Data.List (isSuffixOf, isPrefixOf, tails, partition, nub, intercalate, foldl')
import           Data.Maybe (isJust, fromMaybe, catMaybes, isNothing)

import qualified Data.ByteString.Base16 as B16
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8  as C8
import qualified Data.Serialize as C
import qualified Gen2.Generator as Gen2
import qualified Gen2.Linker    as Gen2
import qualified Gen2.Rts       as Gen2
import           Gen2.PrimIface as Gen2

-- debug
import           Finder
import           PrelNames
import FastString

main :: IO ()
main =
  do args0 <- getArgs
     logCmd <- getEnvOpt "GHCJS_LOG_COMMANDLINE"
     when logCmd $ do
         dir <- getAppUserDataDirectory "ghcjs"
         createDirectoryIfMissing True dir
         filename <- fromMaybe "cmd.log" <$>
                         getEnvMay "GHCJS_LOG_COMMANDLINE_NAME"
         appendFile (dir </> filename) (intercalate " " args0 ++ "\n")

     noNative <- getEnvOpt "GHCJS_NO_NATIVE"

     let (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
         oneshot = "-c" `elem` args1
         mbMinusB | null minusB_args = Nothing
                  | otherwise = Just . drop 2 . last $ minusB_args
     handleCommandline args1 mbMinusB
     libDir <- getGlobalPackageBase
     when (isNothing mbMinusB) checkIsBooted
     (argsS, _) <- parseStaticFlags $ map noLoc args1

     if noNative
            then liftIO (putStrLn "skipping native code")
            else liftIO $ do
--              putStrLn "generating native code"
              generateNative oneshot argsS args1 mbMinusB
     errorHandler
        fatalMessager
        defaultFlushOut
        $ runGhc (mbMinusB `mplus` Just libDir) $
       do sdflags <- getSessionDynFlags
          let sdflags' = sdflags { ghcMode = if oneshot then OneShot else CompManager
                                 , ghcLink = NoLink
                                 , hscTarget = HscAsm
                                 }
          (dflags0, fileargs', _) <- parseDynamicFlags sdflags' $ ignoreUnsupported argsS
          dflags1 <- liftIO $ if isJust mbMinusB then return dflags0 else addPkgConf dflags0
          (dflags2, pkgs) <- liftIO (initPackages dflags1)
          liftIO (doPackageFallback pkgs args1)
          base <- liftIO ghcjsDataDir
          let way = WayCustom "js"
          _ <- setSessionDynFlags $ setDfOpts $ setGhcjsPlatform base $
               dflags2 { objectSuf = "js_o"
                       , hiSuf     = "js_hi"
                       , buildTag   = mkBuildTag [way]
                       , ways       = [way]
                       , outputFile = fmap mkGhcjsOutput (outputFile dflags2)
                       , outputHi   = fmap mkGhcjsOutput (outputHi dflags2)
                       , hscOutName = mkGhcjsOutput (hscOutName dflags2)
                       }
          let (jsArgs, fileargs) = partition isJsFile (map unLoc fileargs')
          fixNameCache
          if all isBootFilename fileargs
            then sourceErrorHandler $ do
--              liftIO $ putStrLn "doing one shot"
              env <- getSession
              liftIO $ oneShot env StopLn (map (,Nothing) fileargs)
            else sourceErrorHandler $ do
              mtargets <- catchMaybe $ mapM (flip guessTarget Nothing) fileargs
              case mtargets of
                Nothing      -> do
--                          liftIO (putStrLn "falling back")
                          when (not noNative) (liftIO $ fallbackGhc False False args1)
                Just targets -> do
--                          liftIO $ putStrLn "generating code myself"
                          setTargets targets
                          origMgraph <- getModuleGraph
--                          liftIO $ print $ map (showPpr dflags2 . ms_mod) origMgraph
--                          _ <- load LoadAllTargets
                          mgraph0 <- reverse <$> depanal [] False
                          let mgraph = flattenSCCs $ topSortModuleGraph False mgraph0 Nothing
--                          liftIO $ print $ map (showPpr dflags2 . ms_mod) mgraph
                          if oneshot
                            then mapM_ compileModSummary (take (length targets) mgraph)
                            else mapM_ compileModSummary mgraph
                          dflags3 <- getSessionDynFlags
                          case ghcLink sdflags of
                            LinkBinary -> when (not oneshot) (buildExecutable dflags3 jsArgs) >> touchOutputFile
                            LinkDynLib -> return ()
                            _          -> touchOutputFile



isBootFilename :: FilePath -> Bool
isBootFilename fn = any (`isSuffixOf` fn) [".hs-boot", ".lhs-boot"]

catchMaybe a = (fmap Just a) `gcatch` \(_::Ex.SomeException) -> return Nothing

isJsFile :: FilePath -> Bool
isJsFile = (==".js") . takeExtension

addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  base <- getGlobalPackageBase
  return $ df {
               extraPkgConfs = ([PkgConfFile db1, PkgConfFile db2]++)
             , includePaths  = (base ++ "/include") : includePaths df -- fixme: shouldn't be necessary if builtin_rts has this in its include-dirs?
             }

pkgConfArgs :: IO [String]
pkgConfArgs = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  return $ map ("-package-db"++) [db1,db2]

ignoreUnsupported :: [Located String] -> [Located String]
ignoreUnsupported =
  removeBy (`elem` unsup) .
  removeBy (\x -> any (x `isPrefixOf`) unsupPre)
    where
      removeBy :: (a -> Bool) -> [Located a] -> [Located a]
      removeBy g = filter (not . g . unLoc)
      unsup    = ["--make", "-c"] -- remove these arguments
      unsupPre = ["-H"]           -- remove arguments that start with these


handleCommandline :: [String] -> Maybe String -> IO ()
handleCommandline args minusBargs
    | "-c" `elem` args      = handleOneShot args
    | Just act <- lookupAct = act >> exitSuccess
    | otherwise             = return ()
   where
     lookupAct = getFirst . mconcat . map (First . (`lookup` acts)) $ args
--     unsupported xs = putStrLn (xs ++ " is currently unsupported") >> exitFailure
     acts :: [(String, IO ())]
     acts = [ ("--supported-languages", mapM_ putStrLn (supportedLanguagesAndExtensions++
                                          ["JavaScriptFFI", "NoJavaScriptFFI"]))
            , ("--numeric-version", fallbackGhc False True args) -- putStrLn getCompilerVersion)
            , ("--info", print =<< getCompilerInfo)
            , ("--print-libdir", putStrLn =<< getLibDir)
            , ("--abi-hash", abiHash args minusBargs) -- fallbackGhc False True args)
            , ("-M", fallbackGhc False True args)
            , ("--print-rts", printRts)
            , ("--print-ji", printJi args)
            , ("--show-iface", printIface args)
            ]

handleOneShot :: [String] -> IO ()
handleOneShot args | fallback  = fallbackGhc True True args >> exitSuccess
                   | otherwise = return ()
    where
      fallback = any isFb (tails args)
      isFb ({- "-c": -} c:_) = any (`isSuffixOf` c) [".c", ".cmm"]
      isFb _          = False

-- | make sure we don't show panic messages with the "report GHC bug" text, since
--   those are probably our fault.
errorHandler :: (ExceptionMonad m, MonadIO m)
             => FatalMessager -> FlushOut -> m a -> m a
errorHandler fm fo inner = defaultErrorHandler fm fo (convertError inner)
  where
    convertError m = handleGhcException (\ge ->
                       throwGhcException $
                         case ge of
                           Panic str -> ProgramError str
                           x         -> x
                     ) m

sourceErrorHandler m = handleSourceError (\e -> do
  GHC.printException e
  liftIO $ exitWith (ExitFailure 1)) m

fatalMessager :: String -> IO ()
fatalMessager str = do
  args <- getArgs
  hPutStrLn stderr (str ++ "\n--- arguments: \n" ++ unwords args ++ "\n---\n")
  exitWith (ExitFailure 1)

{-
   call GHC for things that we don't handle internally

   fixme: either remove this hack or properly check that the version of the called ghc is the expected one
   GHCJS_FALLBACK_GHC is the location of the ghc executable
   if GHCJS_FALLBACK_PLAIN is set, all arguments are passed through verbatim
   to the fallback ghc, including -B
-}
fallbackGhc :: Bool -> Bool -> [String] -> IO ()
fallbackGhc isNative nonHaskell args = do
  pkgargs <- pkgConfArgs
  ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_FALLBACK_GHC"
  plain <- getEnvOpt "GHCJS_FALLBACK_PLAIN"
  args' <- if plain then getArgs else return args
  noNative <- getEnvOpt "GHCJS_NO_NATIVE"
  if isNative
    then when (not noNative || nonHaskell) (void $ rawSystem ghc $ pkgargs ++ args') -- ++ ["-hisuf", "native_hi"])
    else void $ rawSystem ghc $ pkgargs ++ args' ++ ["-osuf", "js_o"]
  return ()

-- why doesn't GHC pick up .lhs-boot as boot? are we loading it wrong?
isBootModSum :: ModSummary -> Bool
isBootModSum ms = isBootSummary ms || (maybe False ("-boot"`isSuffixOf`).ml_hs_file.ms_location $ ms)

compileModSummary :: GhcMonad m => ModSummary -> m ()
compileModSummary mod
  | isBootModSum mod = liftIO $ putStrLn $ concat ["Skipping boot ", name]
  | otherwise =
       do liftIO $ putStrLn $ concat ["Compiling ", name]
          desugaredMod <- desugaredModuleFromModSummary mod
          dsm2 <- loadModule (noCode desugaredMod)
          writeDesugaredModule desugaredMod -- dsm2 -- desugaredMod
  where name = moduleNameString . moduleName . ms_mod $ mod
        mod' = mod { ms_hspp_opts = (ms_hspp_opts mod) { hscTarget = HscAsm } }

noCode (DesugaredModule (TypecheckedModule (ParsedModule ms  ps esf) rs tcs cmi is) cm) =
       (DesugaredModule (TypecheckedModule (ParsedModule ms' ps esf) rs tcs cmi is) cm)
  where
    ms' = ms { ms_hspp_opts = (ms_hspp_opts ms) { hscTarget = HscNothing } }

desugaredModuleFromModSummary :: GhcMonad m => ModSummary -> m DesugaredModule
desugaredModuleFromModSummary =
  parseModule >=> typecheckModule >=> desugarModule

-- ioMsgMaybe :: GhcMonad m => IO (Messages, Maybe a) -> m a -- Hsc a
ioMsgMaybe ioA = do
    ((warns,errs), mb_r) <- liftIO ioA
--    logWarnings warns
    case mb_r of
        Nothing -> liftIO . throwIO . mkSrcErr $ errs -- throwErrors errs
        Just r  -> return r -- ASSERT( isEmptyBag errs ) return r

writeDesugaredModule :: GhcMonad m => DesugaredModule -> m ()
writeDesugaredModule mod =
  do env <- getSession
     let mod_guts0 = coreModule mod
         mb_old_iface = Nothing -- fixme?
     mod_guts1 <- liftIO $ hscSimplify env mod_guts0
     (tidyCore, details) <- liftIO $ tidyProgram env mod_guts1
     (iface, no_change) <- ioMsgMaybe $ mkIface env mb_old_iface details mod_guts1
     liftIO $ writeIfaceFile dflags ifaceFile iface
     versions <- liftIO $ forM variants $ \variant -> do
          (program, meta) <- liftIO $ concreteJavascriptFromCgGuts dflags env tidyCore variant
          let outputFile = addExtension outputBase (variantExtension variant)
          putStrLn $ concat ["Writing module ", name, " (", outputFile, ")"]
          B.writeFile outputFile program
          case variantMetaExtension variant of
            Nothing -> return ()
            Just mext -> B.writeFile (addExtension outputBase mext) meta
          return (variant, program, meta)
     df <- getSessionDynFlags
     liftIO $ doFakeNative df outputBase
--     liftIO $ writeCachedFiles dflags outputBase versions
  where
    mod_summary = pm_mod_summary . tm_parsed_module . dm_typechecked_module $ mod
    ifaceFile   = ml_hi_file (ms_location mod_summary)
    outputBase = dropExtension (ml_hi_file . ms_location $ mod_summary)
    name = moduleNameString . moduleName . ms_mod $ mod_summary
    dflags = ms_hspp_opts mod_summary
{-
cgGutsFromModGuts :: GhcMonad m => ModGuts -> m CgGuts
cgGutsFromModGuts guts =
  do hscEnv <- getSession
     simplGuts <- liftIO $ hscSimplify hscEnv guts
     (cgGuts, _) <- liftIO $ tidyProgram hscEnv simplGuts
     return cgGuts
-}

concreteJavascriptFromCgGuts :: DynFlags -> HscEnv -> CgGuts -> Variant -> IO (ByteString, ByteString)
concreteJavascriptFromCgGuts dflags env core variant =
  do core_binds <- corePrepPgm dflags
                               env
                               (cg_binds core)
                               (cg_tycons $ core)
     stg <- coreToStg dflags core_binds
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
     return (variantRender variant dflags stg' (cg_module core))

{-
  with -o x, ghcjs links all required functions into an executable
  bundle, which is a directory x.jsexe, rather than a filename

  if the executable is built with cabal, it also writes
  the file, and the executable bundle to the cache, so that
  cabaljs can install the executable
-}

buildExecutable :: GhcMonad m => DynFlags -> [FilePath] -> m ()
buildExecutable df linkedFiles = do
--  case outputFile df of
--    Just file -> liftIO $ writeFile file "ghcjs generated executable"
--    Nothing   -> return ()
  graph <- fmap hsc_mod_graph $ getSession
  ifaces <- fmap catMaybes $ mapM modsumToInfo graph
  let ofiles = map (ml_obj_file . ms_location) graph
  -- TODO find a suitable way to get a list of Modules to use
  -- passing [] now defaults to JSMain (or failing that Main)
  liftIO $ GHCJSMain.linkJavaScript df (linkedFiles++ofiles) (collectDeps ifaces) []

modsumToInfo :: GhcMonad m => ModSummary -> m (Maybe ModuleInfo)
modsumToInfo ms = getModuleInfo (ms_mod ms)

collectDeps :: [ModuleInfo] -> [PackageId]
collectDeps mis = nub $ concatMap pkgs mis
    where
      pkgs mi = maybe [] (map fst . dep_pkgs . mi_deps) $ modInfoIface mi

printRts :: IO ()
#ifdef GHCJS_GEN2
printRts = putStrLn Gen2.rtsStr >> exitSuccess
#else
printRts = return () >> exitSuccess
#endif

printJi :: [String] -> IO ()
#ifdef GHCJS_GEN2
printJi ["--print-ji", file] = Gen2.readDeps file >>= putStrLn . Gen2.dumpDeps
printJi _                    = putStrLn "usage: ghcjs --print-ji jifile"
#else
printJi _ = return ()
#endif

setOpt = gopt_set
unsetOpt = gopt_unset

-- add some configs
setDfOpts :: DynFlags -> DynFlags
setDfOpts df = foldl' setOpt (foldl' unsetOpt df unsetList) setList
  where
    setList = []
    unsetList = [Opt_SplitObjs]

-- | configure the GHC API for building 32 bit code
setGhcjsPlatform :: FilePath -> DynFlags -> DynFlags
setGhcjsPlatform basePath df = addPlatformDefines basePath $ df { settings = settings' }
  where
    settings' = (settings df) { sTargetPlatform    = ghcjsPlatform
                              , sPlatformConstants = ghcjsPlatformConstants
                              , sPgm_a             = ("ghcjs-gcc-stub", [])
                              , sPgm_l             = ("ghcjs-gcc-stub", [])
#ifdef GHCJS_GEN2
                              , sOverridePrimIface = Just ghcjsPrimIface
#endif
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
           , "WORDS_BIGENDIAN=1"
           , "FLOAT_WORDS_BIGENDIAN=1"
           , "__BIG_ENDIAN__=1"
           ]

runGhcSession mbMinusB a = do
     libDir <- getGlobalPackageBase
     errorHandler
        fatalMessager
        defaultFlushOut $
          runGhc (mbMinusB `mplus` Just libDir) $ a

-- also generate native code, compile with regular GHC settings, but make sure
-- that generated files don't clash with ours
generateNative :: Bool -> [Located String] -> [String] -> Maybe String -> IO ()
generateNative oneshot argsS args1 mbMinusB =
  runGhcSession mbMinusB $ do -- sourceErrorHandler $ do
       do   sdflags <- getSessionDynFlags
            (dflags0, fileargs', _) <- parseDynamicFlags sdflags (ignoreUnsupported argsS)
            dflags1 <- liftIO $ if isJust mbMinusB then return dflags0 else addPkgConf dflags0
            (dflags2, _) <- liftIO (initPackages dflags1)
            setSessionDynFlags $
                                 dflags2 { ghcMode = if oneshot then OneShot else CompManager
                                         , ghcLink = NoLink
                                         }
            df <- getSessionDynFlags
            liftIO (writeIORef (canGenerateDynamicToo df) True)
            let (jsArgs, fileargs) = partition isJsFile (map unLoc fileargs')
            if all isBootFilename fileargs
              then do
--                liftIO $ putStrLn "native one shot"
                env <- getSession
                liftIO $ oneShot env StopLn (map (,Nothing) fileargs)
              else do
                mtargets <- catchMaybe $ mapM (flip guessTarget Nothing) fileargs
                case mtargets of
                  Nothing -> do
--                    liftIO $ putStrLn "falling back for native"
                    liftIO (fallbackGhc True False args1) -- fixme check status code
                  Just targets -> do
--                    liftIO $ putStrLn "generating native myself"
                    setTargets targets
                    _ <- load LoadAllTargets
                    return ()

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
                      map (getName . AnId . mkGhcjsPrimOpId) allThePrimOps
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
       setSessionDynFlags $ setGhcjsPlatform base sdflags
       env <- getSession
       liftIO $ showIface env iface
printIface _                       = putStrLn "usage: ghcjs --show-iface hifile"

-- touch an output file, don't overwrite if it exists, to keep build systems happy
touchOutputFile :: GhcMonad m => m ()
touchOutputFile = do
  df <- getSessionDynFlags
  liftIO $
    case outputFile df of
      Nothing -> return ()
      Just file -> do
        e <- doesFileExist file
        putStrLn $ "touching: " ++ file
        if not e
          then writeFile file "GHCJS dummy output"
          else touch df "keep build system happy" file

mkGhcjsOutput :: String -> String
mkGhcjsOutput "" = ""
mkGhcjsOutput file
  | ext == ".hi" = replaceExtension file ".js_hi"
  | ext == ".o"  = replaceExtension file ".js_o"
  | otherwise    = file
  where
    ext = takeExtension file

doFakeNative :: DynFlags -> FilePath -> IO ()
doFakeNative df base = do
  b <- getEnvOpt "GHCJS_FAKE_NATIVE"
  when b $ do
    putStrLn ("faking native: " ++ base)
    copyNoOverwrite (base ++ ".backup_hi") (base ++ ".hi")
    copyNoOverwrite (base ++ ".backup_o") (base ++ ".o")
    touchFile df (base ++ ".hi")
    touchFile df (base ++ ".o")

touchFile :: DynFlags -> FilePath -> IO ()
touchFile df file = do
  e <- doesFileExist file
  when e (touch df "keep build system happy" file)
  
copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
  ef <- doesFileExist from
  et <- doesFileExist to
  when (ef && not et) (copyFile from to)

-- | generate native code only from native compiler for these package
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


