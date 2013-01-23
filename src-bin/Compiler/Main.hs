{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables, PackageImports #-}
module Main where

import           Paths_ghcjs

import qualified GHC.Paths
import           GHC
import           HscMain (hscSimplify)
import           TidyPgm (tidyProgram)
import           CoreToStg (coreToStg)
import           SimplStg (stg2stg)
import           DynFlags
#if __GLASGOW_HASKELL__ >= 707
import           Platform
#endif
import           HscTypes (ModGuts, CgGuts (..), HscEnv (..), Dependencies (..))
import           CorePrep (corePrepPgm)
import           DriverPhases (HscSource (HsBootFile))
import           Packages (initPackages)
import           Outputable (showPpr)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import qualified Control.Exception as Ex

import           Control.Monad (when, mplus, forM, forM_)
import           System.Exit (exitSuccess)
import           System.Process (rawSystem)
import           System.IO
import           Data.Monoid (mconcat, First(..))
import           Data.List (isSuffixOf, isPrefixOf, tails, partition, nub, intercalate, foldl')
import           Data.Maybe (isJust, fromMaybe, catMaybes)

import           Crypto.Skein
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8  as C8
import           Crypto.Conduit (hashFile)
import qualified Data.Serialize as C

#ifdef GHCJS_GEN2
import qualified Gen2.Generator as Gen2
import qualified Gen2.Linker    as Gen2
import qualified Gen2.Rts       as Gen2
#endif

main :: IO ()
main =
  do args0 <- getArgs
     logCmd <- getEnvMay "GHCJS_LOG_COMMANDLINE"
     case logCmd of
       Just "1" -> do
         dir <- getAppUserDataDirectory "ghcjs"
         createDirectoryIfMissing True dir
         appendFile (dir </> "cmd.log") (intercalate " " ("ghcjs" : args0) ++ "\n")
       _ -> return ()

     let (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
         mbMinusB | null minusB_args = Nothing
                  | otherwise = Just . drop 2 . last $ minusB_args

     handleCommandline args1
     libDir <- getGlobalPackageBase
     (argsS, _) <- parseStaticFlags $ map noLoc args1
     defaultErrorHandler
#if __GLASGOW_HASKELL__ >= 706
        defaultFatalMessager
        defaultFlushOut
#else
        defaultLogAction
#endif
        $ runGhc (mbMinusB `mplus` Just libDir) $
       do sdflags <- getSessionDynFlags
          let oneshot = "-c" `elem` args1
              sdflags' = sdflags { ghcMode = if oneshot then OneShot else CompManager
                                 , ghcLink = NoLink
                                 }
          (dflags0, fileargs', _) <- parseDynamicFlags sdflags' $ ignoreUnsupported argsS
          dflags1 <- liftIO $ if isJust mbMinusB then return dflags0 else addPkgConf dflags0
          (dflags2, _) <- liftIO $ initPackages dflags1
          _ <- setSessionDynFlags $ setDfOpts $ setGhcjsPlatform $
               dflags2 { ghcLink = NoLink
                       , objectSuf = "ghcjs_o"
                       }
          let (jsArgs, fileargs) = partition isJsFile (map unLoc fileargs')
--          liftIO $ putStrLn $ "js targets: " ++ show jsArgs
          -- if guessing targets results in an exception, there were non-haskell files: fallback
          mtargets <- catchMaybe $ mapM (flip guessTarget Nothing) fileargs
          case mtargets of
            Nothing      -> liftIO (fallbackGhc args1)
            Just targets -> do
                          liftIO (generateNative argsS mbMinusB)
                          setTargets targets
                          _ <- load LoadAllTargets
                          mgraph <- depanal [] False
                          mapM_ compileModSummary mgraph
                          dflags3 <- getSessionDynFlags
                          case ghcLink sdflags of
                            LinkBinary -> when (not oneshot) $ buildExecutable dflags3 jsArgs
                            LinkDynLib -> return ()
                            _          -> return ()

catchMaybe a = (fmap Just a) `gcatch` \(_::Ex.SomeException) -> return Nothing

isJsFile :: FilePath -> Bool
isJsFile = (==".js") . takeExtension

addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  base <- getGlobalPackageBase
  return $ df {
#if __GLASGOW_HASKELL__ >= 706
               extraPkgConfs = ([PkgConfFile db1, PkgConfFile db2]++)
#else
               extraPkgConfs = db1 : db2 : extraPkgConfs df
#endif
             , includePaths  = (base ++ "/include") : includePaths df -- fixme: shouldn't be necessary if builtin_rts has this in its include-dirs?
             }

pkgConfArgs :: IO [String]
pkgConfArgs = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  return $ map ("-package-conf"++) [db1,db2]

ignoreUnsupported :: [Located String] -> [Located String]
ignoreUnsupported =
  removeBy (`elem` unsup) .
  removeBy (\x -> any (x `isPrefixOf`) unsupPre)
    where
      removeBy :: (a -> Bool) -> [Located a] -> [Located a]
      removeBy g = filter (not . g . unLoc)
      unsup    = ["--make", "-c"] -- remove these arguments
      unsupPre = ["-H"]           -- remove arguments that start with these


handleCommandline :: [String] -> IO ()
handleCommandline args
    | "-c" `elem` args      = handleOneShot args
    | Just act <- lookupAct = act >> exitSuccess
    | otherwise             = return ()
   where
     lookupAct = getFirst . mconcat . map (First . (`lookup` acts)) $ args
--     unsupported xs = putStrLn (xs ++ " is currently unsupported") >> exitFailure
     acts :: [(String, IO ())]
     acts = [ ("--supported-languages", mapM_ putStrLn supportedLanguagesAndExtensions)
            , ("--numeric-version", fallbackGhc args) -- putStrLn getCompilerVersion)
            , ("--info", print =<< getCompilerInfo)
            , ("--print-libdir", putStrLn =<< getLibDir)
            , ("--abi-hash", fallbackGhc args)
            , ("-M", fallbackGhc args)
            , ("--print-rts", printRts)
            , ("--print-ji", printJi args)
            ]

handleOneShot :: [String] -> IO ()
handleOneShot args | fallback  = fallbackGhc args >> exitSuccess
                   | otherwise = return ()
    where
      fallback = any isFb (tails args)
      isFb ({- "-c": -} c:_) = any (`isSuffixOf` c) [".c", ".cmm", ".hs-boot", ".lhs-boot"]
      isFb _          = False

{-
   call GHC for things that we don't handle internally

   fixme: either remove this hack or properly check that the version of the called ghc is the expected one
   GHCJS_FALLBACK_GHC is the location of the ghc executable
   if GHCJS_FALLBACK_PLAIN is set, all arguments are passed through verbatim
   to the fallback ghc, including -B
-}
fallbackGhc :: [String] -> IO ()
fallbackGhc args = do
  pkgargs <- pkgConfArgs
  ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_FALLBACK_GHC"
  plain <- getEnvMay "GHCJS_FALLBACK_PLAIN"
  case plain of
    Just _  -> do
       as <- getArgs
       rawSystem ghc as --  (as ++ ["-hisuf", "native_hi"])
    Nothing -> rawSystem ghc $ pkgargs ++ args -- ++ ["-hisuf", "native_hi"]
  return ()

getEnvMay :: String -> IO (Maybe String)
getEnvMay xs = fmap Just (getEnv xs)
               `Ex.catch` \(_::Ex.SomeException) -> return Nothing


compileModSummary :: GhcMonad m => ModSummary -> m ()
compileModSummary mod =
  case ms_hsc_src mod
  of HsBootFile -> liftIO $ putStrLn $ concat ["Skipping boot ", name]
     _ ->
       do liftIO $ putStrLn $ concat ["Compiling ", name]
          desugaredMod <- desugaredModuleFromModSummary mod
          writeDesugaredModule desugaredMod
  where name = moduleNameString . moduleName . ms_mod $ mod

desugaredModuleFromModSummary :: GhcMonad m => ModSummary -> m DesugaredModule
desugaredModuleFromModSummary mod =
  do parsedMod <- parseModule mod
     typedCheckMod <- typecheckModule parsedMod
     desugarModule typedCheckMod

writeDesugaredModule :: GhcMonad m => DesugaredModule -> m ()
writeDesugaredModule mod =
  do tidyCore <- cgGutsFromModGuts (coreModule mod)
     env <- getSession
     versions <- liftIO $ forM variants $ \variant -> do
          (program, meta) <- liftIO $ concreteJavascriptFromCgGuts dflags env tidyCore variant
          let outputFile = addExtension outputBase (variantExtension variant)
          putStrLn $ concat ["Writing module ", name, " (", outputFile, ")"]
          B.writeFile outputFile program
          case variantMetaExtension variant of
            Nothing -> return ()
            Just mext -> B.writeFile (addExtension outputBase mext) meta
          return (variant, program, meta)
     liftIO $ writeCachedFiles dflags outputBase versions
  where
    summary = pm_mod_summary . tm_parsed_module . dm_typechecked_module $ mod
    outputBase = dropExtension (ml_hi_file . ms_location $ summary)
    name = moduleNameString . moduleName . ms_mod $ summary
    dflags = ms_hspp_opts $ summary

{-
   temporary workaround for lacking Cabal support:
   - write .js source to cache, file name based on the skein hash of
     the corresponding .hi file. ghcjs-cabal picks this up to complete
     the package installation
-}
writeCachedFiles :: DynFlags -> FilePath -> [(Variant, ByteString, ByteString)] -> IO ()
writeCachedFiles df jsFile variants = do
  let hiFile = (dropExtension jsFile) ++ "." ++ hiSuf df
  (hash :: Skein_512_512) <- hashFile hiFile
  cacheDir <- getGlobalCache
  let basename = C8.unpack . B16.encode . C.encode $ hash
  createDirectoryIfMissing True cacheDir
  forM_ variants $ \(variant, program, meta) -> do
    B.writeFile (cacheDir </> basename ++ variantExtension variant) program
    case variantMetaExtension variant of
      Nothing   -> return ()
      Just mext -> B.writeFile (cacheDir </> basename ++ mext) meta

cgGutsFromModGuts :: GhcMonad m => ModGuts -> m CgGuts
cgGutsFromModGuts guts =
  do hscEnv <- getSession
     simplGuts <- liftIO $ hscSimplify hscEnv guts
     (cgGuts, _) <- liftIO $ tidyProgram hscEnv simplGuts
     return cgGuts

concreteJavascriptFromCgGuts :: DynFlags -> HscEnv -> CgGuts -> Variant -> IO (ByteString, ByteString)
concreteJavascriptFromCgGuts dflags env core variant =
  do core_binds <- corePrepPgm dflags
#if __GLASGOW_HASKELL__ >= 706
                               env
#endif
                               (cg_binds core)
                               (cg_tycons $ core)
     stg <- coreToStg dflags core_binds
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
#if __GLASGOW_HASKELL__ >= 707
     return (variantRender variant dflags stg' (cg_module core))
#else
     return (variantRender variant dflags (map fst stg') (cg_module core))
#endif
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

#if __GLASGOW_HASKELL__ >= 707
setOpt = gopt_set
unsetOpt = gopt_unset
#else
setOpt = dopt_set
unsetOpt = dopt_unset
#endif

-- add some configs
setDfOpts :: DynFlags -> DynFlags
setDfOpts df = foldl' setOpt (foldl' unsetOpt df unsetList) setList
  where
    setList = []
    unsetList = [Opt_SplitObjs]

-- | configure the GHC API for building 32 bit code
setGhcjsPlatform :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 707
setGhcjsPlatform df = addPlatformDefines $ df { settings = settings' }
  where
    settings' = (settings df) { sTargetPlatform    = ghcjsPlatform
                              , sPlatformConstants = ghcjsPlatformConstants
                              }
    ghcjsPlatform = Platform ArchUnknown OSUnknown 4 False False False False
    ghcjsPlatformConstants = (sPlatformConstants (settings df))
       { pc_WORD_SIZE       = 4
       , pc_DOUBLE_SIZE     = 8
       , pc_CINT_SIZE       = 4
       , pc_CLONG_SIZE      = 4
       , pc_CLONG_LONG_SIZE = 8
       , pc_WORDS_BIGENDIAN = False
       }
#else
setGhcjsPlatform = addPlatformDefines
#endif

-- ghcjs builds for a strange platform: like 32 bit
-- instead of letting autoconf doing the defines, we override them here
addPlatformDefines :: DynFlags -> DynFlags
addPlatformDefines df = df { settings = settings1 }
  where
    settings0 = settings df
    settings1 = settings0 { sOpt_P = map ("-D"++) defs ++ sOpt_P settings0 }
    defs = [ "__GHCAUTOCONF_H__=1"
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
           , "GHCJS=1"
           , "__GHCJS__=1"
           ]

-- also generate native code, compile with regular GHC, but make sure
-- that generated files don't clash with ours
generateNative :: [Located String] -> Maybe String -> IO ()
generateNative argsS mbMinusB = 
 do
  libDir <- getGlobalPackageBase
  defaultErrorHandler
#if __GLASGOW_HASKELL__ >= 706
        defaultFatalMessager
        defaultFlushOut
#else
        defaultLogAction
#endif
        $ runGhc (mbMinusB `mplus` Just libDir) $
       do   sdflags <- getSessionDynFlags
            (dflags0, fileargs', _) <- parseDynamicFlags sdflags (ignoreUnsupported argsS)
            dflags1 <- liftIO $ if isJust mbMinusB then return dflags0 else addPkgConf dflags0
            (dflags2, _) <- liftIO (initPackages dflags1)
            setSessionDynFlags $ dflags2 { ghcLink = NoLink
                                         , hiSuf   = "native_hi"
                                         } 
            let (jsArgs, fileargs) = partition isJsFile (map unLoc fileargs')
            mtargets <- catchMaybe $ mapM (flip guessTarget Nothing) fileargs
            _ <- load LoadAllTargets
            mgraph <- depanal [] False
            mapM_ compileModSummary mgraph

