{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables, TupleSections #-}
module Main where

import           Paths_ghcjs
import           Data.Typeable
import qualified GHC.Paths
import           GHC
import           HscMain (hscSimplify)
import           TidyPgm (tidyProgram)
import           CoreToStg (coreToStg)
import           SimplStg (stg2stg)
import           DynFlags
import           Platform
import           ErrUtils (fatalErrorMsg'')
import           CorePrep (corePrepPgm)
import           DriverPhases (HscSource (HsBootFile), Phase (..),
                               isHaskellSrcFilename, isSourceFilename)
import           DriverPipeline (oneShot)
import           DsMeta (templateHaskellNames)
import           Exception
import           HscTypes (ModGuts, CgGuts (..), HscEnv (..), Dependencies (..),
                           NameCache (..), isBootSummary, mkSrcErr, ModGuts(..),
                           SourceError)
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
import           Outputable hiding ((<>))

import           Control.Applicative
import           Control.Monad
import           Data.Char (toLower)
import           Data.IORef (modifyIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.Monoid

import           System.Environment (getArgs, getEnv)
import           System.Exit(ExitCode(..), exitWith)

import           Compiler.Info
import           Compiler.Variants
import qualified GHCJSMain
import           MonadUtils (MonadIO(..))
import           System.FilePath (takeExtension, dropExtension, addExtension,
                                  replaceExtension, (</>))
import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory,
                                   doesFileExist, copyFile)
import qualified Control.Exception as Ex

import           Control.Monad (when, mplus, forM, forM_)
import           System.Exit (exitSuccess)
import           System.Process (rawSystem)
import           System.IO
import           Data.Monoid (mconcat, First(..))
import           Data.List (isSuffixOf, isPrefixOf, tails, partition, nub,
                            intercalate, foldl')
import           Data.Maybe (isJust, fromMaybe, catMaybes, isNothing)

import           Options.Applicative
import           Options.Applicative.Types
import           Options.Applicative.Builder.Internal
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8  as C8
import qualified Gen2.Generator as Gen2
import qualified Gen2.Linker    as Gen2
import qualified Gen2.Rts       as Gen2
import           Gen2.PrimIface as Gen2

-- debug
import           Finder
import           PrelNames
import           FastString

data GhcjsSettings = GhcjsSettings { gsNativeExecutables :: Bool
                                   , gsNoNative          :: Bool
                                   , gsNoJSExecutables   :: Bool
                                   , gsLogCommandLine    :: Maybe FilePath
                                   , gsGhc               :: Maybe FilePath
                                   } deriving (Eq, Show)

instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False Nothing Nothing
  mappend (GhcjsSettings ne1 nn1 nj1 lc1 gh1)
          (GhcjsSettings ne2 nn2 nj2 lc2 gh2) =
          GhcjsSettings (ne1 || ne2) (nn1 || nn2) (nj1 || nj2) (lc1 <> lc2) (gh1 <> gh2)

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
         ]
    envSettings = GhcjsSettings <$> getEnvOpt "GHCJS_NATIVE_EXECUTABLES"
                                <*> getEnvOpt "GHCJS_NO_NATIVE"
                                <*> pure False
                                <*> getEnvMay "GHCJS_LOG_COMMANDLINE_NAME"
                                <*> getEnvMay "GHCJS_WITH_GHC"

optParser' :: ParserInfo GhcjsSettings
optParser' = info (helper <*> optParser) fullDesc

optParser :: Parser GhcjsSettings
optParser = GhcjsSettings
            <$> switch ( long "native-executables" )
            <*> switch ( long "no-native" )
            <*> switch ( long "no-js-executables" )
            <*> optStr ( long "log-commandline" )
            <*> optStr ( long "with-ghc" )

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

     if noNative
            then liftIO (putStrLn "skipping native code")
            else liftIO $ do
--              putStrLn "generating native code"
              generateNative settings oneshot argsS args1 mbMinusB
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
                          when (not noNative) (liftIO $ fallbackGhc settings False False args1)
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
                            LinkBinary -> when (        not oneshot
                                               && isJust (outputFile dflags3)
                                               && not (gsNoJSExecutables settings)) $ do
                              buildExecutable dflags3 jsArgs
--                              touchOutputFile
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


handleCommandline :: GhcjsSettings -> [String] -> Maybe String -> IO ()
handleCommandline settings args minusBargs
    | "-c" `elem` args      = handleOneShot settings args
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
            , ("-M", fallbackGhc settings False True args)
            , ("--print-rts", printRts)
            , ("--print-ji", printJi args)
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

handleOneShot :: GhcjsSettings -> [String] -> IO ()
handleOneShot settings args
  | fallback  = fallbackGhc settings True True args >> exitSuccess
  | otherwise = return ()
    where
      fallback = any isFb (tails args)
      isFb ({- "-c": -} c:_) = any (`isSuffixOf` c) [".c", ".cmm"]
      isFb _          = False

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

{-
   call GHC for things that we don't handle internally

   fixme: either remove this hack or properly check that the version of the called ghc is the expected one
   GHCJS_FALLBACK_GHC is the location of the ghc executable
   if GHCJS_FALLBACK_PLAIN is set, all arguments are passed through verbatim
   to the fallback ghc, including -B
-}
fallbackGhc :: GhcjsSettings -> Bool -> Bool -> [String] -> IO ()
fallbackGhc settings isNative nonHaskell args = do
  pkgargs <- pkgConfArgs
  let ghc = fromMaybe "ghc" (gsGhc settings)
      noNative = gsNoNative settings
  plain <- getEnvOpt "GHCJS_FALLBACK_PLAIN"
  args' <- if plain then getArgs else return args
  if isNative
    then when (not noNative || nonHaskell) $ do
--      putStrLn ("falling back with: " ++ intercalate " " (pkgargs ++ args'))
      void (rawSystem ghc $ pkgargs ++ args' ++ ["-v"]) -- ++ ["-hisuf", "native_hi"])
    else do
--      putStrLn $ "falling back with: " ++ intercalate " " (pkgargs ++ args' ++ ["-osuf", "js_o"])
      void $ rawSystem ghc $ pkgargs ++ args' ++ ["-osuf", "js_o"]
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
printRts = putStrLn Gen2.rtsStr >> exitSuccess


printJi :: [String] -> IO ()
printJi ["--print-ji", file] = Gen2.readDeps file >>= putStrLn . Gen2.dumpDeps
printJi _                    = putStrLn "usage: ghcjs --print-ji jifile"

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
                              , sOverridePrimIface = Just ghcjsPrimIface
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
generateNative :: GhcjsSettings -> Bool -> [Located String] -> [String] -> Maybe String -> IO ()
generateNative settings oneshot argsS args1 mbMinusB =
  runGhcSession mbMinusB $ do
       do   sdflags <- getSessionDynFlags
            (dflags0, fileargs', _) <- parseDynamicFlags sdflags (ignoreUnsupported argsS)
            dflags1 <- liftIO $ if isJust mbMinusB then return dflags0 else addPkgConf dflags0
            (dflags2, _) <- liftIO (initPackages dflags1)
            setSessionDynFlags $
                                 dflags2 { ghcMode = if oneshot then OneShot else CompManager
                                         , ghcLink = if gsNativeExecutables settings then ghcLink dflags2
                                                                                     else NoLink
                                         }
            df <- getSessionDynFlags
            liftIO (writeIORef (canGenerateDynamicToo df) True)
            let (jsArgs, fileargs) = partition isJsFile (map unLoc fileargs')
            if all isBootFilename fileargs
              then sourceErrorHandler $ do
--                liftIO $ putStrLn "native one shot"
                env <- getSession
                liftIO $ oneShot env StopLn (map (,Nothing) fileargs)
              else sourceErrorHandler $ do
                mtargets <- catchMaybe $ mapM (flip guessTarget Nothing) fileargs
                case mtargets of
                  Nothing -> do
--                    liftIO $ putStrLn "falling back for native"
                    liftIO (fallbackGhc settings True False args1) -- fixme check status code
                  Just targets -> do
--                    liftIO $ putStrLn "generating native myself"
                    setTargets targets
                    success <- load LoadAllTargets
                    when (failed success) (throw (ExitFailure 1))
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


