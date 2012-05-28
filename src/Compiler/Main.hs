{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Main where

import Paths_ghcjs

import qualified GHC.Paths
import GHC
import HscMain (hscSimplify)
import TidyPgm (tidyProgram)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import DynFlags (defaultLogAction, defaultDynFlags, supportedLanguagesAndExtensions, compilerInfo)
import HscTypes (ModGuts, CgGuts (..))
import CorePrep (corePrepPgm)
import DriverPhases (HscSource (HsBootFile))

import System.Environment (getArgs, getEnv)

import Compiler.Info
import Compiler.Variants

import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import MonadUtils (MonadIO(..))
import Generator.Helpers (runGen, newGenState)
import System.FilePath (dropExtension, addExtension, replaceExtension, (</>))
import System.Directory (createDirectoryIfMissing)
import qualified Control.Exception as Ex

import Control.Monad (when, mplus, forM, forM_)
import System.Exit (exitSuccess, exitFailure)
import System.Process (rawSystem)
import System.IO
import Data.Monoid (mconcat, First(..))
import Data.List (isSuffixOf, isPrefixOf, tails, partition)
import Data.Maybe (isJust, fromMaybe)

import Crypto.Skein
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import Crypto.Conduit (hashFile)
import qualified Data.Serialize as C

main :: IO ()
main =
  do args0 <- getArgs
       
     let (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
         mbMinusB | null minusB_args = Nothing
                  | otherwise = Just . drop 2 . last $ minusB_args

     handleCommandline args1
     libDir <- getGlobalPackageBase
     (argsS, _) <- parseStaticFlags $ map noLoc args1
     defaultErrorHandler
        defaultLogAction
        $ runGhc (mbMinusB `mplus` Just libDir) $
       do sdflags <- getSessionDynFlags
          let oneshot = "-c" `elem` args1
              sdflags' = sdflags { ghcMode = if oneshot then OneShot else CompManager
                                 , ghcLink = if oneshot then NoLink  else ghcLink sdflags
                                 }
          (dflags, fileargs', _) <- parseDynamicFlags sdflags' $ ignoreUnsupported argsS --  (map noLoc $ ignoreUnsupported args1) -- ("-DWORD_SIZE_IN_BITS=32":args'))
          dflags' <- liftIO $ if isJust mbMinusB then return dflags else addPkgConf dflags
          _ <- setSessionDynFlags dflags'
          let fileargs = map unLoc fileargs'
          targets <- mapM (flip guessTarget Nothing) fileargs
          setTargets targets
          _ <- load LoadAllTargets
          mgraph <- depanal [] False
          mapM_ compileModSummary mgraph

addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  base <- getGlobalPackageBase
  return $ df
             { extraPkgConfs = db1 : db2 : extraPkgConfs df
             , includePaths  = (base ++ "/include") : includePaths df -- fixme: shouldn't be necessary if builtin_rts has this in its include-dirs?
             }

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
     unsupported xs = putStrLn (xs ++ " is currently unsupported") >> exitFailure
     acts :: [(String, IO ())]
     acts = [ ("--supported-languages", mapM_ putStrLn supportedLanguagesAndExtensions)
            , ("--numeric-version", putStrLn getCompilerVersion)
            , ("--info", print =<< getCompilerInfo)
            , ("--print-libdir", putStrLn =<< getLibDir)
            , ("--abi-hash", fallbackGhc args)
            , ("-M", fallbackGhc args)
            , ("-shared", fallbackGhc args)
            ]

handleOneShot :: [String] -> IO ()
handleOneShot args | fallback  = fallbackGhc args >> exitSuccess
                   | otherwise = return ()
    where
      fallback = any isFb (tails args)
      isFb ({- "-c": -} c:_) = any (`isSuffixOf` c) [".c", ".cmm", ".hs-boot", ".lhs-boot"]
      isFb _          = False

-- call ghc for things that we don't handle internally
-- fixme: either remove this hack or properly check that the version of the called ghc is the expected one
-- GHCJS_FALLBACK_GHC is the location of the ghc executable
-- if GHCJS_FALLBACK_PLAIN is set, all arguments are passed through verbatim
-- to the fallback ghc, including -B
fallbackGhc args = do
  db <- getGlobalPackageDB
  ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_FALLBACK_GHC"
  plain <- getEnvMay "GHCJS_FALLBACK_PLAIN"
  case plain of
    Just _  -> getArgs >>= rawSystem ghc
    Nothing -> rawSystem ghc $ "-package-conf" : db : args
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
     versions <- liftIO $ forM variants $ \variant -> do
          program <- liftIO $ concreteJavascriptFromCgGuts dflags tidyCore variant
          let outputFile = addExtension outputBase (variantExtension variant)
          putStrLn $ concat ["Writing module ", name, " (", outputFile, ")"]
          writeFile outputFile program
          return (variant, program)
     liftIO $ writeCachedFiles outputBase versions
  where summary = pm_mod_summary . tm_parsed_module . dm_typechecked_module $ mod
        outputBase = dropExtension (ml_hi_file . ms_location $ summary)
        name = moduleNameString . moduleName . ms_mod $ summary
        dflags = ms_hspp_opts $ summary

{-
   temporary workaround for lacking Cabal support: 
   - write .js source to cache, file name based on the skein hash of
     the corresponding .hi file. cabaljs picks this up to complete
     the package installation
-}
writeCachedFiles :: FilePath -> [(Variant, String)] -> IO ()
writeCachedFiles jsFile variants = do
  let hiFile = (dropExtension jsFile) ++ ".hi"
  (hash :: Skein_512_512) <- hashFile hiFile
  cacheDir <- getGlobalCache
  let basename = C8.unpack . B16.encode . C.encode $ hash
  createDirectoryIfMissing True cacheDir
  forM_ variants $ \(variant, program) ->
    writeFile (cacheDir </> basename ++ variantExtension variant) program

cgGutsFromModGuts :: GhcMonad m => ModGuts -> m CgGuts
cgGutsFromModGuts guts =
  do hscEnv <- getSession
     simplGuts <- liftIO $ hscSimplify hscEnv guts
     (cgGuts, _) <- liftIO $ tidyProgram hscEnv simplGuts
     return cgGuts

concreteJavascriptFromCgGuts :: DynFlags -> CgGuts -> Variant -> IO String
concreteJavascriptFromCgGuts dflags core variant =
  do core_binds <- corePrepPgm dflags (cg_binds core) (cg_tycons $ core)
     stg <- coreToStg dflags core_binds
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
     return $ variantRender variant stg' (cg_module core)

