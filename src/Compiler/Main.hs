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

import System.Environment (getArgs)

import Compiler.Info
import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import MonadUtils (MonadIO(..))
import Generator.Helpers (runGen, newGenState)
import System.FilePath (dropExtension, replaceExtension, (</>))
import System.Directory (createDirectoryIfMissing)

import Control.Monad (when, mplus)
import System.Exit (exitSuccess, exitFailure)
import System.Process (rawSystem)
import System.IO
import Data.Monoid (mconcat, First(..))
import Data.List (isSuffixOf, isPrefixOf, tails, partition)
import Data.Maybe (isJust)

import Crypto.Skein
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import Crypto.Conduit (hashFile)
import qualified Data.Serialize as C

data CallingConvention = Plain | Trampoline

main :: IO ()
main =
  do args0 <- getArgs
       
     let (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
         mbMinusB | null minusB_args = Nothing
                  | otherwise = Just . drop 2 . last $ minusB_args
         -- FIXME: I wasn't able to find any sane command line parsing
         --        library that allow sending unprocessed arguments to GHC...
         (callingConvention, args2) =
             case args1
               of ("--calling-convention=plain":args1') -> (Plain, args1')
                  ("--calling-convention=trampoline":args1') -> (Trampoline, args1')
                  _ -> (Trampoline, args1)

     handleCommandline args2
     libDir <- getGlobalPackageBase
     defaultErrorHandler
        defaultLogAction
        $ runGhc (mbMinusB `mplus` Just libDir) $
       do sdflags <- getSessionDynFlags
          let oneshot = "-c" `elem` args2
              sdflags' = sdflags { ghcMode = if oneshot then OneShot else CompManager
                                 , ghcLink = if oneshot then NoLink  else ghcLink sdflags
                                 }
          (dflags, fileargs', _) <- parseDynamicFlags sdflags' (map noLoc $ ignoreUnsupported args2) -- ("-DWORD_SIZE_IN_BITS=32":args'))
          dflags' <- liftIO $ if isJust mbMinusB then return dflags else addPkgConf dflags
          _ <- setSessionDynFlags dflags'
          let fileargs = map unLoc fileargs'
          targets <- mapM (flip guessTarget Nothing) fileargs
          setTargets targets
          _ <- load LoadAllTargets
          mgraph <- depanal [] False
          mapM_ (compileModSummary callingConvention) mgraph

addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  base <- getGlobalPackageBase
  return $ df
             { extraPkgConfs = db1 : db2 : extraPkgConfs df
             , includePaths  = (base ++ "/include") : includePaths df -- fixme: shouldn't be necessary if builtin_rts has this in its include-dirs?
             }

ignoreUnsupported :: [String] -> [String]
ignoreUnsupported args = filter (`notElem` ["--make", "-c", "-threaded", "-prof", "-fPIC", "-dynamic"]) args'
    where
      args' = filter (\x -> not $ any (`isPrefixOf` x) ["-H"]) args

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
fallbackGhc args = do
  db <- getGlobalPackageDB
  rawSystem "ghc" $ "-package-conf" : db : args
  return ()

compileModSummary :: GhcMonad m => CallingConvention -> ModSummary -> m ()
compileModSummary callingConvention mod =
  case ms_hsc_src mod
  of HsBootFile -> liftIO $ putStrLn $ concat ["Skipping boot ", name]
     _ ->
       do liftIO $ putStrLn $ concat ["Compiling ", name]
          desugaredMod <- desugaredModuleFromModSummary mod
          writeDesugaredModule callingConvention desugaredMod
  where name = moduleNameString . moduleName . ms_mod $ mod

desugaredModuleFromModSummary :: GhcMonad m => ModSummary -> m DesugaredModule
desugaredModuleFromModSummary mod =
  do parsedMod <- parseModule mod
     typedCheckMod <- typecheckModule parsedMod
     desugarModule typedCheckMod

writeDesugaredModule :: GhcMonad m => CallingConvention -> DesugaredModule -> m ()
writeDesugaredModule callingConvention mod =
  do tidyCore <- cgGutsFromModGuts (coreModule mod)
     program <- liftIO $ concreteJavascriptFromCgGuts dflags callingConvention tidyCore
     liftIO $
       do putStrLn $ concat ["Writing module ", name, " (to ", outputFile, ")"]
          writeFile outputFile program
          writeCachedFile outputFile program
  where summary = pm_mod_summary . tm_parsed_module . dm_typechecked_module $ mod
        outputFile = replaceExtension (ml_hi_file . ms_location $ summary) ".js"
        name = moduleNameString . moduleName . ms_mod $ summary
        dflags = ms_hspp_opts $ summary

{-
   temporary workaround for lacking Cabal support: 
   - write .js source to cache, file name based on the skein hash of
     the corresponding .hi file. cabaljs picks this up to complete
     the package installation
-}
writeCachedFile :: FilePath -> String -> IO ()
writeCachedFile jsFile program = do
  let hiFile = (dropExtension jsFile) ++ ".hi"
  (hash :: Skein_512_512) <- hashFile hiFile
  cacheDir <- getGlobalCache
  let basename = C8.unpack . B16.encode . C.encode $ hash
  createDirectoryIfMissing True cacheDir
  writeFile (cacheDir </> basename ++ ".js") program

cgGutsFromModGuts :: GhcMonad m => ModGuts -> m CgGuts
cgGutsFromModGuts guts =
  do hscEnv <- getSession
     simplGuts <- liftIO $ hscSimplify hscEnv guts
     (cgGuts, _) <- liftIO $ tidyProgram hscEnv simplGuts
     return cgGuts

concreteJavascriptFromCgGuts :: DynFlags -> CallingConvention -> CgGuts -> IO String
concreteJavascriptFromCgGuts dflags callingConvention core =
  do core_binds <- corePrepPgm dflags (cg_binds core) (cg_tycons $ core)
     stg <- coreToStg dflags core_binds
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
     let abstract :: Javascript js => js
         abstract = fst $ runGen (Js.generate (cg_module core) stg') newGenState
     return $
       case callingConvention
       of Plain -> show (abstract :: Js.Formatted)
          Trampoline -> show (abstract :: Js.Trampoline Js.Formatted)

