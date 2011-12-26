{-# LANGUAGE CPP, TypeFamilies #-}
module Main where

import qualified GHC.Paths
import GHC
import HscMain (hscSimplify)
import TidyPgm (tidyProgram)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
#if __GLASGOW_HASKELL__ >= 702
import DynFlags (defaultLogAction)
#else
import DynFlags (defaultDynFlags)
#endif
import HscTypes (ModGuts, CgGuts (..))
import CorePrep (corePrepPgm)
import DriverPhases (HscSource (HsBootFile))

import System.Environment (getArgs)

import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import MonadUtils (MonadIO(..))
import Generator.Helpers (runGen, newGenState)
import System.FilePath (replaceExtension)

data CallingConvention = Plain | Trampoline

main :: IO ()
main =
  do args <- getArgs

     -- FIXME: I wasn't able to find any sane command line parsing
     --        library that allow sending unprocessed arguments to GHC...
     let (callingConvention, args') =
           case args
             of ("--calling-convention=plain":args) -> (Plain, args)
                ("--calling-convention=trampoline":args) -> (Trampoline, args)
                _ -> (Plain, args)
     defaultErrorHandler
#if __GLASGOW_HASKELL__ >= 702
        defaultLogAction
#else
        defaultDynFlags
#endif
        $ runGhc (Just GHC.Paths.libdir) $
       do sdflags <- getSessionDynFlags
          (dflags, fileargs', _) <- parseDynamicFlags sdflags (map noLoc args') -- ("-DWORD_SIZE_IN_BITS=32":args'))
          _ <- setSessionDynFlags dflags
          let fileargs = map unLoc fileargs'
          targets <- mapM (flip guessTarget Nothing) fileargs
          setTargets targets
          _ <- load LoadAllTargets
          mgraph <- depanal [] False
          mapM_ (compileModSummary callingConvention) mgraph

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
  where summary = pm_mod_summary . tm_parsed_module . dm_typechecked_module $ mod
        outputFile = replaceExtension (ml_hi_file . ms_location $ summary) ".js"
        name = moduleNameString . moduleName . ms_mod $ summary
        dflags = ms_hspp_opts $ summary

cgGutsFromModGuts :: GhcMonad m => ModGuts -> m CgGuts
cgGutsFromModGuts guts =
  do hscEnv <- getSession
#if __GLASGOW_HASKELL__ >= 702
     simplGuts <- liftIO $ hscSimplify hscEnv guts
#else
     simplGuts <- hscSimplify guts
#endif
     (cgGuts, _) <- liftIO $ tidyProgram hscEnv simplGuts
     return cgGuts

concreteJavascriptFromCgGuts :: DynFlags -> CallingConvention -> CgGuts -> IO String
concreteJavascriptFromCgGuts dflags callingConvention core =
  do core_binds <- corePrepPgm dflags (cg_binds core) (cg_tycons $ core)
#if __GLASGOW_HASKELL__ >= 703
     stg <- coreToStg dflags core_binds
#else
     stg <- coreToStg (modulePackageId . cg_module $ core) core_binds
#endif
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
     let abstract :: Javascript js => js
         abstract = fst $ runGen (Js.generate (cg_module core) stg') newGenState
     return $
       case callingConvention
       of Plain -> show (abstract :: Js.Formatted)
          Trampoline -> show (abstract :: Js.Trampoline Js.Formatted)

