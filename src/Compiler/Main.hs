{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified GHC.Paths
import GHC
import HscMain (hscSimplify)
import TidyPgm (tidyProgram)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import DynFlags (defaultLogAction)
import HscTypes (ModGuts, CgGuts (..))
import CorePrep (corePrepPgm)
import DriverPhases (HscSource (HsBootFile))

import System.Environment (getArgs)

import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import MonadUtils (MonadIO(..))
import Generator.Helpers (newGenState)
import Control.Monad.State.Lazy (runState)
import System.IO (hFlush, openFile, hPutStrLn, Handle, IOMode(..))

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
     defaultErrorHandler defaultLogAction $ runGhc (Just GHC.Paths.libdir) $
       do sdflags <- getSessionDynFlags
          (dflags, fileargs', _) <- parseDynamicFlags sdflags (map noLoc args') -- ("-DWORD_SIZE_IN_BITS=32":args'))
          _ <- setSessionDynFlags dflags
          let fileargs = map unLoc fileargs'
          targets <- mapM (flip guessTarget Nothing) fileargs
          setTargets targets
          _ <- load LoadAllTargets
          mgraph <- depanal [] False
          let outputFileName = maybe "a.js" id (outputFile dflags)
          output <- liftIO $ openFile outputFileName WriteMode
          mapM_ (compileModSummary output callingConvention) mgraph

compileModSummary :: GhcMonad m => Handle -> CallingConvention -> ModSummary -> m ()
compileModSummary output callingConvention mod =
  case ms_hsc_src mod
  of HsBootFile -> liftIO $ putStrLn $ concat ["Skipping boot ", name]
     _ ->
       do liftIO $ putStrLn $ concat ["Compiling ", name]
          desugaredMod <- desugaredModuleFromModSummary mod
          writeDesugaredModule output callingConvention desugaredMod
  where name = moduleNameString . moduleName . ms_mod $ mod

desugaredModuleFromModSummary :: GhcMonad m => ModSummary -> m DesugaredModule
desugaredModuleFromModSummary mod =
  do parsedMod <- parseModule mod
     typedCheckMod <- typecheckModule parsedMod
     desugarModule typedCheckMod

writeDesugaredModule :: GhcMonad m => Handle -> CallingConvention -> DesugaredModule -> m ()
writeDesugaredModule output callingConvention mod =
  do tidyCore <- cgGutsFromModGuts (coreModule mod)
     program <- liftIO $ concreteJavascriptFromCgGuts dflags callingConvention tidyCore
     liftIO $
       do putStrLn $ concat ["Writing module ", name]
          hPutStrLn output $ "// Module " ++ name
          hPutStrLn output program
          hFlush output
          return ()
  where summary = pm_mod_summary . tm_parsed_module . dm_typechecked_module $ mod
        name = moduleNameString . moduleName . ms_mod $ summary
        dflags = ms_hspp_opts $ summary

cgGutsFromModGuts :: GhcMonad m => ModGuts -> m CgGuts
cgGutsFromModGuts guts =
  do hscEnv <- getSession
     simplGuts <- liftIO $ hscSimplify hscEnv guts
     (cgGuts, _) <- liftIO $ tidyProgram hscEnv simplGuts
     return cgGuts

concreteJavascriptFromCgGuts :: DynFlags -> CallingConvention -> CgGuts -> IO String
concreteJavascriptFromCgGuts dflags callingConvention core =
  do core_binds <- corePrepPgm dflags (cg_binds core) (cg_tycons $ core)
     stg <- coreToStg (modulePackageId . cg_module $ core) core_binds
     (stg', _ccs) <- stg2stg dflags (cg_module core) stg
     let abstract :: Javascript js => js
         abstract = fst $ runState (Js.generate (cg_module core) stg') newGenState
     return $
       case callingConvention
       of Plain -> show (abstract :: Js.Formatted)
          Trampoline -> show (abstract :: Js.Trampoline Js.Formatted)

