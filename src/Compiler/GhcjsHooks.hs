{-# LANGUAGE OverloadedStrings #-}
module Compiler.GhcjsHooks where

import           Config               (cDYNAMIC_GHC_PROGRAMS, cProjectVersion)
import           Gen2.GHC.CoreToStg (coreToStg) -- version that does not generate StgLetNoEscape
import           CorePrep (corePrepPgm)
import           Distribution.Package (PackageName (..))
import           DriverPipeline
import           DriverPhases
import           DynFlags
import           GHC
import           GhcMonad
import           Hooks
import           HscTypes             (mkHsSOName, mkSOName, CgGuts(..), HscEnv(..))
import           HscMain              (HscStatus(..))
import           LoadIface
import           Module
import           Panic
import           Packages
import           Platform
import qualified SysTools
import           SimplStg             (stg2stg)
import           UniqFM               (eltsUFM)

import           Control.Monad
import qualified Data.ByteString      as B
import           Data.List            (isInfixOf, isPrefixOf, sort)
import           System.Directory     (doesFileExist, copyFile,
                                       createDirectoryIfMissing)
import           System.FilePath

import           Compiler.Info
import           Compiler.Variants
import           Compiler.Utils
import qualified Compiler.Utils       as Utils
import qualified Gen2.PrimIface       as Gen2
import qualified Gen2.Foreign         as Gen2
import qualified Gen2.DynamicLinking  as Gen2

installGhcjsHooks :: Bool        -- ^ Debug
                  -> [FilePath]  -- JS objects
                  -> DynFlags -> DynFlags
installGhcjsHooks debug js_objs dflags =
  Gen2.installForeignHooks True . flip setHooks dflags . addHooks . getHooks $ dflags
    where
      addHooks h = h { linkHook               = Just (Gen2.ghcjsLink debug js_objs True)
                     , getValueSafelyHook     = Just Gen2.ghcjsGetValueSafely
                     , hscCompileCoreExprHook = Just Gen2.ghcjsCompileCoreExpr
                     }

installNativeHooks :: DynFlags -> DynFlags
installNativeHooks dflags =
  Gen2.installForeignHooks False . flip setHooks dflags . addHooks . getHooks $ dflags
    where
      addHooks h = h { linkHook               = Just (Gen2.ghcjsLink False [] False)
                     , getValueSafelyHook     = Just Gen2.ghcjsGetValueSafely
                     , hscCompileCoreExprHook = Just Gen2.ghcjsCompileCoreExpr
                     }

--------------------------------------------------
-- One shot replacement (the oneShot in DriverPipeline
-- always uses the unhooked linker)

ghcjsOneShot :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
ghcjsOneShot hsc_env stop_phase srcs = do
  o_files <- mapM (compileFile hsc_env stop_phase) srcs
  Gen2.ghcjsDoLink (hsc_dflags hsc_env) stop_phase o_files

--------------------------------------------------
-- Driver hooks

installDriverHooks :: Bool -> DynFlags -> DynFlags
installDriverHooks debug df = setHooks hooks' df
  where hooks' = (getHooks df) { runPhaseHook     = Just (runGhcjsPhase debug)
                               , ghcPrimIfaceHook = Just Gen2.ghcjsPrimIface
                               }

runGhcjsPhase :: Bool
              -> PhasePlus -> FilePath -> DynFlags
              -> CompPipeline (PhasePlus, FilePath)
runGhcjsPhase debug (HscOut src_flavour mod_name result) _ dflags = do

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

                    outputFilename <- liftIO $ ghcjsWriteModule debug hsc_env' cgguts mod_summary output_fn

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


ghcjsWriteModule :: Bool        -- ^ Debug
                   -> HscEnv      -- ^ Environment in which to compile
                   -- the module
                   -> CgGuts      
                   -> ModSummary   
                   -> FilePath    -- ^ Output path
                   -> IO FilePath
ghcjsWriteModule debug env core mod output = do
    B.writeFile output =<< ghcjsCompileModule debug env core mod
    return output

ghcjsCompileModule :: Bool        -- ^ Debug
                   -> HscEnv      -- ^ Environment in which to compile
                   -- the module
                   -> CgGuts      
                   -> ModSummary   
                   -> IO B.ByteString
ghcjsCompileModule debug env core mod 
--  | WayDyn `elem` ways dflags = do
--      return "GHCJS dummy output"
  | otherwise = do
      core_binds <- corePrepPgm dflags env (cg_binds core) (cg_tycons core)
      stg <- coreToStg dflags (cg_module core) core_binds
      (stg', _ccs) <- stg2stg dflags (cg_module core) stg
      let obj = variantRender gen2Variant debug dflags stg' (cg_module core)
      return obj
    where
      dflags = hsc_dflags env


doFakeNative :: DynFlags -> FilePath -> IO ()
doFakeNative df base = do
  b <- getEnvOpt "GHCJS_FAKE_NATIVE"
  when b $ do
    mapM_ backupExt ["hi", "o", "dyn_hi", "dyn_o"]
    mapM_ touchExt  ["hi", "o", "dyn_hi", "dyn_o"]
  where
    backupExt ext = copyNoOverwrite (base ++ ".backup_" ++ ext) (base ++ "." ++ ext)
    touchExt  ext = touchFile df (base ++ "." ++ ext)

