{-# LANGUAGE CPP, GADTs, ScopedTypeVariables, ImpredicativeTypes, OverloadedStrings, TupleSections #-}
module Compiler.GhcjsHooks where

import           CorePrep             (corePrepPgm)
-- import           Gen2.GHC.CorePrep    (corePrepPgm) -- customized to not float new toplevel binds
import           CoreToStg            (coreToStg)
import           DriverPipeline
import           DriverPhases
import           DynFlags
import           GHC
import           GhcMonad
import           Hooks
import           Panic
import qualified SysTools
import           SimplStg             (stg2stg)
import           HeaderInfo
import           HscTypes
import           Maybes               (expectJust)

import           Control.Concurrent.MVar
import           Control.Monad

import qualified Data.ByteString      as B
import qualified Data.Map             as M

import           System.Directory     (copyFile, createDirectoryIfMissing)
import           System.FilePath

import           Compiler.Settings
import qualified Compiler.Utils       as Utils
import           Compiler.Variants

import qualified Gen2.DynamicLinking  as Gen2
import qualified Gen2.Foreign         as Gen2

import qualified Gen2.PrimIface       as Gen2
import qualified Gen2.TH              as Gen2TH

import           System.IO.Error

import           TcRnTypes

installGhcjsHooks :: GhcjsEnv
                  -> GhcjsSettings
                  -> [FilePath]  -- ^ JS objects
                  -> DynFlags -> DynFlags
installGhcjsHooks env settings js_objs dflags =
  Gen2.installForeignHooks True $ dflags { hooks = addHooks (hooks dflags) }
    where
      addHooks h = h
        { linkHook               = Just (Gen2.ghcjsLink env settings js_objs True)
        , getValueSafelyHook     = Just (Gen2TH.ghcjsGetValueSafely settings)
#if __GLASGOW_HASKELL__ >= 709
        , runMetaHook            = Just (Gen2TH.ghcjsRunMeta env settings)
#else
        , hscCompileCoreExprHook = Just (Gen2TH.ghcjsCompileCoreExpr env settings)
#endif
        }

installNativeHooks :: GhcjsEnv -> GhcjsSettings -> DynFlags -> DynFlags
installNativeHooks env settings dflags =
  Gen2.installForeignHooks False $ dflags { hooks = addHooks (hooks dflags) }
    where
      addHooks h = h { linkHook               = Just (Gen2.ghcjsLink env settings [] False)
#if !(__GLASGOW_HASKELL__ >= 709)
                     , getValueSafelyHook     = Just Gen2.ghcjsGetValueSafely
                     , hscCompileCoreExprHook = Just Gen2.ghcjsCompileCoreExpr
#endif
                     }

--------------------------------------------------
-- One shot replacement (the oneShot in DriverPipeline
-- always uses the unhooked linker)

ghcjsOneShot :: GhcjsEnv -> GhcjsSettings -> Bool -> HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
ghcjsOneShot env settings native hsc_env stop_phase srcs = do
  o_files <- mapM (compileFile hsc_env stop_phase) srcs
  Gen2.ghcjsDoLink env settings native (hsc_dflags hsc_env) stop_phase o_files

--------------------------------------------------
-- Driver hooks

installDriverHooks :: GhcjsSettings -> GhcjsEnv -> DynFlags -> DynFlags
installDriverHooks settings env df = df { hooks = hooks' }
  where hooks' = (hooks df) { runPhaseHook     = Just (runGhcjsPhase settings env)
                            , ghcPrimIfaceHook = Just Gen2.ghcjsPrimIface
                            }

runGhcjsPhase :: GhcjsSettings
              -> GhcjsEnv
              -> PhasePlus -> FilePath -> DynFlags
              -> CompPipeline (PhasePlus, FilePath)

runGhcjsPhase _settings _env (RealPhase (Cpp sf)) input_fn dflags0
  = do
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       liftIO $ checkProcessArgsResult dflags1 unhandled_flags

       if not (xopt Opt_Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (gopt Opt_Pp dflags1) $
               liftIO $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (RealPhase (HsPp sf), input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            liftIO $ Utils.doCpp dflags1 True{-raw-} True
                           input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- liftIO $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags0 src_opts
            liftIO $ checkProcessArgsResult dflags2 unhandled_flags
            unless (gopt Opt_Pp dflags2) $
                liftIO $ handleFlagWarnings dflags2 warns
            -- the HsPp pass below will emit warnings

            setDynFlags dflags2

            return (RealPhase (HsPp sf), output_fn)

runGhcjsPhase settings env (HscOut src_flavour mod_name result) _ dflags = do

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
#if MIN_VERSION_ghc(7,9,0)
            HscUpdateSig ->
                do -- We need to create a REAL but empty .o file
                   -- because we are going to attempt to put it in a library
                   PipeState{hsc_env=hsc_env'} <- getPipeState
                   let input_fn = expectJust "runPhase" (ml_hs_file location)
                       basename = dropExtension input_fn
		   -- fixme do we need to create a js_o file here?
                   -- liftIO $ compileEmptyStub dflags hsc_env' basename location
                   return (RealPhase next_phase, o_file)
#endif
            HscRecomp cgguts mod_summary
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    outputFilename <- liftIO $
                      ghcjsWriteModule settings env
                        hsc_env' cgguts mod_summary output_fn

                    return (RealPhase next_phase, outputFilename)
-- skip these, but copy the result
runGhcjsPhase _ _ (RealPhase ph) input _dflags
  | Just next <- lookup ph skipPhases = do
    output <- phaseOutputFilename next
    liftIO $ (createDirectoryIfMissing True (takeDirectory output) >>
              copyFile input output)
                `catchIOError` \_ -> return ()
    return (RealPhase next, output)
  where
#if MIN_VERSION_ghc(7,8,3)
    skipPhases = [ (CmmCpp, Cmm), (Cmm, As False), (Cmm, As True), (As False, StopLn), (As True, StopLn) ]
#else
    skipPhases = [ (CmmCpp, Cmm), (Cmm, As), (As, StopLn) ]
#endif

-- otherwise use default
runGhcjsPhase _ _ p input dflags = runPhase p input dflags

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path


ghcjsWriteModule :: GhcjsSettings
                 -> GhcjsEnv
                 -> HscEnv      -- ^ Environment in which to compile the module
                 -> CgGuts
                 -> ModSummary
                 -> FilePath    -- ^ Output path
                 -> IO FilePath
ghcjsWriteModule settings jsEnv env core mod output = do
    b <- ghcjsCompileModule settings jsEnv env core mod
    createDirectoryIfMissing True (takeDirectory output)
    B.writeFile output b
    return output

ghcjsCompileModule :: GhcjsSettings
                   -> GhcjsEnv
                   -> HscEnv      -- ^ Environment in which to compile the module
                   -> CgGuts
                   -> ModSummary
                   -> IO B.ByteString
-- dynamic-too will invoke this twice, cache results in GhcjsEnv
ghcjsCompileModule settings jsEnv env core mod = do
  ifGeneratingDynamicToo dflags genDynToo genOther
  where
    genDynToo = do
      result <- compile
      modifyMVar_ cms (return . M.insert mod' result)
      return result
    genOther =
      join $ modifyMVar cms $ \m -> do
        case M.lookup mod' m of
          Nothing -> return (m, compile)
          Just r  -> return (M.delete mod' m, return r)
    mod'   = ms_mod mod
    cms    = compiledModules jsEnv
    dflags = hsc_dflags env
    compile = do
#if __GLASGOW_HASKELL__ < 709
      core_binds <- corePrepPgm dflags env (cg_binds core) (cg_tycons core)
#else
      core_binds <- corePrepPgm env (ms_location mod) (cg_binds core) (cg_tycons core)
#endif
      stg <- coreToStg dflags (cg_module core) core_binds
      (stg', cCCs) <- stg2stg dflags (cg_module core) stg
      return $ variantRender gen2Variant settings dflags (cg_module core) stg' cCCs


