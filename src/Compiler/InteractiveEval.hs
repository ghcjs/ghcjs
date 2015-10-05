{-# LANGUAGE CPP, MagicHash, NondecreasingIndentation, UnboxedTuples, BangPatterns, TupleSections, QuasiQuotes #-}

module Compiler.InteractiveEval (
--        RunResult(..), Status(..), Resume(..), History(..),
        runStmt, runStmtWithLocation, runDecls, runDeclsWithLocation,
        parseImportDecl, RunnerState(..),
        ghcjsiInterrupt
--        , SingleStep(..),
{-        resume,
        abandon, abandonAll,
        getResumeContext,
        getHistorySpan,
        getModBreaks,
        getHistoryModule,
        back, forward,
        setContext, getContext,
        availsToGlobalRdrEnv,
        getNamesInScope,
        getRdrNamesInScope,
        moduleIsInterpreted,
        getInfo,
        exprType,
        typeKind,
        parseName,
        showModule,
        isModuleInterpreted,
        compileExpr, dynCompileExpr,
        Term(..), obtainTermFromId, obtainTermFromVal, reconstructType
-}
        ) where

-- #ifdef GHCI

#include "HsVersions.h"

import Data.Monoid

import InteractiveEvalTypes

import GhcMonad hiding (logWarnings)
import HscMain hiding (hscParseType, hscParseStmtWithLocation, hscParseStmt,
                       hscKcType, hscTcExpr, hscImport, hscDeclsWithLocation,
                       hscDecls, hscStmtWithLocation, hscStmt)
import HsSyn
import HscTypes
import BasicTypes ( HValue )
import InstEnv
import FamInstEnv ( FamInst, orphNamesOfFamInst )
import TyCon
import Type     hiding( typeKind )
import TcType           hiding( typeKind )
import Var
import Id hiding (setIdExported)
import Name             hiding ( varName )
import NameSet
import Avail
import RdrName
import VarSet
import VarEnv
import ByteCodeInstr
import Linker
import DynFlags
import Unique
import UniqSupply
import Module
import Panic
import UniqFM
import Maybes
import ErrUtils
import SrcLoc
import BreakArray
import RtClosureInspect
import Outputable hiding ((<>))
import FastString
import MonadUtils

import System.Mem.Weak
import System.Directory
import Data.Dynamic
import Data.Either
import Data.List (find)
import Control.Monad
#if __GLASGOW_HASKELL__ >= 709
import Foreign
#else
import Foreign.Safe
#endif
import Foreign.C
import GHC.Exts
import Data.Array
import Exception
import Control.Concurrent
import System.IO.Unsafe
import ForeignCall

-- for HscMain things

import Util
import CorePrep
import Parser
import TcRnDriver
import Bag
import qualified Lexer
import Lexer
import StringBuffer
import TidyPgm
import CoreLint hiding (showPass)
import Desugar
import CoreSyn
import ConLike
import TcRnTypes
import UniqSet
import qualified Data.Text as T
import qualified Gen2.Generator as Gen2
import CoreToStg
import SimplStg
import Compiler.Settings
import System.Process
import System.IO
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Gen2.Object
import Gen2.Base
import Data.ByteString (ByteString)
import qualified Gen2.Linker as Gen2
import qualified Gen2.Shim as Gen2
import Packages
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Base16         as B16
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import           Gen2.Rts                 (rtsText, rtsDeclsText)
import qualified Gen2.ClosureInfo as Gen2
import qualified CoreUtils
import qualified Data.List as L
import qualified Gen2.Object as Object
import Encoding
import Gen2.Utils (showIndent, encodeUnique, globaliseIdWith, decl)
import FloatOut
import CoreMonad (FloatOutSwitches(..))
import Control.Lens.Plated
import Data.Data.Lens
import Control.Lens
import Text.PrettyPrint.Leijen.Text (renderPretty, displayT)
import Gen2.Printer (pretty)
import Gen2.RtsTypes
import Compiler.JMacro
import IdInfo
import Data.List (foldl')
import TysWiredIn

import InteractiveEval (RunResult(..), Status(..), Resume(..), History(..),
                        SingleStep(..))

data RunnerState = RunnerState { runnerProcess   :: ProcessHandle
                               , runnerIn        :: Handle
                               , runnerErr       :: MVar (Int, ByteString)
                               , runnerBase      :: MVar (Maybe Base)
                               , runnerRtsLoaded :: MVar Bool
                               , runnerThread    :: ThreadId
                               , runnerSymbols   :: MVar (Map Id Text)
                               }

-- -----------------------------------------------------------------------------
-- running a statement interactively

getResumeContext :: GhcMonad m => m [Resume]
getResumeContext = withSession (return . ic_resume . hsc_IC)
{-
data SingleStep
   = RunToCompletion
   | SingleStep
   | RunAndLogSteps
-}
isStep :: SingleStep -> Bool
isStep RunToCompletion = False
isStep _ = True

mkHistory :: HscEnv -> HValue -> BreakInfo -> History
mkHistory hsc_env hval bi = let
    decls = findEnclosingDecls hsc_env bi
    in History hval bi decls


getHistoryModule :: History -> Module
getHistoryModule = breakInfo_module . historyBreakInfo

getHistorySpan :: HscEnv -> History -> SrcSpan
getHistorySpan hsc_env hist =
   let inf = historyBreakInfo hist
       num = breakInfo_number inf
   in case lookupUFM (hsc_HPT hsc_env) (moduleName (breakInfo_module inf)) of
       Just hmi -> modBreaks_locs (getModBreaks hmi) ! num
       _ -> panic "getHistorySpan"

getModBreaks :: HomeModInfo -> ModBreaks
getModBreaks hmi
  | Just linkable <- hm_linkable hmi,
    [BCOs _ modBreaks] <- linkableUnlinked linkable
  = modBreaks
  | otherwise
  = emptyModBreaks -- probably object code

{- | Finds the enclosing top level function name -}
-- ToDo: a better way to do this would be to keep hold of the decl_path computed
-- by the coverage pass, which gives the list of lexically-enclosing bindings
-- for each tick.
findEnclosingDecls :: HscEnv -> BreakInfo -> [String]
findEnclosingDecls hsc_env inf =
   let hmi = expectJust "findEnclosingDecls" $
             lookupUFM (hsc_HPT hsc_env) (moduleName $ breakInfo_module inf)
       mb = getModBreaks hmi
   in modBreaks_decls mb ! breakInfo_number inf

-- | Update fixity environment in the current interactive context.
updateFixityEnv :: GhcMonad m => FixityEnv -> m ()
updateFixityEnv fix_env = do
  hsc_env <- getSession
  let ic = hsc_IC hsc_env
  setSession $ hsc_env { hsc_IC = ic { ic_fix_env = fix_env } }

-- | Run a statement in the current interactive context.  Statement
-- may bind multple values.
runStmt :: GhcMonad m => GhcjsEnv -> GhcjsSettings -> RunnerState -> String -> SingleStep -> m RunResult
runStmt js_env js_settings rs = runStmtWithLocation js_env js_settings rs "<interactive>" 1

-- | Run a statement in the current interactive context.  Passing debug information
--   Statement may bind multple values.
runStmtWithLocation :: GhcMonad m => GhcjsEnv -> GhcjsSettings -> RunnerState -> String -> Int ->
                       String -> SingleStep -> m RunResult
runStmtWithLocation js_env js_settings rs source linenumber expr step =
  do
    hsc_env <- getSession

    breakMVar  <- liftIO $ newEmptyMVar  -- wait on this when we hit a breakpoint
    statusMVar <- liftIO $ newEmptyMVar  -- wait on this when a computation is running

    -- Turn off -fwarn-unused-bindings when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let ic       = hsc_IC hsc_env -- use the interactive dflags
        idflags' = ic_dflags ic `wopt_unset` Opt_WarnUnusedBinds
        hsc_env' = hsc_env{ hsc_IC = ic{ ic_dflags = idflags' } }

    -- compile to value, don't run
    r <- liftIO $ ghcjsStmtWithLocation hsc_env' js_env js_settings rs expr source linenumber

    case r of
      -- empty statement / comment
      Nothing -> return (RunOk [])

      Just (tyThings, code, name, fix_env) -> do
        updateFixityEnv fix_env

        status <-
          withVirtualCWD . liftIO $ do
            ghcjsiLoadCode rs code
            ghcjsiRunActions rs [name]
          {-
            withBreakAction (isStep step) idflags' breakMVar statusMVar $ do
                liftIO $ sandboxIO idflags' statusMVar hval
-}
        let ic = hsc_IC hsc_env
            bindings = (ic_tythings ic, ic_rn_gbl_env ic)

            size = ghciHistSize idflags'

        handleRunStatus step expr bindings tyThings
                        breakMVar statusMVar status (emptyHistory size)

-------------------------------------------------------------

ghcjsiSendToRunner :: RunnerState -> Int -> ByteString -> IO ()
ghcjsiSendToRunner rs typ payload = do
  let header = BL.toStrict . runPut $ do
        putWord32be (fromIntegral $ B.length payload)
        putWord32be (fromIntegral typ)
  B.hPut (runnerIn rs) (B16.encode $ header <> payload)
  hFlush (runnerIn rs)

ghcjsiReadFromRunner :: RunnerState -> IO (Int, ByteString)
ghcjsiReadFromRunner runner = takeMVar (runnerErr runner)

ghcjsiLoadInitialCode :: RunnerState -> ByteString -> IO ()
ghcjsiLoadInitialCode rs code = do
  takeMVar (runnerRtsLoaded rs)
  ghcjsiLoadInitialCode' rs code
  putMVar (runnerRtsLoaded rs) True

ghcjsiLoadInitialCode' :: RunnerState -> ByteString -> IO ()
ghcjsiLoadInitialCode' rs code = do
  ghcjsiSendToRunner rs 0 code
  ghcjsiReadFromRunner rs
  return ()
  
ghcjsiLoadCode :: RunnerState -> ByteString -> IO ()
ghcjsiLoadCode rs code = do
  rl <- takeMVar (runnerRtsLoaded rs)
  if not rl
    then ghcjsiLoadInitialCode' rs code
    else do
      ghcjsiSendToRunner rs 1 code
      ghcjsiReadFromRunner rs
      return ()
  putMVar (runnerRtsLoaded rs) True

ghcjsiRunActions :: RunnerState -> [Text] -> IO Status -- RunResult
ghcjsiRunActions rs actions = do
  forM_ actions $ \a -> do
    ghcjsiSendToRunner rs 2 (TE.encodeUtf8 a)
    ghcjsiReadFromRunner rs
  return (Complete $ Right []) -- fixme return bound stuff
  -- return (RunOk []) -- fixme return names bound and handle exceptions

-- fixme this could lock up if we got interrupted just after getting the value from the MVar in ghcjsiRunActions
ghcjsiInterrupt :: RunnerState -> IO ()
ghcjsiInterrupt rs = withMVar (runnerRtsLoaded rs) $ \_ -> do
  ghcjsiSendToRunner rs 3 mempty
  ghcjsiReadFromRunner rs
  return ()

ghcjsStmtWithLocation :: HscEnv
                    -> GhcjsEnv
                    -> GhcjsSettings
                    -> RunnerState
                    -> String -- ^ The statement
                    -> String -- ^ The source
                    -> Int    -- ^ Starting line
                    -> IO (Maybe ([Id], ByteString, Text, FixityEnv))
ghcjsStmtWithLocation hsc_env0 js_env js_settings rs stmt source linenumber =
 runInteractiveHsc hsc_env0 $ do
    maybe_stmt <- hscParseStmtWithLocation source linenumber stmt
    case maybe_stmt of
        Nothing -> return Nothing

        Just parsed_stmt -> do
            -- Rename and typecheck it
            hsc_env <- getHscEnv
            (ids, tc_expr, fix_env) <- ioMsgMaybe $ tcRnStmt hsc_env parsed_stmt
            -- Desugar it
            ds_expr <- ioMsgMaybe $ deSugarExpr hsc_env tc_expr

            handleWarnings
            -- Then code-gen, and link it
            -- It's important NOT to have package 'interactive' as thisPackageKey
            -- for linking, else we try to link 'main' and can't find it.
            -- Whereas the linker already knows to ignore 'interactive'
            let  src_span     = srcLocSpan interactiveSrcLoc
                 m            = icInteractiveModule (hsc_IC hsc_env)
            (code, name) <- liftIO $ ghcjsCompileInteractiveCoreExpr js_env js_settings rs hsc_env src_span ds_expr ids
            return $ Just (map (globaliseIdWith m) ids, code, name, fix_env)

-- fixme this should be doable with generic traveral

idsE :: Traversal' CoreExpr Id
idsE f (Var i)         = Var  <$> f i
idsE f (App b a)       = App  <$> idsE f b <*> idsE f a
idsE f (Let b e)       = Let  <$> idsB f b <*> idsE f e
idsE f (Lam i e)       = Lam  <$> f i <*> idsE f e
idsE f (Case e i t as) = Case <$> idsE f e <*> f i <*> pure t
                              <*> traverse (\(ac, bs, e) -> (,,) <$> pure ac <*> traverse f bs <*> idsE f e) as
idsE f (Cast e c)      = Cast <$> idsE f e <*> pure c
idsE f (Tick t e)      = Tick <$> idsT f t <*> idsE f e
idsE _ x               = pure x

idsB :: Traversal' CoreBind Id
idsB f (NonRec b e) = NonRec <$> f b <*> idsE f e
idsB f (Rec bs) = Rec <$> traverse (\(b,e) -> (,) <$> f b <*> idsE f e) bs

idsT :: Traversal' (Tickish Id) Id
idsT f (Breakpoint i fvs) = Breakpoint i <$> traverse f fvs
idsT _ x                  = pure x

ghcjsCompileInteractiveCoreExpr :: GhcjsEnv -> GhcjsSettings -> RunnerState -> HscEnv -> SrcSpan -> CoreExpr -> [Id] -> IO (ByteString, Text)
ghcjsCompileInteractiveCoreExpr js_env settings rs hsc_env srcspan ds_expr iids = do
  bs <- ghcjsCompileInteractiveCoreBinds js_env settings rs hsc_env srcspan [bind] iids
  return (bs <> sets, symb)
  where
    m     = icInteractiveModule (hsc_IC hsc_env)
    iExpr = mkVanillaGlobal (mkExternalName (getUnique m) m (mkVarOcc "iExpr") srcspan)
                            (CoreUtils.exprType ds_expr)
    bind = NonRec iExpr ds_expr
    symb = T.pack ("h$interactiveZC" ++ zEncodeString (moduleNameString $ moduleName m) ++ "ziiExpr")
    sets = TE.encodeUtf8 . T.pack $
             "\nh$GHCJSi.loadedSymbols['" ++ T.unpack symb ++ "'] = " ++ T.unpack symb ++ ";\n"

ghcjsCompileInteractiveCoreBinds :: GhcjsEnv -> GhcjsSettings -> RunnerState -> HscEnv -> SrcSpan -> CoreProgram -> [Id] -> IO ByteString
ghcjsCompileInteractiveCoreBinds js_env settings rs hsc_env srcspan core_pgm iids = do
  let iids'     = map (\i -> (i, globaliseIdWith m i)) iids
      core_pgm1 = captureBindings iids' core_pgm
  core_pgm2     <- corePrepPgm hsc_env iNTERACTIVE_loc core_pgm1 []
  stg_pgm0      <- coreToStg dflags m core_pgm2
  (stg_pgm1, c) <- stg2stg dflags m stg_pgm0
  base <- takeMVar (runnerBase rs)
  let bs        = Gen2.generate settings dflags m stg_pgm1 c
      settings' = settings { gsUseBase = maybe NoBase BaseState base }
      pkgs      = L.nub $
           {-        (imp_dep_pkgs . tcg_imports $ gbl_env) ++ -}
                   concatMap (map fst . dep_pkgs .  mi_deps . hm_iface)
                             (eltsUFM $ hsc_HPT hsc_env)
  lr <- linkInteractive js_env settings' [] {-fixme-} dflags pkgs (hsc_HPT hsc_env) bs
  rts <- if isNothing base
            then do
              let rtsd = TL.encodeUtf8 rtsDeclsText
                  rts  = TL.encodeUtf8 $ rtsText dflags (Gen2.dfCgSettings dflags)
              fr <- BL.fromChunks <$> mapM (Gen2.tryReadShimFile dflags) (Gen2.linkLibRTS lr)
              return (rtsd <> fr <> rts)
            else return mempty
  lla    <- mapM (Gen2.tryReadShimFile dflags) (Gen2.linkLibA lr)
  llarch <- mapM (Gen2.readShimsArchive dflags) (Gen2.linkLibAArch lr)
  putMVar (runnerBase rs) (Just $ Gen2.linkBase lr)
  return (BL.toStrict $ rts <> BL.fromChunks lla <> BL.fromChunks llarch <> declareCapture dflags m (map snd iids') <> Gen2.linkOut lr)
  where
    dflags   = hsc_dflags hsc_env
    m        = icInteractiveModule (hsc_IC hsc_env)
    iNTERACTIVE_loc = ModLocation{ ml_hs_file   = Nothing,
                                  ml_hi_file   = panic "ghcjsCompileInteractiveCoreExpr:ml_hi_file",
                                  ml_obj_file  = panic "ghcjsCompileInteractiveCoreExpr:ml_hi_file"}

declareCapture :: DynFlags -> Module -> [Id] -> BL.ByteString
declareCapture dflags m ids = pe (mconcat $ runGen dflags m emptyUFM (mapM d ids))
  where
    d  = fmap decl . jsIdI
    pe = TL.encodeUtf8 . (<> TL.pack "\n") . displayT . renderPretty 0.8 150 . pretty
         

{-
  rewrite: let x = y in z ->
                let x = y in case capture cx x of z
            \x -> y to
                \x -> case capture cx x of y
-}
captureBindings :: [(Id,Id)] -> CoreProgram -> CoreProgram
captureBindings bs pgm = map tb pgm
  where
    repl = listToUFM bs
    r i  = lookupUFM repl i
    tb (NonRec i e) = NonRec i (te e)
    tb (Rec bs)     = Rec (map (\(i,e) -> (i, te e)) bs)
    te (Lam i e) | Just i' <- r i = Lam i $ capture i i' (te e)
    te (Lam i e)                  = Lam i (te e)
    te (App e a) = App (te e) (te a)
    te (Let b e) = tbe b (te e)
    te (Case e i t as) | Just i' <- r i =
      Case (te e) i t (mapAlts (capture i i' . te) as)
    te (Case e i t as) = Case (te e) i t (mapAlts te as)
    te (Cast e c) = Cast (te e) c
    te (Tick t e) = Tick t (te e)
    te x          = x -- Var, Lit, Type, Coercion
    tbe (NonRec i e1) e2 | Just i' <- r i = Let (NonRec i (te e1)) (capture i i' (te e2))
    tbe (NonRec i e1) e2 = Let (NonRec i (te e1)) (te e2)
    tbe (Rec bs) e =
      let capt :: CoreExpr -> Id -> CoreExpr
          capt e i | Just i' <- r i = capture i i' e
                   | otherwise      = e
      in  Let (Rec $ map (\(b,e0) -> (b, te e0)) bs) (foldl' capt (te e) (map fst bs))
    capture i i' e = Case (App (App (Var captureOp) (Var i)) (Var i')) captureOp unitTy [(DEFAULT, [], e)]
    mapAlts f as = map (_3 %~ f) as

captureOp :: Id
captureOp = mkGlobalId (FCallId call) (mkSystemName (mkBuiltinUnique 5000) (mkVarOcc "capture")) ty info
  where
    ty   = mkFunTys [unitTy, unitTy] unitTy -- wrong
    call = CCall (CCallSpec (StaticTarget (fsLit "__ghcjsi_capture") Nothing True) PrimCallConv PlayRisky)
    -- name = mkFCallName uniq occ_str
    info = noCafIdInfo `setArityInfo` 2
{-                       `setStrictnessInfo`    strict_sig

    (_, tau)        = tcSplitForAllTys ty
    (arg_tys, _)    = tcSplitFunTys tau
    arity           = length arg_tys
    strict_sig      = mkClosedStrictSig (replicate arity evalDmd) topRes
-}
linkInteractive :: GhcjsEnv
                -> GhcjsSettings        -- settings (contains the base state)
                -> [FilePath]           -- extra js files
                -> DynFlags             -- dynamic flags
                -> [PackageKey]         -- package dependencies
                -> HomePackageTable     -- what to link
                -> ByteString     -- current module or Nothing to get the initial code + rts
                -> IO Gen2.LinkResult
linkInteractive env settings js_files dflags pkgs hpt code = do
  let home_mod_infos = eltsUFM hpt
      is_root   = const True
      linkables = map (expectJust "link".hm_linkable) home_mod_infos
      getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
      dflags'   = dflags { ways        = WayDebug : ways dflags
                         , thisPackage = interactivePackageKey
                         , verbosity   = 0
                         }
      obj_files = ObjLoaded "<interactive>" code : map ObjFile (concatMap getOfiles linkables)
      packageLibPaths :: PackageKey -> [FilePath]
      packageLibPaths pkg = maybe [] libraryDirs (lookupPackage dflags pkg)
  lr <- Gen2.link' dflags' env settings "interactive" [] pkgs obj_files js_files is_root S.empty
  let b  = Gen2.linkBase lr
      b' = b { baseUnits = S.filter (\(Object.Package p,_,_) -> p /= T.pack "interactive") (baseUnits b) }
  return $ lr { Gen2.linkBase = b' }

-------------------------------------------------------------

runDecls :: GhcMonad m => GhcjsSettings -> GhcjsEnv -> RunnerState -> String -> m [Name]
runDecls js_settings js_env rs = runDeclsWithLocation js_settings js_env rs "<interactive>" 1

runDeclsWithLocation :: GhcMonad m => GhcjsSettings -> GhcjsEnv -> RunnerState -> String -> Int -> String -> m [Name]
runDeclsWithLocation js_settings js_env rs source linenumber expr =
  do
    hsc_env <- getSession
    (tyThings, ic, bs) <- liftIO $ hscDeclsWithLocation js_settings js_env rs hsc_env expr source linenumber
    liftIO $ ghcjsiLoadCode rs bs
    setSession $ hsc_env { hsc_IC = ic }
    hsc_env <- getSession
    hsc_env' <- liftIO $ rttiEnvironment hsc_env
    modifySession (\_ -> hsc_env')
    return (map getName tyThings)


withVirtualCWD :: GhcMonad m => m a -> m a
withVirtualCWD m = do
  hsc_env <- getSession
  let ic = hsc_IC hsc_env

  let set_cwd = do
        dir <- liftIO $ getCurrentDirectory
        case ic_cwd ic of
           Just dir -> liftIO $ setCurrentDirectory dir
           Nothing  -> return ()
        return dir

      reset_cwd orig_dir = do
        virt_dir <- liftIO $ getCurrentDirectory
        hsc_env <- getSession
        let old_IC = hsc_IC hsc_env
        setSession hsc_env{  hsc_IC = old_IC{ ic_cwd = Just virt_dir } }
        liftIO $ setCurrentDirectory orig_dir

  gbracket set_cwd reset_cwd $ \_ -> m

parseImportDecl :: GhcMonad m => String -> m (ImportDecl RdrName)
parseImportDecl expr = withSession $ \hsc_env -> liftIO $ hscImport hsc_env expr

emptyHistory :: Int -> BoundedList History
emptyHistory size = nilBL size

handleRunStatus :: GhcMonad m
                => SingleStep -> String-> ([TyThing],GlobalRdrEnv) -> [Id]
                -> MVar () -> MVar Status -> Status -> BoundedList History
                -> m RunResult

handleRunStatus step expr bindings final_ids
               breakMVar statusMVar status history
  | RunAndLogSteps <- step = tracing
  | otherwise              = not_tracing
 where
  tracing
    | Break is_exception apStack info tid <- status
    , not is_exception
    = do
       hsc_env <- getSession
       b <- liftIO $ isBreakEnabled hsc_env info
       if b
         then not_tracing
           -- This breakpoint is explicitly enabled; we want to stop
           -- instead of just logging it.
         else do
           let history' = mkHistory hsc_env apStack info `consBL` history
                 -- probably better make history strict here, otherwise
                 -- our BoundedList will be pointless.
           _ <- liftIO $ evaluate history'
           status <- withBreakAction True (hsc_dflags hsc_env)
                                     breakMVar statusMVar $ do
                     liftIO $ mask_ $ do
                        putMVar breakMVar ()  -- awaken the stopped thread
                        redirectInterrupts tid $
                          takeMVar statusMVar   -- and wait for the result
           handleRunStatus RunAndLogSteps expr bindings final_ids
                           breakMVar statusMVar status history'
    | otherwise
    = not_tracing

  not_tracing
    -- Hit a breakpoint
    | Break is_exception apStack info tid <- status
    = do
         hsc_env <- getSession
         let mb_info | is_exception = Nothing
                     | otherwise    = Just info
         (hsc_env1, names, span) <- liftIO $
           bindLocalsAtBreakpoint hsc_env apStack mb_info
         let
           resume = Resume
             { resumeStmt = expr, resumeThreadId = tid
             , resumeBreakMVar = breakMVar, resumeStatMVar = statusMVar
             , resumeBindings = bindings, resumeFinalIds = final_ids
             , resumeApStack = apStack, resumeBreakInfo = mb_info
             , resumeSpan = span, resumeHistory = toListBL history
             , resumeHistoryIx = 0 }
           hsc_env2 = pushResume hsc_env1 resume
  
         modifySession (\_ -> hsc_env2)
         return (RunBreak tid names mb_info)
  
    -- Completed with an exception
    | Complete (Left e) <- status
    = return (RunException e)
  
    -- Completed successfully
    | Complete (Right hvals) <- status
    = do hsc_env <- getSession
         let final_ic = extendInteractiveContextWithIds (hsc_IC hsc_env) final_ids
             final_names = map getName final_ids
         liftIO $ Linker.extendLinkEnv (zip final_names hvals)
         hsc_env' <- liftIO $ rttiEnvironment hsc_env{hsc_IC=final_ic}
         modifySession (\_ -> hsc_env')
         return (RunOk final_names)
  
    | otherwise
    = panic "handleRunStatus"  -- The above cases are in fact exhaustive

isBreakEnabled :: HscEnv -> BreakInfo -> IO Bool
isBreakEnabled hsc_env inf =
   case lookupUFM (hsc_HPT hsc_env) (moduleName (breakInfo_module inf)) of
       Just hmi -> do
         w <- getBreak (hsc_dflags hsc_env)
                       (modBreaks_flags (getModBreaks hmi))
                       (breakInfo_number inf)
         case w of Just n -> return (n /= 0); _other -> return False
       _ ->
         return False


foreign import ccall "&rts_stop_next_breakpoint" stepFlag      :: Ptr CInt
foreign import ccall "&rts_stop_on_exception"    exceptionFlag :: Ptr CInt

setStepFlag :: IO ()
setStepFlag = poke stepFlag 1
resetStepFlag :: IO ()
resetStepFlag = poke stepFlag 0

-- this points to the IO action that is executed when a breakpoint is hit
foreign import ccall "&rts_breakpoint_io_action"
   breakPointIOAction :: Ptr (StablePtr (Bool -> BreakInfo -> HValue -> IO ()))

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
--
-- Careful here: there may be ^C exceptions flying around, so we start the new
-- thread blocked (forkIO inherits mask from the parent, #1048), and unblock
-- only while we execute the user's code.  We can't afford to lose the final
-- putMVar, otherwise deadlock ensues. (#1583, #1922, #1946)
sandboxIO :: DynFlags -> MVar Status -> IO [HValue] -> IO Status
sandboxIO dflags statusMVar thing =
   mask $ \restore -> -- fork starts blocked
     let runIt = liftM Complete $ try (restore $ rethrow dflags thing)
     in if gopt Opt_GhciSandbox dflags
        then do tid <- forkIO $ do res <- runIt
                                   putMVar statusMVar res -- empty: can't block
                redirectInterrupts tid $
                  takeMVar statusMVar

        else -- GLUT on OS X needs to run on the main thread. If you
             -- try to use it from another thread then you just get a
             -- white rectangle rendered. For this, or anything else
             -- with such restrictions, you can turn the GHCi sandbox off
             -- and things will be run in the main thread.
             --
             -- BUT, note that the debugging features (breakpoints,
             -- tracing, etc.) need the expression to be running in a
             -- separate thread, so debugging is only enabled when
             -- using the sandbox.
             runIt

--
-- While we're waiting for the sandbox thread to return a result, if
-- the current thread receives an asynchronous exception we re-throw
-- it at the sandbox thread and continue to wait.
--
-- This is for two reasons:
--
--  * So that ^C interrupts runStmt (e.g. in GHCi), allowing the
--    computation to run its exception handlers before returning the
--    exception result to the caller of runStmt.
--
--  * clients of the GHC API can terminate a runStmt in progress
--    without knowing the ThreadId of the sandbox thread (#1381)
--
-- NB. use a weak pointer to the thread, so that the thread can still
-- be considered deadlocked by the RTS and sent a BlockedIndefinitely
-- exception.  A symptom of getting this wrong is that conc033(ghci)
-- will hang.
--
redirectInterrupts :: ThreadId -> IO a -> IO a
redirectInterrupts target wait
  = do wtid <- mkWeakThreadId target
       wait `catch` \e -> do
          m <- deRefWeak wtid
          case m of
            Nothing -> wait
            Just target -> do throwTo target (e :: SomeException); wait

-- We want to turn ^C into a break when -fbreak-on-exception is on,
-- but it's an async exception and we only break for sync exceptions.
-- Idea: if we catch and re-throw it, then the re-throw will trigger
-- a break.  Great - but we don't want to re-throw all exceptions, because
-- then we'll get a double break for ordinary sync exceptions (you'd have
-- to :continue twice, which looks strange).  So if the exception is
-- not "Interrupted", we unset the exception flag before throwing.
--
rethrow :: DynFlags -> IO a -> IO a
rethrow dflags io = Exception.catch io $ \se -> do
                   -- If -fbreak-on-error, we break unconditionally,
                   --  but with care of not breaking twice
                if gopt Opt_BreakOnError dflags &&
                   not (gopt Opt_BreakOnException dflags)
                    then poke exceptionFlag 1
                    else case fromException se of
                         -- If it is a "UserInterrupt" exception, we allow
                         --  a possible break by way of -fbreak-on-exception
                         Just UserInterrupt -> return ()
                         -- In any other case, we don't want to break
                         _ -> poke exceptionFlag 0

                Exception.throwIO se

-- This function sets up the interpreter for catching breakpoints, and
-- resets everything when the computation has stopped running.  This
-- is a not-very-good way to ensure that only the interactive
-- evaluation should generate breakpoints.
withBreakAction :: (ExceptionMonad m, MonadIO m) =>
                   Bool -> DynFlags -> MVar () -> MVar Status -> m a -> m a
withBreakAction step dflags breakMVar statusMVar act
 = gbracket (liftIO setBreakAction) (liftIO . resetBreakAction) (\_ -> act)
 where
   setBreakAction = do
     stablePtr <- newStablePtr onBreak
     poke breakPointIOAction stablePtr
     when (gopt Opt_BreakOnException dflags) $ poke exceptionFlag 1
     when step $ setStepFlag
     return stablePtr
        -- Breaking on exceptions is not enabled by default, since it
        -- might be a bit surprising.  The exception flag is turned off
        -- as soon as it is hit, or in resetBreakAction below.

   onBreak is_exception info apStack = do
     tid <- myThreadId
     putMVar statusMVar (Break is_exception apStack info tid)
     takeMVar breakMVar

   resetBreakAction stablePtr = do
     poke breakPointIOAction noBreakStablePtr
     poke exceptionFlag 0
     resetStepFlag
     freeStablePtr stablePtr

noBreakStablePtr :: StablePtr (Bool -> BreakInfo -> HValue -> IO ())
noBreakStablePtr = unsafePerformIO $ newStablePtr noBreakAction

noBreakAction :: Bool -> BreakInfo -> HValue -> IO ()
noBreakAction False _ _ = putStrLn "*** Ignoring breakpoint"
noBreakAction True  _ _ = return () -- exception: just continue

resume :: GhcMonad m => (SrcSpan->Bool) -> SingleStep -> m RunResult
resume canLogSpan step
 = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic

   case resume of
     [] -> liftIO $
           throwGhcExceptionIO (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        -- unbind the temporary locals by restoring the TypeEnv from
        -- before the breakpoint, and drop this Resume from the
        -- InteractiveContext.
        let (resume_tmp_te,resume_rdr_env) = resumeBindings r
            ic' = ic { ic_tythings = resume_tmp_te,
                       ic_rn_gbl_env = resume_rdr_env,
                       ic_resume   = rs }
        modifySession (\_ -> hsc_env{ hsc_IC = ic' })

        -- remove any bindings created since the breakpoint from the
        -- linker's environment
        let new_names = map getName (filter (`notElem` resume_tmp_te)
                                           (ic_tythings ic))
        liftIO $ Linker.deleteFromLinkEnv new_names

        when (isStep step) $ liftIO setStepFlag
        case r of
          Resume { resumeStmt = expr, resumeThreadId = tid
                 , resumeBreakMVar = breakMVar, resumeStatMVar = statusMVar
                 , resumeBindings = bindings, resumeFinalIds = final_ids
                 , resumeApStack = apStack, resumeBreakInfo = info, resumeSpan = span
                 , resumeHistory = hist } -> do
               withVirtualCWD $ do
                withBreakAction (isStep step) (hsc_dflags hsc_env)
                                        breakMVar statusMVar $ do
                status <- liftIO $ mask_ $ do
                             putMVar breakMVar ()
                                      -- this awakens the stopped thread...
                             redirectInterrupts tid $
                               takeMVar statusMVar
                                      -- and wait for the result
                let prevHistoryLst = fromListBL 50 hist
                    hist' = case info of
                       Nothing -> prevHistoryLst
                       Just i
                         | not $canLogSpan span -> prevHistoryLst
                         | otherwise -> mkHistory hsc_env apStack i `consBL`
                                                        fromListBL 50 hist
                handleRunStatus step expr bindings final_ids
                                breakMVar statusMVar status hist'

back :: GhcMonad m => m ([Name], Int, SrcSpan)
back  = moveHist (+1)

forward :: GhcMonad m => m ([Name], Int, SrcSpan)
forward  = moveHist (subtract 1)

moveHist :: GhcMonad m => (Int -> Int) -> m ([Name], Int, SrcSpan)
moveHist fn = do
  hsc_env <- getSession
  case ic_resume (hsc_IC hsc_env) of
     [] -> liftIO $
           throwGhcExceptionIO (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        let ix = resumeHistoryIx r
            history = resumeHistory r
            new_ix = fn ix
        --
        when (new_ix > length history) $ liftIO $
           throwGhcExceptionIO (ProgramError "no more logged breakpoints")
        when (new_ix < 0) $ liftIO $
           throwGhcExceptionIO (ProgramError "already at the beginning of the history")

        let
          update_ic apStack mb_info = do
            (hsc_env1, names, span) <- liftIO $ bindLocalsAtBreakpoint hsc_env
                                                apStack mb_info
            let ic = hsc_IC hsc_env1
                r' = r { resumeHistoryIx = new_ix }
                ic' = ic { ic_resume = r':rs }

            modifySession (\_ -> hsc_env1{ hsc_IC = ic' })

            return (names, new_ix, span)

        -- careful: we want apStack to be the AP_STACK itself, not a thunk
        -- around it, hence the cases are carefully constructed below to
        -- make this the case.  ToDo: this is v. fragile, do something better.
        if new_ix == 0
           then case r of
                   Resume { resumeApStack = apStack,
                            resumeBreakInfo = mb_info } ->
                          update_ic apStack mb_info
           else case history !! (new_ix - 1) of
                   History apStack info _ ->
                          update_ic apStack (Just info)

-- -----------------------------------------------------------------------------
-- After stopping at a breakpoint, add free variables to the environment
result_fs :: FastString
result_fs = fsLit "_result"

bindLocalsAtBreakpoint
        :: HscEnv
        -> HValue
        -> Maybe BreakInfo
        -> IO (HscEnv, [Name], SrcSpan)

-- Nothing case: we stopped when an exception was raised, not at a
-- breakpoint.  We have no location information or local variables to
-- bind, all we can do is bind a local variable to the exception
-- value.
bindLocalsAtBreakpoint hsc_env apStack Nothing = do
   let exn_fs    = fsLit "_exception"
       exn_name  = mkInternalName (getUnique exn_fs) (mkVarOccFS exn_fs) span
       e_fs      = fsLit "e"
       e_name    = mkInternalName (getUnique e_fs) (mkTyVarOccFS e_fs) span
       e_tyvar   = mkRuntimeUnkTyVar e_name liftedTypeKind
       exn_id    = Id.mkVanillaGlobal exn_name (mkTyVarTy e_tyvar)

       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContextWithIds ictxt0 [exn_id]

       span = mkGeneralSrcSpan (fsLit "<exception thrown>")
   --
   Linker.extendLinkEnv [(exn_name, unsafeCoerce# apStack)]
   return (hsc_env{ hsc_IC = ictxt1 }, [exn_name], span)

-- Just case: we stopped at a breakpoint, we have information about the location
-- of the breakpoint and the free variables of the expression.
bindLocalsAtBreakpoint hsc_env apStack (Just info) = do

   let
       mod_name  = moduleName (breakInfo_module info)
       hmi       = expectJust "bindLocalsAtBreakpoint" $
                        lookupUFM (hsc_HPT hsc_env) mod_name
       breaks    = getModBreaks hmi
       index     = breakInfo_number info
       vars      = breakInfo_vars info
       result_ty = breakInfo_resty info
       occs      = modBreaks_vars breaks ! index
       span      = modBreaks_locs breaks ! index

           -- Filter out any unboxed ids;
           -- we can't bind these at the prompt
       pointers = filter (\(id,_) -> isPointer id) vars
       isPointer id | UnaryRep ty <- repType (idType id)
                    , PtrRep <- typePrimRep ty = True
                    | otherwise                = False

       (ids, offsets) = unzip pointers

       free_tvs = mapUnionVarSet (tyVarsOfType . idType) ids
                  `unionVarSet` tyVarsOfType result_ty

   -- It might be that getIdValFromApStack fails, because the AP_STACK
   -- has been accidentally evaluated, or something else has gone wrong.
   -- So that we don't fall over in a heap when this happens, just don't
   -- bind any free variables instead, and we emit a warning.
   mb_hValues <- mapM (getIdValFromApStack apStack) (map fromIntegral offsets)
   let filtered_ids = [ id | (id, Just _hv) <- zip ids mb_hValues ]
   when (any isNothing mb_hValues) $
      debugTraceMsg (hsc_dflags hsc_env) 1 $
          text "Warning: _result has been evaluated, some bindings have been lost"

   us <- mkSplitUniqSupply 'I'
   let (us1, us2) = splitUniqSupply us
       tv_subst   = newTyVars us1 free_tvs
       new_ids    = zipWith3 (mkNewId tv_subst) occs filtered_ids (uniqsFromSupply us2)
       names      = map idName new_ids

   -- make an Id for _result.  We use the Unique of the FastString "_result";
   -- we don't care about uniqueness here, because there will only be one
   -- _result in scope at any time.
   let result_name = mkInternalName (getUnique result_fs)
                          (mkVarOccFS result_fs) span
       result_id   = Id.mkVanillaGlobal result_name (substTy tv_subst result_ty)

   -- for each Id we're about to bind in the local envt:
   --    - tidy the type variables
   --    - globalise the Id (Ids are supposed to be Global, apparently).
   --
   let result_ok = isPointer result_id

       all_ids | result_ok = result_id : new_ids
               | otherwise = new_ids
       id_tys = map idType all_ids
       (_,tidy_tys) = tidyOpenTypes emptyTidyEnv id_tys
       final_ids = zipWith setIdType all_ids tidy_tys
       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContextWithIds ictxt0 final_ids

   Linker.extendLinkEnv [ (name,hval) | (name, Just hval) <- zip names mb_hValues ]
   when result_ok $ Linker.extendLinkEnv [(result_name, unsafeCoerce# apStack)]
   hsc_env1 <- rttiEnvironment hsc_env{ hsc_IC = ictxt1 }
   return (hsc_env1, if result_ok then result_name:names else names, span)
  where
        -- We need a fresh Unique for each Id we bind, because the linker
        -- state is single-threaded and otherwise we'd spam old bindings
        -- whenever we stop at a breakpoint.  The InteractveContext is properly
        -- saved/restored, but not the linker state.  See #1743, test break026.
   mkNewId :: TvSubst -> OccName -> Id -> Unique -> Id
   mkNewId tv_subst occ id uniq
     = Id.mkVanillaGlobalWithInfo name ty (idInfo id)
     where
         loc    = nameSrcSpan (idName id)
         name   = mkInternalName uniq occ loc
         ty     = substTy tv_subst (idType id)

   newTyVars :: UniqSupply -> TcTyVarSet -> TvSubst
     -- Similarly, clone the type variables mentioned in the types
     -- we have here, *and* make them all RuntimeUnk tyars
   newTyVars us tvs
     = mkTopTvSubst [ (tv, mkTyVarTy (mkRuntimeUnkTyVar name (tyVarKind tv)))
                    | (tv, uniq) <- varSetElems tvs `zip` uniqsFromSupply us
                    , let name = setNameUnique (tyVarName tv) uniq ]

rttiEnvironment :: HscEnv -> IO HscEnv
rttiEnvironment hsc_env@HscEnv{hsc_IC=ic} = do
   let tmp_ids = [id | AnId id <- ic_tythings ic]
       incompletelyTypedIds =
           [id | id <- tmp_ids
               , not $ noSkolems id
               , (occNameFS.nameOccName.idName) id /= result_fs]
   hsc_env' <- foldM improveTypes hsc_env (map idName incompletelyTypedIds)
   return hsc_env'
    where
     noSkolems = isEmptyVarSet . tyVarsOfType . idType
     improveTypes hsc_env@HscEnv{hsc_IC=ic} name = do
      let tmp_ids = [id | AnId id <- ic_tythings ic]
          Just id = find (\i -> idName i == name) tmp_ids
      if noSkolems id
         then return hsc_env
         else do
           mb_new_ty <- reconstructType hsc_env 10 id
           let old_ty = idType id
           case mb_new_ty of
             Nothing -> return hsc_env
             Just new_ty -> do
              case improveRTTIType hsc_env old_ty new_ty of
               Nothing -> return $
                        WARN(True, text (":print failed to calculate the "
                                           ++ "improvement for a type")) hsc_env
               Just subst -> do
                 let dflags = hsc_dflags hsc_env
                 when (dopt Opt_D_dump_rtti dflags) $
                      printInfoForUser dflags alwaysQualify $
                      fsep [text "RTTI Improvement for", ppr id, equals, ppr subst]

                 let ic' = substInteractiveContext ic subst
                 return hsc_env{hsc_IC=ic'}

getIdValFromApStack :: HValue -> Int -> IO (Maybe HValue)
getIdValFromApStack apStack (I# stackDepth) = do
   case getApStackVal# apStack (stackDepth +# 1#) of
                                -- The +1 is magic!  I don't know where it comes
                                -- from, but this makes things line up.  --SDM
        (# ok, result #) ->
            case ok of
              0# -> return Nothing -- AP_STACK not found
              _  -> return (Just (unsafeCoerce# result))

pushResume :: HscEnv -> Resume -> HscEnv
pushResume hsc_env resume = hsc_env { hsc_IC = ictxt1 }
  where
        ictxt0 = hsc_IC hsc_env
        ictxt1 = ictxt0 { ic_resume = resume : ic_resume ictxt0 }

-- -----------------------------------------------------------------------------
-- Abandoning a resume context

abandon :: GhcMonad m => m Bool
abandon = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
   case resume of
      []    -> return False
      r:rs  -> do
         modifySession $ \_ -> hsc_env{ hsc_IC = ic { ic_resume = rs } }
         liftIO $ abandon_ r
         return True

abandonAll :: GhcMonad m => m Bool
abandonAll = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
   case resume of
      []  -> return False
      rs  -> do
         modifySession $ \_ -> hsc_env{ hsc_IC = ic { ic_resume = [] } }
         liftIO $ mapM_ abandon_ rs
         return True

-- when abandoning a computation we have to
--      (a) kill the thread with an async exception, so that the
--          computation itself is stopped, and
--      (b) fill in the MVar.  This step is necessary because any
--          thunks that were under evaluation will now be updated
--          with the partial computation, which still ends in takeMVar,
--          so any attempt to evaluate one of these thunks will block
--          unless we fill in the MVar.
--      (c) wait for the thread to terminate by taking its status MVar.  This
--          step is necessary to prevent race conditions with
--          -fbreak-on-exception (see #5975).
--  See test break010.
abandon_ :: Resume -> IO ()
abandon_ r = do
  killThread (resumeThreadId r)
  putMVar (resumeBreakMVar r) ()
  _ <- takeMVar (resumeStatMVar r)
  return ()

-- -----------------------------------------------------------------------------
-- Bounded list, optimised for repeated cons

data BoundedList a = BL
                        {-# UNPACK #-} !Int  -- length
                        {-# UNPACK #-} !Int  -- bound
                        [a] -- left
                        [a] -- right,  list is (left ++ reverse right)

nilBL :: Int -> BoundedList a
nilBL bound = BL 0 bound [] []

consBL :: a -> BoundedList a -> BoundedList a
consBL a (BL len bound left right)
  | len < bound = BL (len+1) bound (a:left) right
  | null right  = BL len     bound [a]      $! tail (reverse left)
  | otherwise   = BL len     bound (a:left) $! tail right

toListBL :: BoundedList a -> [a]
toListBL (BL _ _ left right) = left ++ reverse right

fromListBL :: Int -> [a] -> BoundedList a
fromListBL bound l = BL (length l) bound l []

-- lenBL (BL len _ _ _) = len

-- -----------------------------------------------------------------------------
-- | Set the interactive evaluation context.
--
-- (setContext imports) sets the ic_imports field (which in turn
-- determines what is in scope at the prompt) to 'imports', and
-- constructs the ic_rn_glb_env environment to reflect it.
--
-- We retain in scope all the things defined at the prompt, and kept
-- in ic_tythings.  (Indeed, they shadow stuff from ic_imports.)

setContext :: GhcMonad m => [InteractiveImport] -> m ()
setContext imports
  = do { hsc_env <- getSession
       ; let dflags = hsc_dflags hsc_env
       ; all_env_err <- liftIO $ findGlobalRdrEnv hsc_env imports
       ; case all_env_err of
           Left (mod, err) ->
               liftIO $ throwGhcExceptionIO (formatError dflags mod err)
           Right all_env -> do {
       ; let old_ic        = hsc_IC hsc_env
             final_rdr_env = all_env `icExtendGblRdrEnv` ic_tythings old_ic
       ; modifySession $ \_ ->
         hsc_env{ hsc_IC = old_ic { ic_imports    = imports
                                  , ic_rn_gbl_env = final_rdr_env }}}}
  where
    formatError dflags mod err = ProgramError . showSDoc dflags $
      text "Cannot add module" <+> ppr mod <+>
      text "to context:" <+> text err

findGlobalRdrEnv :: HscEnv -> [InteractiveImport]
                 -> IO (Either (ModuleName, String) GlobalRdrEnv)
-- Compute the GlobalRdrEnv for the interactive context
findGlobalRdrEnv hsc_env imports
  = do { idecls_env <- hscRnImportDecls hsc_env idecls
                    -- This call also loads any orphan modules
       ; return $ case partitionEithers (map mkEnv imods) of
           ([], imods_env) -> Right (foldr plusGlobalRdrEnv idecls_env imods_env)
           (err : _, _)    -> Left err }
  where
    idecls :: [LImportDecl RdrName]
    idecls = [noLoc d | IIDecl d <- imports]

    imods :: [ModuleName]
    imods = [m | IIModule m <- imports]

    mkEnv mod = case mkTopLevEnv (hsc_HPT hsc_env) mod of
      Left err -> Left (mod, err)
      Right env -> Right env

availsToGlobalRdrEnv :: ModuleName -> [AvailInfo] -> GlobalRdrEnv
availsToGlobalRdrEnv mod_name avails
  = mkGlobalRdrEnv (gresFromAvails imp_prov avails)
  where
      -- We're building a GlobalRdrEnv as if the user imported
      -- all the specified modules into the global interactive module
    imp_prov = Imported [ImpSpec { is_decl = decl, is_item = ImpAll}]
    decl = ImpDeclSpec { is_mod = mod_name, is_as = mod_name,
                         is_qual = False,
                         is_dloc = srcLocSpan interactiveSrcLoc }

mkTopLevEnv :: HomePackageTable -> ModuleName -> Either String GlobalRdrEnv
mkTopLevEnv hpt modl
  = case lookupUFM hpt modl of
      Nothing -> Left "not a home module"
      Just details ->
         case mi_globals (hm_iface details) of
                Nothing  -> Left "not interpreted"
                Just env -> Right env

-- | Get the interactive evaluation context, consisting of a pair of the
-- set of modules from which we take the full top-level scope, and the set
-- of modules from which we take just the exports respectively.
getContext :: GhcMonad m => m [InteractiveImport]
getContext = withSession $ \HscEnv{ hsc_IC=ic } ->
             return (ic_imports ic)

-- | Returns @True@ if the specified module is interpreted, and hence has
-- its full top-level scope available.
moduleIsInterpreted :: GhcMonad m => Module -> m Bool
moduleIsInterpreted modl = withSession $ \h ->
 if modulePackageKey modl /= thisPackage (hsc_dflags h)
        then return False
        else case lookupUFM (hsc_HPT h) (moduleName modl) of
                Just details       -> return (isJust (mi_globals (hm_iface details)))
                _not_a_home_module -> return False

-- | Looks up an identifier in the current interactive context (for :info)
-- Filter the instances by the ones whose tycons (or clases resp)
-- are in scope (qualified or otherwise).  Otherwise we list a whole lot too many!
-- The exact choice of which ones to show, and which to hide, is a judgement call.
--      (see Trac #1581)
getInfo :: GhcMonad m => Bool -> Name -> m (Maybe (TyThing,Fixity,[ClsInst],[FamInst]))
getInfo allInfo name
  = withSession $ \hsc_env ->
    do mb_stuff <- liftIO $ hscTcRnGetInfo hsc_env name
       case mb_stuff of
         Nothing -> return Nothing
         Just (thing, fixity, cls_insts, fam_insts) -> do
           let rdr_env = ic_rn_gbl_env (hsc_IC hsc_env)

           -- Filter the instances based on whether the constituent names of their
           -- instance heads are all in scope.
           let cls_insts' = filter (plausible rdr_env . orphNamesOfClsInst) cls_insts
               fam_insts' = filter (plausible rdr_env . orphNamesOfFamInst) fam_insts
           return (Just (thing, fixity, cls_insts', fam_insts'))
  where
    plausible rdr_env names
          -- Dfun involving only names that are in ic_rn_glb_env
        = allInfo
       || all ok (nameSetElems names)
        where   -- A name is ok if it's in the rdr_env,
                -- whether qualified or not
          ok n | n == name         = True       -- The one we looked for in the first place!
               | isBuiltInSyntax n = True
               | isExternalName n  = any ((== n) . gre_name)
                                         (lookupGRE_Name rdr_env n)
               | otherwise         = True

-- | Returns all names in scope in the current interactive context
getNamesInScope :: GhcMonad m => m [Name]
getNamesInScope = withSession $ \hsc_env -> do
  return (map gre_name (globalRdrEnvElts (ic_rn_gbl_env (hsc_IC hsc_env))))

getRdrNamesInScope :: GhcMonad m => m [RdrName]
getRdrNamesInScope = withSession $ \hsc_env -> do
  let
      ic = hsc_IC hsc_env
      gbl_rdrenv = ic_rn_gbl_env ic
      gbl_names = concatMap greToRdrNames $ globalRdrEnvElts gbl_rdrenv
  return gbl_names


-- ToDo: move to RdrName
greToRdrNames :: GlobalRdrElt -> [RdrName]
greToRdrNames GRE{ gre_name = name, gre_prov = prov }
  = case prov of
     LocalDef -> [unqual]
     Imported specs -> concat (map do_spec (map is_decl specs))
  where
    occ = nameOccName name
    unqual = Unqual occ
    do_spec decl_spec
        | is_qual decl_spec = [qual]
        | otherwise         = [unqual,qual]
        where qual = Qual (is_as decl_spec) occ

-- | Parses a string as an identifier, and returns the list of 'Name's that
-- the identifier can refer to in the current interactive context.
parseName :: GhcMonad m => String -> m [Name]
parseName str = withSession $ \hsc_env -> liftIO $
   do { lrdr_name <- hscParseIdentifier hsc_env str
      ; hscTcRnLookupRdrName hsc_env lrdr_name }
-- -----------------------------------------------------------------------------
-- Getting the type of an expression

-- | Get the type of an expression
-- Returns its most general type
exprType :: GhcMonad m => String -> m Type
exprType expr = withSession $ \hsc_env -> do
   ty <- liftIO $ hscTcExpr hsc_env expr
   return $ tidyType emptyTidyEnv ty

-- -----------------------------------------------------------------------------
-- Getting the kind of a type

-- | Get the kind of a  type
typeKind  :: GhcMonad m => Bool -> String -> m (Type, Kind)
typeKind normalise str = withSession $ \hsc_env -> do
   liftIO $ hscKcType hsc_env normalise str

showModule :: GhcMonad m => ModSummary -> m String
showModule mod_summary =
    withSession $ \hsc_env -> do
        interpreted <- isModuleInterpreted mod_summary
        let dflags = hsc_dflags hsc_env
        return (showModMsg dflags (hscTarget dflags) interpreted mod_summary)

isModuleInterpreted :: GhcMonad m => ModSummary -> m Bool
isModuleInterpreted mod_summary = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) (ms_mod_name mod_summary) of
        Nothing       -> panic "missing linkable"
        Just mod_info -> return (not obj_linkable)
                      where
                         obj_linkable = isObjectLinkable (expectJust "showModule" (hm_linkable mod_info))

----------------------------------------------------------------------------
-- RTTI primitives

obtainTermFromVal :: HscEnv -> Int -> Bool -> Type -> a -> IO Term
obtainTermFromVal hsc_env bound force ty x =
              cvObtainTerm hsc_env bound force ty (unsafeCoerce# x)

obtainTermFromId :: HscEnv -> Int -> Bool -> Id -> IO Term
obtainTermFromId hsc_env bound force id =  do
              hv <- Linker.getHValue hsc_env (varName id)
              cvObtainTerm hsc_env bound force (idType id) hv

-- Uses RTTI to reconstruct the type of an Id, making it less polymorphic
reconstructType :: HscEnv -> Int -> Id -> IO (Maybe Type)
reconstructType hsc_env bound id = do
              hv <- Linker.getHValue hsc_env (varName id)
              cvReconstructType hsc_env bound (idType id) hv

mkRuntimeUnkTyVar :: Name -> Kind -> TyVar
mkRuntimeUnkTyVar name kind = mkTcTyVar name kind RuntimeUnk

hscDecls :: GhcjsSettings
         -> GhcjsEnv
         -> RunnerState
         -> HscEnv
         -> String -- ^ The statement
         -> IO ([TyThing], InteractiveContext, ByteString)
hscDecls js_settings js_env rs hsc_env str = hscDeclsWithLocation js_settings js_env rs hsc_env str "<interactive>" 1

-- | Compile a decls
hscDeclsWithLocation :: GhcjsSettings
                     -> GhcjsEnv
                     -> RunnerState
                     -> HscEnv
                     -> String -- ^ The statement
                     -> String -- ^ The source
                     -> Int    -- ^ Starting line
                     -> IO ([TyThing], InteractiveContext, ByteString)
hscDeclsWithLocation js_settings js_env rs hsc_env0 str source linenumber =
 runInteractiveHsc hsc_env0 $ do
    L _ (HsModule{ hsmodDecls = decls }) <-
        hscParseThingWithLocation source linenumber parseModule str

    {- Rename and typecheck it -}
    hsc_env <- getHscEnv
    tc_gblenv <- ioMsgMaybe $ tcRnDeclsi hsc_env decls

    {- Grab the new instances -}
    -- We grab the whole environment because of the overlapping that may have
    -- been done. See the notes at the definition of InteractiveContext
    -- (ic_instances) for more details.
    let defaults = tcg_default tc_gblenv

    {- Desugar it -}
    -- We use a basically null location for iNTERACTIVE
    let iNTERACTIVELoc = ModLocation{ ml_hs_file   = Nothing,
                                      ml_hi_file   = panic "hsDeclsWithLocation:ml_hi_file",
                                      ml_obj_file  = panic "hsDeclsWithLocation:ml_hi_file"}
    ds_result <- hscDesugar' iNTERACTIVELoc tc_gblenv

    {- Simplify -}
    simpl_mg <- liftIO $ hscSimplify hsc_env ds_result

    {- Tidy -}
    (tidy_cg, mod_details) <- liftIO $ tidyProgram hsc_env simpl_mg

    let dflags = hsc_dflags hsc_env
        !CgGuts{ cg_module    = this_mod,
                 cg_binds     = core_binds,
                 cg_tycons    = tycons,
                 cg_modBreaks = mod_breaks } = tidy_cg

        !ModDetails { md_insts     = cls_insts
                    , md_fam_insts = fam_insts } = mod_details
            -- Get the *tidied* cls_insts and fam_insts

        data_tycons = filter isDataTyCon tycons

    {- Prepare For Code Generation -}
    -- Do saturation and convert to A-normal form
    prepd_binds <- {-# SCC "CorePrep" #-}
      liftIO $ corePrepPgm hsc_env iNTERACTIVELoc core_binds data_tycons

    let src_span = srcLocSpan interactiveSrcLoc
    bs <- liftIO $ ghcjsCompileInteractiveCoreBinds js_env js_settings rs hsc_env src_span prepd_binds []
 
    let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)
        patsyns = mg_patsyns simpl_mg

        ext_ids = [ id | id <- bindersOfBinds core_binds
                       , isExternalName (idName id)
                       , not (isDFunId id || isImplicitId id) ]
            -- We only need to keep around the external bindings
            -- (as decided by TidyPgm), since those are the only ones
            -- that might be referenced elsewhere.
            -- The DFunIds are in 'cls_insts' (see Note [ic_tythings] in HscTypes
            -- Implicit Ids are implicit in tcs

        tythings =  map AnId ext_ids ++ map ATyCon tcs ++ map (AConLike . PatSynCon) patsyns

    let icontext = hsc_IC hsc_env
        ictxt    = extendInteractiveContext icontext ext_ids tcs
                                            cls_insts fam_insts defaults patsyns
    return (tythings, ictxt, bs)

hscImport :: HscEnv -> String -> IO (ImportDecl RdrName)
hscImport hsc_env str = runInteractiveHsc hsc_env $ do
    (L _ (HsModule{hsmodImports=is})) <-
       hscParseThing parseModule str
    case is of
        [L _ i] -> return i
        _ -> liftIO $ throwOneError $
                 mkPlainErrMsg (hsc_dflags hsc_env) noSrcSpan $
                     ptext (sLit "parse error in import declaration")

-- | Typecheck an expression (but don't run it)
-- Returns its most general type
hscTcExpr :: HscEnv
          -> String -- ^ The expression
          -> IO Type
hscTcExpr hsc_env0 expr = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    maybe_stmt <- hscParseStmt expr
    case maybe_stmt of
        Just (L _ (BodyStmt expr _ _ _)) ->
            ioMsgMaybe $ tcRnExpr hsc_env expr
        _ ->
            throwErrors $ unitBag $ mkPlainErrMsg (hsc_dflags hsc_env) noSrcSpan
                (text "not an expression:" <+> quotes (text expr))

-- | Find the kind of a type
-- Currently this does *not* generalise the kinds of the type
hscKcType
  :: HscEnv
  -> Bool            -- ^ Normalise the type
  -> String          -- ^ The type as a string
  -> IO (Type, Kind) -- ^ Resulting type (possibly normalised) and kind
hscKcType hsc_env0 normalise str = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ tcRnType hsc_env normalise ty

hscParseStmt :: String -> Hsc (Maybe (GhciLStmt RdrName))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int -> String
                         -> Hsc (Maybe (GhciLStmt RdrName))
hscParseStmtWithLocation source linenumber stmt =
    hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType RdrName)
hscParseType = hscParseThing parseType

hscParseThing :: (Outputable thing) => Lexer.P thing -> String -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str
  = {-# SCC "Parser" #-} do
    dflags <- getDynFlags
    liftIO $ showPass dflags "Parser"

    let buf = stringToStringBuffer str
        loc = mkRealSrcLoc (fsLit source) linenumber 1

    case unP parser (mkPState dflags buf loc) of
        PFailed span err -> do
            let msg = mkPlainErrMsg dflags span err
            throwErrors $ unitBag msg

        POk pst thing -> do
            logWarningsReportErrors (getMessages pst)
            liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr thing)
            return thing

-- -----------------------------------------------------------------------------

getWarnings :: Hsc WarningMessages
getWarnings = Hsc $ \_ w -> return (w, w)

clearWarnings :: Hsc ()
clearWarnings = Hsc $ \_ _ -> return ((), emptyBag)

logWarnings :: WarningMessages -> Hsc ()
logWarnings w = Hsc $ \_ w0 -> return ((), w0 `unionBags` w)

-- getHscEnv :: Hsc HscEnv
-- getHscEnv = Hsc $ \e w -> return (e, w)

handleWarnings :: Hsc ()
handleWarnings = do
    dflags <- getDynFlags
    w <- getWarnings
    liftIO $ printOrThrowWarnings dflags w
    clearWarnings

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: Messages -> Hsc ()
logWarningsReportErrors (warns,errs) = do
    logWarnings warns
    when (not $ isEmptyBag errs) $ throwErrors errs

-- | Throw some errors.
throwErrors :: ErrorMessages -> Hsc a
throwErrors = liftIO . throwIO . mkSrcErr

-- | Deal with errors and warnings returned by a compilation step
--
-- In order to reduce dependencies to other parts of the compiler, functions
-- outside the "main" parts of GHC return warnings and errors as a parameter
-- and signal success via by wrapping the result in a 'Maybe' type. This
-- function logs the returned warnings and propagates errors as exceptions
-- (of type 'SourceError').
--
-- This function assumes the following invariants:
--
--  1. If the second result indicates success (is of the form 'Just x'),
--     there must be no error messages in the first result.
--
--  2. If there are no error messages, but the second result indicates failure
--     there should be warnings in the first result. That is, if the action
--     failed, it must have been due to the warnings (i.e., @-Werror@).
ioMsgMaybe :: IO (Messages, Maybe a) -> Hsc a
ioMsgMaybe ioA = do
    ((warns,errs), mb_r) <- liftIO ioA
    logWarnings warns
    case mb_r of
        Nothing -> throwErrors errs
        Just r  -> ASSERT( isEmptyBag errs ) return r

-- | like ioMsgMaybe, except that we ignore error messages and return
-- 'Nothing' instead.
ioMsgMaybe' :: IO (Messages, Maybe a) -> Hsc (Maybe a)
ioMsgMaybe' ioA = do
    ((warns,_errs), mb_r) <- liftIO $ ioA
    logWarnings warns
    return mb_r
