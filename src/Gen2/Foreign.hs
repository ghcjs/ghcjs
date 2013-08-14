{-
  This module takes over desugaring and typechecking foreign declarations and calls
  from GHC. foreign import javascript should be desugared differently
  from other foreign imports since we don't want Bool to de be marshalled through
  0/1 for example.

  Contains code adapted from DsForeign and DsCCall
 -}

module Gen2.Foreign where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16

import Data.Maybe
import Data.List (partition)

import Hooks
import DynFlags

import Id
import OrdList
import Name
import Bag
import CoreSyn
import HscTypes
import HsBinds
import HsDecls
import DsForeign
import DsMonad
import TcRnTypes
import TcForeign
import TcType
import MonadUtils
import RdrName
import FastString
import ForeignCall
import SrcLoc
import Unique
import PrelNames
import PrimOp
import MkId
import TyCon
import DataCon
import Outputable
import Coercion
import Type
import TysWiredIn
import TysPrim
import CoreUtils
import MkCore
import DsCCall
import BasicTypes
import CoreUnfold
import Pair
import Literal
import Module

installForeignHooks :: Bool -> DynFlags -> DynFlags
installForeignHooks generatingJs df = df { hooks = f generatingJs (hooks df) }
  where
    f True  = insertHook DsForeignsHook       ghcjsDsForeigns
            . insertHook TcForeignImportsHook ghcjsTcForeignImports
            . insertHook TcForeignExportsHook ghcjsTcForeignExports
    f False = insertHook DsForeignsHook       ghcjsNativeDsForeigns
            . insertHook TcForeignImportsHook ghcjsNativeTcForeignImports
            . insertHook TcForeignExportsHook ghcjsTcForeignExports

{-
   desugar foreign declarations for JavaScript
-}
ghcjsDsForeigns :: [LForeignDecl Id]
                -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsDsForeigns fos = do
  (stubs, ret) <- dsForeigns' fos'
  retJs <- mapM imp_js jsImports
  let jsBs = foldr (appOL . toOL . fst3) nilOL retJs
      fst3 (x,_,_) = x
  return (stubs, ret `appOL` jsBs)
    where
      imp_js (L loc fimp) = putSrcSpanDs loc (dsJsImport fimp)
      (jsImports, fos') = partition isJsImport fos
      isJsImport (L _ (ForeignImport _ _ _ (CImport _ _ _ _))) = True
      isJsImport _ = False

dsJsImport :: ForeignDecl Id -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsJsImport (ForeignImport (L _ id) t co (CImport cconv safety mHeader spec)) =
  case spec of
-- fixme do we need something special here?
    CLabel cid -> do
      dflags <- getDynFlags
      let ty = pFst $ coercionKind co
          fod = case tyConAppTyCon_maybe (dropForAlls ty) of
                  Just tycon
                    | tyConUnique tycon == funPtrTyConKey -> IsFunction
                  _ -> IsData
      (resTy, foRhs) <- jsResultWrapper ty
      let
        rhs = foRhs (Lit (MachLabel cid stdcall_info fod))
        rhs' = Cast rhs co
        stdcall_info = fun_type_arg_stdcall_info dflags cconv ty
--      in
      return ([(id, rhs')], empty, empty)
-- no special primcalls
--    CFunction target | cconv == PrimCallConv ->
--      dsPrimCall id co (CCall (CCallSpec target cconv safety))
    CFunction target ->
      dsJsCall id co (CCall (CCallSpec target cconv safety)) mHeader
    CWrapper ->
      dsFExportDynamic id co cconv
dsJsImport _ = error "dsJsImport"

dsJsCall :: Id -> Coercion -> ForeignCall -> Maybe Header
        -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsJsCall fn_id co fcall mDeclHeader = do
    let
        ty                   = pFst $ coercionKind co
        (tvs, fun_ty)        = tcSplitForAllTys ty
        (arg_tys, io_res_ty) = tcSplitFunTys fun_ty
                -- Must use tcSplit* functions because we want to
                -- see that (IO t) in the corner

    args <- newSysLocalsDs arg_tys
    (val_args, arg_wrappers) <- mapAndUnzipM unboxJsArg (map Var args)

    let work_arg_ids  = [v | Var v <- val_args] -- All guaranteed to be vars

    (ccall_result_ty, res_wrapper) <- boxJsResult io_res_ty

    ccall_uniq <- newUnique
    work_uniq  <- newUnique

    dflags <- getDynFlags
    let
        -- Build the worker
        worker_ty     = mkForAllTys tvs (mkFunTys (map idType work_arg_ids) ccall_result_ty)
        the_ccall_app = mkFCall dflags ccall_uniq fcall val_args ccall_result_ty
        work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
        work_id       = mkSysLocal (fsLit "$wccall") work_uniq worker_ty

        -- Build the wrapper
        work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
        wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkLams (tvs ++ args) wrapper_body
        wrap_rhs'    = Cast wrap_rhs co
        fn_id_w_inl  = fn_id `setIdUnfolding` mkInlineUnfolding (Just (length args)) wrap_rhs'

    return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')], empty, empty)

{-
  We unbox arguments for JS calls a bit different from native code:
    - Bool is marshalled to true/false, not 0/1
    - All int types are narrowed, since JS floats have a greater range than Int32
 -}

unboxJsArg :: CoreExpr                  -- The supplied argument
         -> DsM (CoreExpr,              -- To pass as the actual argument
                 CoreExpr -> CoreExpr   -- Wrapper to unbox the arg
                )
unboxJsArg arg
  -- Primtive types: nothing to unbox
  | isPrimitiveType arg_ty
  = return (arg, \body -> body)

  -- Recursive newtypes
  | Just(_rep_ty, co) <- splitNewTypeRepCo_maybe arg_ty
  = unboxJsArg (mkCast arg co)

  -- Booleans, do not convert to 0/1, only force them
  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` boolTyConKey
  = do dflags <- getDynFlags
       return (arg,
              \ body -> mkWildCase arg boolTy (exprType body) [(DEFAULT,[],body)])

  -- Data types with a single constructor, which has a single, primitive-typed arg
  -- This deals with Int, Float etc; also Ptr, ForeignPtr
  | is_product_type && data_con_arity == 1
    = do case_bndr <- newSysLocalDs arg_ty
         prim_arg <- newSysLocalDs data_con_arg_ty1
         return (Var prim_arg,
               \ body -> Case arg case_bndr (exprType body) [(DataAlt data_con,[prim_arg],body)]
              )

  -- Byte-arrays, both mutable and otherwise; hack warning
  -- We're looking for values of type ByteArray, MutableByteArray
  --    data ByteArray          ix = ByteArray        ix ix ByteArray#
  --    data MutableByteArray s ix = MutableByteArray ix ix (MutableByteArray# s)
  | is_product_type &&
    data_con_arity == 3 &&
    isJust maybe_arg3_tycon &&
    (arg3_tycon ==  byteArrayPrimTyCon ||
     arg3_tycon ==  mutableByteArrayPrimTyCon)
  = do case_bndr <- newSysLocalDs arg_ty
       vars@[_l_var, _r_var, arr_cts_var] <- newSysLocalsDs data_con_arg_tys
       return (Var arr_cts_var,
               \ body -> Case arg case_bndr (exprType body) [(DataAlt data_con,vars,body)]
              )

  | otherwise
  = do l <- getSrcSpanDs
       pprPanic "unboxJsArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty                                      = exprType arg
    maybe_product_type                          = splitDataProductType_maybe arg_ty
    is_product_type                             = isJust maybe_product_type
    Just (_, _, data_con, data_con_arg_tys)     = maybe_product_type
    data_con_arity                              = dataConSourceArity data_con
    (data_con_arg_ty1 : _)                      = data_con_arg_tys

    (_ : _ : data_con_arg_ty3 : _) = data_con_arg_tys
    maybe_arg3_tycon               = tyConAppTyCon_maybe data_con_arg_ty3
    Just arg3_tycon                = maybe_arg3_tycon


boxJsResult :: Type
          -> DsM (Type, CoreExpr -> CoreExpr)

-- Takes the result of the user-level ccall:
--      either (IO t),
--      or maybe just t for an side-effect-free call
-- Returns a wrapper for the primitive ccall itself, along with the
-- type of the result of the primitive ccall.  This result type
-- will be of the form
--      State# RealWorld -> (# State# RealWorld, t' #)
-- where t' is the unwrapped form of t.  If t is simply (), then
-- the result type will be
--      State# RealWorld -> (# State# RealWorld #)

boxJsResult result_ty
  | Just (io_tycon, io_res_ty) <- tcSplitIOType_maybe result_ty
        -- isIOType_maybe handles the case where the type is a 
        -- simple wrapping of IO.  E.g.
        --      newtype Wrap a = W (IO a)
        -- No coercion necessary because its a non-recursive newtype
        -- (If we wanted to handle a *recursive* newtype too, we'd need
        -- another case, and a coercion.)
        -- The result is IO t, so wrap the result in an IO constructor
  = do  { res <- jsResultWrapper io_res_ty
        ; let extra_result_tys
                = case res of
                     (Just ty,_)
                       | isUnboxedTupleType ty
                       -> let Just ls = tyConAppArgs_maybe ty in tail ls
                     _ -> []

              return_result state anss
                = mkConApp (tupleCon UnboxedTuple (2 + length extra_result_tys))
                           (map Type (realWorldStatePrimTy : io_res_ty : extra_result_tys)
                              ++ (state : anss))

        ; (ccall_res_ty, the_alt) <- mk_alt return_result res

        ; state_id <- newSysLocalDs realWorldStatePrimTy
        ; let io_data_con = head (tyConDataCons io_tycon)
              toIOCon     = dataConWrapId io_data_con

              wrap the_call =
                              mkApps (Var toIOCon)
                                     [ Type io_res_ty,
                                       Lam state_id $
                                       mkWildCase (App the_call (Var state_id))
                                             ccall_res_ty
                                             (coreAltType the_alt)
                                             [the_alt]
                                     ]

        ; return (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap) }

boxJsResult result_ty
  = do -- It isn't IO, so do unsafePerformIO
       -- It's not conveniently available, so we inline it
       res <- jsResultWrapper result_ty
       (ccall_res_ty, the_alt) <- mk_alt return_result res
       let
           wrap = \ the_call -> mkWildCase (App the_call (Var realWorldPrimId))
                                           ccall_res_ty
                                           (coreAltType the_alt)
                                           [the_alt]
       return (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap)
  where
    return_result _ [ans] = ans
    return_result _ _     = panic "return_result: expected single result"


mk_alt :: (Expr Var -> [Expr Var] -> Expr Var)
       -> (Maybe Type, Expr Var -> Expr Var)
       -> DsM (Type, (AltCon, [Id], Expr Var))
mk_alt return_result (Nothing, wrap_result)
  = do -- The ccall returns ()
       state_id <- newSysLocalDs realWorldStatePrimTy
       let
             the_rhs = return_result (Var state_id) 
                                     [wrap_result (panic "jsBoxResult")]

             ccall_res_ty = mkTyConApp unboxedSingletonTyCon [realWorldStatePrimTy]
             the_alt      = (DataAlt unboxedSingletonDataCon, [state_id], the_rhs)
       
       return (ccall_res_ty, the_alt)

mk_alt return_result (Just prim_res_ty, wrap_result)
                -- The ccall returns a non-() value
  | isUnboxedTupleType prim_res_ty= do
    let
        Just ls = tyConAppArgs_maybe prim_res_ty
        arity = 1 + length ls
    args_ids@(result_id:as) <- mapM newSysLocalDs ls
    state_id <- newSysLocalDs realWorldStatePrimTy
    let
        the_rhs = return_result (Var state_id) 
                                (wrap_result (Var result_id) : map Var as)
        ccall_res_ty = mkTyConApp (tupleTyCon UnboxedTuple arity)
                                  (realWorldStatePrimTy : ls)
        the_alt      = ( DataAlt (tupleCon UnboxedTuple arity)
                       , (state_id : args_ids)
                       , the_rhs
                       )
    return (ccall_res_ty, the_alt)

  | otherwise = do
    result_id <- newSysLocalDs prim_res_ty
    state_id <- newSysLocalDs realWorldStatePrimTy
    let
        the_rhs = return_result (Var state_id) 
                                [wrap_result (Var result_id)]
        ccall_res_ty = mkTyConApp unboxedPairTyCon [realWorldStatePrimTy, prim_res_ty]
        the_alt      = (DataAlt unboxedPairDataCon, [state_id, result_id], the_rhs)
    return (ccall_res_ty, the_alt)

fun_type_arg_stdcall_info :: DynFlags -> CCallConv -> Type -> Maybe Int
fun_type_arg_stdcall_info _ _other_conv _ = Nothing

jsResultWrapper :: Type
              -> DsM (Maybe Type,               -- Type of the expected result, if any
                      CoreExpr -> CoreExpr)     -- Wrapper for the result 
-- resultWrapper deals with the result *value*
-- E.g. foreign import foo :: Int -> IO T
-- Then resultWrapper deals with marshalling the 'T' part
--jsResultWrapper tr = resultWrapper tr
jsResultWrapper result_ty
  -- Base case 1: primitive types
  | isPrimitiveType result_ty
  = return (Just result_ty, \e -> e)

  -- Base case 2: the unit type ()
  | Just (tc,_) <- maybe_tc_app, tc `hasKey` unitTyConKey
  = return (Nothing, \_ -> Var unitDataConId)

  -- Base case 3: the boolean type
  | Just (tc,_) <- maybe_tc_app, tc `hasKey` boolTyConKey
  = do
    dflags <- getDynFlags
--    result_id <- newSysLocalDs boolTy
    ccall_uniq <- newUnique
    let forceBool e = mkJsCall dflags ccall_uniq "$r = !(!$1)" [e] boolTy
    return
     (Just intPrimTy, \e -> forceBool e)
-- Cast (forceBool e) (UnivCo Representational intPrimTy boolTy))
{-
Case (forceBool e) result_id boolTy
        [(DEFAULT,[], Cast (Var result_id) (UnivCo Representational intPrimTy boolTy))]) -}

-- e -> e -- mkIfThenElse e (Var falseDataConId) (Var trueDataConId)
{-
mkWildCase e boolTy
                                   boolTy
                                   [(DEFAULT             ,[],Var trueDataConId),
                                    (DataAlt falseDataCon,[],Var falseDataConId)] -}

  -- Recursive newtypes
  | Just (rep_ty, co) <- splitNewTypeRepCo_maybe result_ty
  = do (maybe_ty, wrapper) <- jsResultWrapper rep_ty
       return (maybe_ty, \e -> mkCast (wrapper e) (mkSymCo co))

  -- The type might contain foralls (eg. for dummy type arguments,
  -- referring to 'Ptr a' is legal).
  | Just (tyvar, rest) <- splitForAllTy_maybe result_ty
  = do (maybe_ty, wrapper) <- jsResultWrapper rest
       return (maybe_ty, \e -> Lam tyvar (wrapper e))

  -- Data types with a single constructor, which has a single arg
  -- This includes types like Ptr and ForeignPtr
  | Just (tycon, tycon_arg_tys, data_con, data_con_arg_tys) <- splitDataProductType_maybe result_ty,
    dataConSourceArity data_con == 1
  = do dflags <- getDynFlags
       let
           (unwrapped_res_ty : _) = data_con_arg_tys
           narrow_wrapper         = maybeJsNarrow dflags tycon
       (maybe_ty, wrapper) <- jsResultWrapper unwrapped_res_ty
       return
         (maybe_ty, \e -> mkApps (Var (dataConWrapId data_con))
                                 (map Type tycon_arg_tys ++ [wrapper (narrow_wrapper e)]))

  | otherwise
  = pprPanic "jsResultWrapper" (ppr result_ty)
  where
    maybe_tc_app = splitTyConApp_maybe result_ty

-- low-level primitive JavaScript call:
mkJsCall :: DynFlags -> Unique -> String -> [CoreExpr] -> Type -> CoreExpr
mkJsCall dflags u tgt args t =
  mkFCall dflags u (CCall (CCallSpec (StaticTarget (mkFastString tgt) (Just primPackageId) True)
                                      JavaScriptCallConv PlayRisky)) args t


-- When the result of a foreign call is smaller than the word size, we
-- need to sign- or zero-extend the result up to the word size.  The C
-- standard appears to say that this is the responsibility of the
-- caller, not the callee.

-- narrow int32 and word32 since JS numbers can contain more
maybeJsNarrow :: DynFlags -> TyCon -> (CoreExpr -> CoreExpr)
maybeJsNarrow dflags tycon
  | tycon `hasKey` int8TyConKey   = \e -> App (Var (mkPrimOpId Narrow8IntOp)) e
  | tycon `hasKey` int16TyConKey  = \e -> App (Var (mkPrimOpId Narrow16IntOp)) e
  | tycon `hasKey` int32TyConKey  = \e -> App (Var (mkPrimOpId Narrow32IntOp)) e
  | tycon `hasKey` word8TyConKey  = \e -> App (Var (mkPrimOpId Narrow8WordOp)) e
  | tycon `hasKey` word16TyConKey = \e -> App (Var (mkPrimOpId Narrow16WordOp)) e
  | tycon `hasKey` word32TyConKey = \e -> App (Var (mkPrimOpId Narrow32WordOp)) e
  | otherwise                     = id

{-
  desugar foreign declarations for native code: replace
  all foreign import JavaScript by foreign import CCall
  with the function name generated from a hash of the
  JavaScript contents
-}
ghcjsNativeDsForeigns :: [LForeignDecl Id]
                      -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsNativeDsForeigns fos = dsForeigns' fos

-- fixme allow additional js types here (and in the native version of this)
ghcjsTcForeignImports :: [LForeignDecl Name]
                      -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
ghcjsTcForeignImports decls =
  tcForeignImports' decls

ghcjsNativeTcForeignImports :: [LForeignDecl Name]
                                -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
ghcjsNativeTcForeignImports decls = tcForeignImports' (map f decls)
  where
    f (L l (ForeignImport n t c (CImport JavaScriptCallConv safety mheader spec))) =
      (L l (ForeignImport n t c (CImport CCallConv safety mheader (convertSpec spec))))
    f x = x
    convertSpec (CLabel fs) = CLabel (hashFs "jsCall_" fs)
    convertSpec (CFunction (StaticTarget lbl mpkg isFun)) =
      CFunction (StaticTarget (hashFs "jsCall_" lbl) mpkg isFun)
    convertSpec x = x

hashFs :: String -> FastString -> FastString
hashFs prefix fs = mkFastString . (prefix++) . T.unpack . TE.decodeUtf8
                 . B16.encode . SHA1.hash . TE.encodeUtf8
                 . T.pack . unpackFS $ fs


ghcjsTcForeignExports :: [LForeignDecl Name]
                      -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
ghcjsTcForeignExports decls = do
--  liftIO $ putStrLn "typechecking foreign exports"
  tcForeignExports' decls


