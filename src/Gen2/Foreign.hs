{-# LANGUAGE CPP #-}
{-
  This module takes over desugaring and typechecking foreign declarations and calls
  from GHC. foreign import javascript should be desugared differently
  from other foreign imports since we don't want Bool to de be marshalled through
  0/1 for example.

  Contains code adapted from DsForeign, DsCCall and TcForeign
 -}

module Gen2.Foreign where

import Control.Monad

import Data.Maybe
import Data.List (isPrefixOf, unzip4)

import Hooks
import DynFlags

import Id
import IdInfo
import OrdList
import Name
import Bag
import CoreSyn
import ErrUtils
import HscTypes
import HsBinds
import HsDecls
import DsForeign
import DsMonad
import Encoding
import HsUtils
import TcEnv
import TcExpr
import TcRnTypes
import TcForeign
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
import TcType
import TcRnMonad
import TcHsType
import Platform

import Compiler.Compat

import Gen2.PrimIface

import Data.Char
import Data.List (stripPrefix)
import Data.List.Split

type Binding = (Id, CoreExpr)

installForeignHooks :: Bool -> DynFlags -> DynFlags
installForeignHooks generatingJs dflags =
  dflags { hooks = f generatingJs $ hooks dflags }
    where
      f True h  = h { dsForeignsHook       = Just ghcjsDsForeigns
                    , tcForeignImportsHook = Just ghcjsTcForeignImports
                    , tcForeignExportsHook = Just ghcjsTcForeignExports
                    }
      f False h = h { dsForeignsHook       = Just ghcjsNativeDsForeigns
                    , tcForeignImportsHook = Just ghcjsNativeTcForeignImports
                    , tcForeignExportsHook = Just ghcjsNativeTcForeignExports
                    }
{-
   desugar foreign declarations for JavaScript
-}
ghcjsDsForeigns :: [LForeignDecl Id]
                -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsDsForeigns []
  = return (NoStubs, nilOL)
ghcjsDsForeigns fos = do
    fives <- mapM do_ldecl fos
    let
        (hs, cs, _idss, bindss) = unzip4 fives
        -- fe_ids = concat idss

    return (ForeignStubs
             (vcat hs)
             (vcat cs),
            foldr (appOL . toOL) nilOL bindss)
  where
   do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)

   do_decl (ForeignImport id _ co spec) = do
      traceIf (text "fi start" <+> ppr id)
      (bs, h, c) <- ghcjsDsFImport (unLoc id) co spec
      traceIf (text "fi end" <+> ppr id)
      return (h, c, [], bs)

   do_decl (ForeignExport (L _ id) _ co
                          (CExport (L _ (CExportStatic ext_nm cconv)) _)) = do
      (h, c, bs) <- ghcjsDsFExport id co ext_nm cconv False
      return (h, c, [id], bs)

ghcjsDsFExport :: Id                 -- Either the exported Id,
                                     -- or the foreign-export-dynamic constructor
               -> Coercion           -- Coercion between the Haskell type callable
                                     -- from JS, and its representation type
               -> CLabelString       -- The name to export to JS land
               -> CCallConv
               -> Bool               -- True => foreign export dynamic
                                     --         so invoke IO action that's hanging off
                                     --         the first argument's stable pointer
               -> DsM ( SDoc         -- contents of Module_stub.h
                      , SDoc         -- contents of Module_stub.c
                      , [Binding]    -- bindings to include
                      )

ghcjsDsFExport fn_id co ext_name cconv isDyn = -- return (empty, empty, [])

                                               do
    let
       ty                              = pSnd $ coercionKind co
       (_tvs,sans_foralls)             = tcSplitForAllTys ty
       (fe_arg_tys', orig_res_ty)      = tcSplitFunTys sans_foralls
       -- We must use tcSplits here, because we want to see
       -- the (IO t) in the corner of the type!
       fe_arg_tys | isDyn     = tail fe_arg_tys'
                  | otherwise = fe_arg_tys'
    dflags <- getDynFlags
    u <- newUnique
    u1 <- newUnique
    let bnd = mkExportedLocalId VanillaId (mkSystemVarName u1 $ fsLit "dsjs") unitTy
        bs  = mkFExportJsBits bnd dflags (cconv == JavaScriptCallConv) ext_name
                     (if isDyn then Nothing else Just fn_id)
                     fe_arg_tys orig_res_ty cconv u
    
    return (empty, empty, bs)

mkFExportJsBits :: Id
                -> DynFlags
                -> Bool
                -> FastString
                -> Maybe Id -- Just==static, Nothing==dynamic
                -> [Type]
                -> Type
                -> CCallConv
                -> Unique
                -> [Binding]
mkFExportJsBits btgt dflags is_js_conv js_nm (Just target) arg_htys res_hty _cc u =
  let expr = mkApps (
        Var mkExport) [ mkIntLit dflags $ if is_js_conv then 1 else 0
                      , Lit (mkMachString $ unpackFS js_nm)
                      , Var target
                      ]
      mkExport = mkFCallId dflags u
                    (CCall (CCallSpec (StaticTarget (fsLit "__mkExport") Nothing True) JavaScriptCallConv PlayRisky))
                    mkExportTy
      mkExportTy = mkFunTys [intPrimTy, addrPrimTy, (mkFunTys arg_htys res_hty)] unitTy
  in [(btgt, expr)]
mkFExportJsBits _ _ _ _ _ _ _ _ _ = []

ghcjsDsFImport :: Id
               -> Coercion
               -> ForeignImport
               -> DsM ([Binding], SDoc, SDoc)
ghcjsDsFImport id co (CImport cconv safety mHeader spec _) = do
    (ids, h, c) <- dsJsImport id co spec (unLoc cconv) (unLoc safety) mHeader
    return (ids, h, c)


dsJsImport :: Id
           -> Coercion
           -> CImportSpec
           -> CCallConv
           -> Safety
           -> Maybe Header
           -> DsM ([Binding], SDoc, SDoc)
dsJsImport id co (CLabel cid) cconv _ _ = do
   dflags <- getDynFlags
   let ty = pFst $ coercionKind co
       fod = case tyConAppTyCon_maybe (dropForAlls ty) of
             Just tycon
              | tyConUnique tycon == funPtrTyConKey ->
                 IsFunction
             _ -> IsData
   (_resTy, foRhs) <- resultWrapper ty
--   ASSERT(fromJust resTy `eqType` addrPrimTy)    -- typechecker ensures this
   let rhs = foRhs (Lit (MachLabel cid stdcall_info fod))
       rhs' = Cast rhs co
       stdcall_info = fun_type_arg_stdcall_info dflags cconv ty
                      -- in
   return ([(id, rhs')], empty, empty)

dsJsImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsJsImport id co (CFunction target) cconv safety mHeader
  = dsJsCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsJsImport id co CWrapper cconv _ _
  = dsJsFExportDynamic id co cconv

-- fixme work in progress
dsJsFExportDynamic :: Id
                 -> Coercion
                 -> CCallConv
                 -> DsM ([Binding], SDoc, SDoc)
dsJsFExportDynamic id co0 _cconv = do
    dflags <- getDynFlags
    u <- newUnique
    let fun_ty = head arg_tys
    arg_id <- newSysLocalDs fun_ty
    let mkExport = mkFCallId dflags u
                      (CCall (CCallSpec (StaticTarget (fsLit "h$mkExportDyn") Nothing True) JavaScriptCallConv PlayRisky))
                      (mkFunTy addrPrimTy ty)
        mkExportTy = mkFunTy (mkFunTys arg_tys res_ty) unitTy
        (_fun_args0, _fun_r) = splitFunTys (dropForAlls fun_ty)
        -- fixme: disabled due to bug. enable again to make foreign exports work
        expr       = Lam arg_id $ (Var arg_id) -- mkApps (Var mkExport) [Lit (mkMachString $ (snd (jsTySigLit dflags True fun_r) : ".") ++ map (snd . jsTySigLit dflags False) fun_args0), Var arg_id]
        fed        = (id `setInlineActivation` NeverActive)
    return ([(fed,expr)], empty, empty)

 where
  ty                       = pFst (coercionKind co0)
  (_tvs, sans_foralls)     = tcSplitForAllTys ty
  (arg_tys, fn_res_ty)     = tcSplitFunTys sans_foralls
  Just (_io_tc, res_ty)    = tcSplitIOType_maybe fn_res_ty
        -- Must have an IO type; hence Just

dsJsCall :: Id -> Coercion -> ForeignCall -> Maybe Header
        -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsJsCall fn_id co fcall _mDeclHeader = do
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
  | Just (co, _rep_ty) <- topNormaliseNewType_maybe arg_ty
  = unboxJsArg (mkCast arg co)

  -- Booleans, do not convert to 0/1, only force them
  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` boolTyConKey
  = return (arg,
              \ body -> mkWildCase arg boolTy (exprType body) [(DEFAULT,[],body)])

  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` anyTyConKey
  = return (arg,
              \ body -> mkWildCase arg arg_ty (exprType body) [(DEFAULT,[],body)])
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
        ; let return_result state ans
                = mkConApp (tupleCon UnboxedTuple 2)
                           (map Type [realWorldStatePrimTy, io_res_ty]
                              ++ [state, ans])

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
    return_result _ ans = ans

mk_alt :: (Expr Var -> Expr Var -> Expr Var)
       -> (Maybe Type, Expr Var -> Expr Var)
       -> DsM (Type, (AltCon, [Id], Expr Var))
mk_alt return_result (Nothing, wrap_result)
  = do -- The ccall returns ()
       state_id <- newSysLocalDs realWorldStatePrimTy
       let
             the_rhs = return_result (Var state_id)
                                     (wrap_result $ panic "jsBoxResult")

             ccall_res_ty = mkTyConApp unboxedSingletonTyCon [realWorldStatePrimTy]
             the_alt      = (DataAlt unboxedSingletonDataCon, [state_id], the_rhs)
       
       return (ccall_res_ty, the_alt)

mk_alt return_result (Just prim_res_ty, wrap_result)
                -- The ccall returns a non-() value
  | isUnboxedTupleType prim_res_ty = do
    let
        Just ls = tyConAppArgs_maybe prim_res_ty
        arity = 1 + length ls
    args_ids {-@(result_id:as)-} <- mapM newSysLocalDs ls
    state_id <- newSysLocalDs realWorldStatePrimTy
    let
        result_tup = mkCoreConApps (tupleCon UnboxedTuple (length ls)) (map Type ls ++ map Var args_ids)
        the_rhs = return_result (Var state_id)
                                (wrap_result result_tup)
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
                                (wrap_result (Var result_id))
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
  -- Base case 1a: unboxed tuples
  | Just (tc, args) <- splitTyConApp_maybe result_ty
  , isUnboxedTupleTyCon tc {- && False -} = do
    (tys, wrappers) <- unzip <$> mapM jsResultWrapper args
    matched <- mapM (mapM newSysLocalDs) tys
    let tys'    = catMaybes tys
        arity   = length args
        resCon  = tupleCon UnboxedTuple (length args)
        err     = panic "jsResultWrapper: used Id with result type Nothing"
        resWrap :: CoreExpr
        resWrap = mkCoreConApps resCon (map Type args ++ zipWith (\w -> w . Var . fromMaybe err) wrappers matched)
    return $
      if null tys'
        then (Nothing, \_ -> resWrap)
        else let innerArity = length tys'
                 innerTy    = mkTyConApp (tupleTyCon UnboxedTuple innerArity) tys'
                 innerCon   = tupleCon UnboxedTuple innerArity
                 inner :: CoreExpr -> CoreExpr
                 inner e    = mkWildCase e innerTy result_ty
                                         [( DataAlt innerCon
                                          , catMaybes matched
                                          , resWrap
                                          )]
             in (Just innerTy, inner)
  -- Base case 1b: primitive types
  | isPrimitiveType result_ty
  = return (Just result_ty, \e -> e)
  -- Base case 1c: boxed tuples
  | Just (tc, args) <- splitTyConApp_maybe result_ty
  , isBoxedTupleTyCon tc = do
      let innerTy = mkTyConApp (tupleTyCon UnboxedTuple (length args)) args
      (inner_res, w) <- jsResultWrapper innerTy
      matched <- mapM newSysLocalDs args
      let inner e = mkWildCase (w e) innerTy result_ty
                               [( DataAlt (tupleCon UnboxedTuple (length args))
                                , matched
                                , mkCoreConApps (tupleCon BoxedTuple (length args)) (map Type args ++ map Var matched)
                                )]
      return (inner_res, inner)
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
  -- Base case 4: the any type
  |  Just (tc,_) <- maybe_tc_app, tc `hasKey` anyTyConKey
  = return (Just result_ty, \e -> e)
  -- Recursive newtypes
  | Just (co, rep_ty) <- topNormaliseNewType_maybe result_ty
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
  mkFCall dflags u (CCall (CCallSpec (StaticTarget (mkFastString tgt)
                                                   (Just primPackageKey)
                                                   True)
                                      JavaScriptCallConv PlayRisky)) args t


-- When the result of a foreign call is smaller than the word size, we
-- need to sign- or zero-extend the result up to the word size.  The C
-- standard appears to say that this is the responsibility of the
-- caller, not the callee.

-- narrow int32 and word32 since JS numbers can contain more
maybeJsNarrow :: DynFlags -> TyCon -> (CoreExpr -> CoreExpr)
maybeJsNarrow _dflags tycon
  | tycon `hasKey` int8TyConKey   = \e -> App (Var (mkGhcjsPrimOpId Narrow8IntOp)) e
  | tycon `hasKey` int16TyConKey  = \e -> App (Var (mkGhcjsPrimOpId Narrow16IntOp)) e
  | tycon `hasKey` int32TyConKey  = \e -> App (Var (mkGhcjsPrimOpId Narrow32IntOp)) e
  | tycon `hasKey` word8TyConKey  = \e -> App (Var (mkGhcjsPrimOpId Narrow8WordOp)) e
  | tycon `hasKey` word16TyConKey = \e -> App (Var (mkGhcjsPrimOpId Narrow16WordOp)) e
  | tycon `hasKey` word32TyConKey = \e -> App (Var (mkGhcjsPrimOpId Narrow32WordOp)) e
  | otherwise                     = id

{-
  desugar foreign declarations for native code: replace
  all foreign import JavaScript by a CCall
  to the JavaScript handler.

  The JavaScript handler can be installed by calling
  `setJavaScriptHandler` (ghcjs.h) from C. The
  default handler prints an error message and terminates the
  program.
-}
ghcjsNativeDsForeigns :: [LForeignDecl Id]
                      -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsNativeDsForeigns fos = do
  dflags <- getDynFlags
  (stubs, ret) <- dsForeigns' (map (convertForeignDecl dflags) fos)
  case catMaybes $ map (importStub dflags) fos of
    [] -> return (stubs, ret)
    xs -> return (stubs `appendStubC'` vcat xs, ret)
    where
      appendStubC' NoStubs s = ForeignStubs empty (inclGhcjs $$ s)
      appendStubC' (ForeignStubs h c) s =
        ForeignStubs h (inclGhcjs $$ c $$ s)
      inclGhcjs = text "#include \"ghcjs.h\""

      convertForeignDecl :: DynFlags -> LForeignDecl Id -> LForeignDecl Id
      convertForeignDecl dflags (L l (ForeignImport n t c (CImport (L lc JavaScriptCallConv) _safety mheader _spec txt))) =
        (L l (ForeignImport n t c (CImport (noLoc CCallConv) (noLoc PlaySafe) mheader (convertSpec dflags n) txt)))
      convertForeignDecl _dflags (L l (ForeignExport n t c (CExport (L _ (CExportStatic lbl JavaScriptCallConv)) txt))) =
        (L l (ForeignExport n t c (CExport (noLoc (CExportStatic lbl CCallConv)) txt)))
      convertForeignDecl _ x = x

      convertSpec :: DynFlags -> Located Id -> CImportSpec
      convertSpec dflags i = CFunction (StaticTarget (stubName dflags (unLoc i)) Nothing True)

      stubName :: DynFlags -> Id -> FastString
      stubName dflags i = mkFastString $
        "__ghcjs_stub_" ++ zEncodeString (showSDocOneLine dflags (ppr $ idName i))

      importStub :: DynFlags -> LForeignDecl Id -> Maybe SDoc
      importStub dflags (L _l (ForeignImport n _t c (CImport (L _ JavaScriptCallConv) (L _ safety) _mheader spec _txt))) =
        Just (mkImportStub dflags (unLoc n) c safety spec)
      importStub _ _ = Nothing

      mkImportStub :: DynFlags -> Id -> Coercion -> Safety -> CImportSpec -> SDoc
      mkImportStub dflags i _c s spec =
        text resTy <+> ftext (stubName dflags i) <> stubArgs <+> braces body
          where
           js :: SDoc
           js = case spec of
                  CLabel cls                       -> escapeQuoted (unpackFS cls)
                  CFunction (StaticTarget cls _ _) -> escapeQuoted (unpackFS cls)
                  _ -> error "ghcjsNativeDsForeigns: unexpected import spec"
           safety | s == PlayRisky         = int 0
                  | s == PlaySafe          = int 1
                  | s == PlayInterruptible = int 2
           escapeQuoted xs = doubleQuotes $ text (concatMap escapeChar xs)
             where
               -- fixme proper escaping and handling of non-ascii characters
               escapeChar '\\' = "\\\\"
               escapeChar '\n' = "\\n"
               escapeChar '\t' = "\\t"
               escapeChar '"'  = "\\\""
               escapeChar x    = x:[]

           t = idType i
           (args, res) = tcSplitFunTys . snd . tcSplitForAllTys $ t
           argNames    = map ((text "arg" <>) . int) [1..]
           (argTys, argsSig) = unzip $ map (jsTySigLit dflags False) args
           (resTy,  resSig) = jsTySigLit dflags True res
           body | resSig == 'v' = vcat [text "int res;", call]
                | otherwise     = vcat [text resTy <+> text "res;", call, text "return res;"]
           call = text "getJavaScriptHandler()" <> parens (pprWithCommas id handlerArgs) <> semi
           handlerArgs = [js, safety, escapeQuoted (resSig : argsSig), text "(void*)&res"]
                           ++ take (length args) argNames
           stubArgs = parens $ pprWithCommas id (zipWith (\ty n -> text ty <+> n) argTys argNames)

jsTySigLit :: DynFlags -> Bool -> Type -> (String, Char)
jsTySigLit dflags isResult t | isResult, Just (_ ,result) <- tcSplitIOType_maybe t =
                                 jsTySigLit dflags isResult result
                             | Just (_, t') <- splitForAllTy_maybe t = jsTySigLit dflags isResult t'
                             | Just (tc, _) <- splitTyConApp_maybe t = tcSig isResult tc
                             | otherwise = error $ "jsTySigLit: unexpected type: "
                                                       ++ showSDoc dflags (ppr t)
  where
           tcSig :: Bool -> TyCon -> (String, Char)
           tcSig isResult tc
             | isUnLiftedTyCon tc                                  = prim (tyConPrimRep tc)
             | Just r <- lookup (getUnique tc) boxed               = r
             | isResult && getUnique tc == unitTyConKey            = ("void", 'v')
             | isJSValTyCon dflags tc = ("StgPtr", 'r')
             | otherwise = error $ "jsTySigLit: unexpected TyCon: "
                                       ++ showSDoc dflags (ppr tc)
              where
                 -- fixme is there already a list of these somewhere else?
                 prim VoidRep  = error "jsTySigLit: VoidRep"
                 prim PtrRep   = hsPtr
                 prim IntRep   = hsInt
                 prim WordRep  = hsWord
                 prim Int64Rep = hsInt64
                 prim Word64Rep = hsWord64
                 prim AddrRep   = hsPtr
                 prim FloatRep  = hsFloat
                 prim DoubleRep = hsDouble
                 prim (VecRep{}) = error "jsTySigLit: VecRep"
                 boxed = [ (intTyConKey,        hsInt               )
                         , (int8TyConKey,       ("StgInt8",      'b'))
                         , (int16TyConKey,      ("StgInt16",     's'))
                         , (int32TyConKey,      ("StgInt32",     'l'))
                         , (int64TyConKey,      hsInt64             )
                         , (wordTyConKey,       hsWord              )
                         , (word8TyConKey,      ("StgWord8",     'B'))
                         , (word16TyConKey,     ("StgWord16",    'S'))
                         , (word32TyConKey,     ("StgWord32",    'L'))
                         , (word64TyConKey,     hsWord64            )
                         , (floatTyConKey,      hsFloat             )
                         , (doubleTyConKey,     hsDouble            )
                         , (ptrTyConKey,        hsPtr               )
                         , (funPtrTyConKey,     hsPtr               )
                         , (charTyConKey,       ("StgChar",      'c'))
                         , (stablePtrTyConKey,  hsPtr               )
                         , (boolTyConKey,       hsInt               )
                         ]
                 hsInt    = ("StgInt",    'i')
                 hsInt64  = ("StgInt64",  'm')
                 hsWord   = ("StgWord",   'I')
                 hsWord64 = ("StgWord64", 'M')
                 hsPtr    = ("StgPtr",    'p')
                 hsFloat  = ("StgFloat",  'f')
                 hsDouble = ("StgDouble", 'd')

ghcjsTcForeignImports :: [LForeignDecl Name]
                      -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
ghcjsTcForeignImports decls
  = do { (ids, decls, gres) <- mapAndUnzip3M ghcjsTcFImport $
                               filter isForeignImport decls
       ; return (ids, decls, unionManyBags gres) }

foreignDeclCtxt :: ForeignDecl Name -> SDoc
foreignDeclCtxt fo
  = hang (ptext (sLit "When checking declaration:"))
       2 (ppr fo)

ghcjsTcFImport :: LForeignDecl Name -> TcM (Id, LForeignDecl Id, Bag GlobalRdrElt)
ghcjsTcFImport (L dloc fo@(ForeignImport (L nloc nm) hs_ty _ imp_decl))
  = setSrcSpan dloc $ addErrCtxt (foreignDeclCtxt fo)  $
    do { sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
       ; (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
       ; let
           -- Drop the foralls before inspecting the
           -- structure of the foreign type.
             (_, t_ty)         = tcSplitForAllTys norm_sig_ty
             (arg_tys, res_ty) = tcSplitFunTys t_ty
             id                = mkLocalId nm sig_ty
                 -- Use a LocalId to obey the invariant that locally-defined
                 -- things are LocalIds.  However, it does not need zonking,
                 -- (so TcHsSyn.zonkForeignExports ignores it).
       ; imp_decl' <- ghcjsTcCheckFIType sig_ty arg_tys res_ty imp_decl
          -- Can't use sig_ty here because sig_ty :: Type and
          -- we need HsType Id hence the undefined
       ; let fi_decl = ForeignImport (L nloc id) undefined (mkSymCo norm_co) imp_decl'
       ; return (id, L dloc fi_decl, gres) }
ghcjsTcFImport d = pprPanic "ghcjsTcFImport" (ppr d)

ghcjsTcCheckFIType :: Type -> [Type] -> Type -> ForeignImport -> TcM ForeignImport
-- this is a temporary hack until template-haskell has been updated,
-- this allows Template Haskell to produce JavaScriptCallConv declarations without proper support for them
ghcjsTcCheckFIType sig_ty arg_tys res_ty (CImport _cconv safety mh (CFunction (StaticTarget lbl mpkg b)) txt)
  | Just lbl' <- stripPrefix "__ghcjs_javascript_" (unpackFS lbl) = do
      let lbl'' = mkFastString $ map (chr . read) (splitOn "_" lbl')
      ghcjsTcCheckFIType sig_ty arg_tys res_ty (CImport (noLoc JavaScriptCallConv) safety mh (CFunction (StaticTarget lbl'' mpkg b)) txt)
ghcjsTcCheckFIType _sig_ty arg_tys res_ty (CImport lcconv@(L _ cconv) lsafety@(L _ safety) mh (CFunction target) txt)
  | cconv == JavaScriptCallConv = do
      dflags <- getDynFlags
      checkForeignArgs (isGhcjsFFIArgumentTy dflags safety) arg_tys
      checkForeignRes nonIOok checkSafe (isGhcjsFFIImportResultTy dflags) res_ty
      case target of
          StaticTarget _ _ False
           | not (null arg_tys) ->
              addErrTc (text "`value' imports cannot have function types")
          _ -> return ()
      return $ CImport lcconv lsafety mh (CFunction target) txt
ghcjsTcCheckFIType _sig_ty arg_tys res_ty idecl = tcCheckFIType arg_tys res_ty idecl

isGhcjsFFIArgumentTy :: DynFlags -> Safety -> Type -> Validity
isGhcjsFFIArgumentTy dflags safety ty
  | isValid (isFFIArgumentTy dflags safety ty)                          = IsValid
  | xopt Opt_GHCForeignImportPrim dflags && xopt Opt_UnliftedFFITypes dflags &&
    isValid (isFFIPrimArgumentTy dflags ty)                             = IsValid
  | isGhcjsFFITy dflags ty                                              = IsValid
  | Just (tc, _) <- tcSplitTyConApp_maybe ty
  , getUnique tc == anyTyConKey && xopt Opt_GHCForeignImportPrim dflags = IsValid
  | otherwise = NotValid
      (text "JavaScript FFI argument type must be a valid CCall FFI argument type or JSVal")

isGhcjsFFIImportResultTy :: DynFlags -> Type -> Validity
isGhcjsFFIImportResultTy dflags ty
  | Just (tc, args) <- tcSplitTyConApp_maybe ty
  , isBoxedTupleTyCon tc || isUnboxedTupleTyCon tc =
      if all (\ty -> isValid (isGhcjsFFIImportResultTy' dflags ty) || isGhcjsFFITy dflags ty) args
         then IsValid
         else NotValid
              (text $ "JavaScript FFI result type must be a valid CCall FFI result type or JSVal: " ++ showPpr dflags ty)
  | otherwise = isGhcjsFFIImportResultTy' dflags ty

isGhcjsFFIImportResultTy' :: DynFlags -> Type -> Validity
isGhcjsFFIImportResultTy' dflags ty
  | isValid (isFFIImportResultTy dflags ty)                   = IsValid

-- -  | xopt Opt_UnliftedFFITypes dflags && isUnboxedTupleType ty = IsValid
  | xopt Opt_GHCForeignImportPrim dflags && xopt Opt_UnliftedFFITypes dflags &&
    checkRepTyCon isUnLiftedTyCon ty                          = IsValid
  --   isValid (isFFIPrimResultTy dflags ty)                     = IsValid
  | Just (tc, _) <- tcSplitTyConApp_maybe ty
  , xopt Opt_GHCForeignImportPrim dflags && getUnique tc == anyTyConKey = IsValid
  | isGhcjsFFITy dflags ty                                    = IsValid
  | otherwise = NotValid
      (text $ "JavaScript FFI result type must be a valid CCall FFI result type or JSVal: " ++ showPpr dflags ty)

isGhcjsFFITy :: DynFlags -> Type -> Bool
isGhcjsFFITy = checkNamedTy jsFfiTys

isJSValTy :: DynFlags -> Type -> Bool
isJSValTy = checkNamedTy [jsValTy]

isJSValTyCon :: DynFlags -> TyCon -> Bool
isJSValTyCon = checkNamedTyCon [jsValTy]

checkNamedTy :: [(String, String, String)] -> DynFlags -> Type -> Bool
checkNamedTy tys dflags ty = checkRepTyCon (checkNamedTyCon tys dflags) ty

checkNamedTyCon :: [(String, String, String)] -> DynFlags -> TyCon -> Bool
checkNamedTyCon tys dflags tc
  = any (\(p,m,n) -> m == mod && n == name && validPkg p pkg) tys
      where
        validPkg p ""
          | [b] <- catMaybes (map (stripPrefix "-DBOOTING_PACKAGE=")
                                  (opt_P dflags)) = p == b
        validPkg p p' = p == p'

        -- comparing strings is probably not too fast, perhaps search
        -- for the types first and use some cache
        n = tyConName (repTc tc)
        (pkg, mod) = case nameModule_maybe n of
                       Nothing -> ("", "")
                       Just m  -> ( modulePackageName dflags m
                                  , moduleNameString (moduleName m))
        name = occNameString (nameOccName n)

repTc :: TyCon -> TyCon
repTc = go
  where
    go :: TyCon -> TyCon
    go tc | Just (_tvs, t, _) <- unwrapNewTyCon_maybe tc =
              case splitTyConApp_maybe (dropForAlls t) of
                Nothing       -> error "repTc: not a tycon application"
                Just (tc', _) -> go tc'
          | otherwise = tc

jsFfiTys :: [(String, String, String)]
jsFfiTys = [jsValTy]

jsValTy :: (String, String, String)
jsValTy = ("ghcjs-prim", "GHCJS.Prim", "JSVal")

-- normaliseFfiType gets run before checkRepTyCon, so we don't
-- need to worry about looking through newtypes or type functions
-- here; that's already been taken care of.
checkRepTyCon :: (TyCon -> Bool) -> Type -> Bool
checkRepTyCon check_tc ty
    | Just (tc, _) <- splitTyConApp_maybe ty
      = check_tc tc
    | otherwise
      = False

ghcjsNativeTcForeignImports :: [LForeignDecl Name]
                            -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
ghcjsNativeTcForeignImports = ghcjsTcForeignImports

ghcjsNativeTcForeignExports :: [LForeignDecl Name]
                            -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
ghcjsNativeTcForeignExports = ghcjsTcForeignExports

ghcjsTcForeignExports :: [LForeignDecl Name]
                 -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
-- For the (Bag GlobalRdrElt) result,
-- see Note [Newtype constructor usage in foreign declarations]
ghcjsTcForeignExports decls
  = foldlM combine (emptyLHsBinds, [], emptyBag) (filter isForeignExport decls)
  where
   combine (binds, fs, gres1) (L loc fe) = do
       (b, f, gres2) <- setSrcSpan loc (ghcjsTcFExport fe)
       return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

ghcjsTcFExport :: ForeignDecl Name -> TcM (LHsBind Id, ForeignDecl Id, Bag GlobalRdrElt)
ghcjsTcFExport fo@(ForeignExport (L loc nm) hs_ty _ spec)
  = addErrCtxt (foreignDeclCtxt fo) $ do

    sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
    rhs <- tcPolyExpr (nlHsVar nm) sig_ty

    (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty

    spec' <- ghcjsTcCheckFEType norm_sig_ty spec

           -- we're exporting a function, but at a type possibly more
           -- constrained than its declared/inferred type. Hence the need
           -- to create a local binding which will call the exported function
           -- at a particular type (and, maybe, overloading).


    -- We need to give a name to the new top-level binding that
    -- is *stable* (i.e. the compiler won't change it later),
    -- because this name will be referred to by the C code stub.
    id  <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
    return (mkVarBind id rhs, ForeignExport (L loc id) undefined norm_co spec', gres)

-- tcFExport d = pprPanic "tcFExport" (ppr d)


ghcjsTcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
ghcjsTcCheckFEType sig_ty (CExport (L l (CExportStatic str cconv)) txt) = do
--    checkCg checkCOrAsmOrLlvm
    check (isCLabelString str) (badCName str)
    cconv' <- ghcjsCheckCConv cconv
    checkForeignArgs isFFIExternalTy arg_tys
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
    return (CExport (L l (CExportStatic str cconv')) txt)
  where
      -- Drop the foralls before inspecting n
      -- the structure of the foreign type.
    (_, t_ty) = tcSplitForAllTys sig_ty
    (arg_tys, res_ty) = tcSplitFunTys t_ty

ghcjsCheckCConv :: CCallConv -> TcM CCallConv
ghcjsCheckCConv CCallConv          = return CCallConv
ghcjsCheckCConv CApiConv           = return CApiConv
ghcjsCheckCConv JavaScriptCallConv = return JavaScriptCallConv

ghcjsCheckCConv StdCallConv  = do dflags <- getDynFlags
                                  let platform = targetPlatform dflags
                                  if platformArch platform == ArchX86
                                    then return StdCallConv
                                    else do -- This is a warning, not an error. see #3336
                                           when (wopt Opt_WarnUnsupportedCallingConventions dflags) $
                                             addWarnTc (text "the 'stdcall' calling convention is unsupported on this platform," $$ text "treating as ccall")
                                           return CCallConv
ghcjsCheckCConv PrimCallConv = do addErrTc (text "The `prim' calling convention can only be used with `foreign import'")
                                  return PrimCallConv


checkCg :: (HscTarget -> Maybe SDoc) -> TcM ()
checkCg check = do
    dflags <- getDynFlags
    let target = hscTarget dflags
    case target of
      HscNothing -> return ()
      _ ->
        case check target of
          Nothing  -> return ()
          Just err -> addErrTc (text "Illegal foreign declaration:" <+> err)

check :: Bool -> MsgDoc -> TcM ()
check True _       = return ()
check _    the_err = addErrTc the_err

badCName :: CLabelString -> MsgDoc
badCName target
  = sep [quotes (ppr target) <+> ptext (sLit "is not a valid C identifier")]
