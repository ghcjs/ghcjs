{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Generator.Core (withBindings, notExportedDecl, creation, definition) where

import Data.List (partition, find)
import Data.Monoid (Monoid(..), mconcat)

import Panic (panic)

import DataCon as Stg (DataCon, dataConTag, dataConName)
import Id as Stg (Id)
import CoreSyn as Stg (AltCon (DataAlt, LitAlt, DEFAULT))
import StgSyn as Stg
import DataCon (isUnboxedTupleCon)

import qualified Javascript.Language as Js
import Javascript.Language (Javascript, Expression)

import Generator.Helpers
import Generator.PrimOp
import Generator.FFI
import qualified RTS.Objects as RTS
import Control.Applicative ((<$>))
import Module (moduleNameString)
import Name (getOccName, getName)
#if __GLASGOW_HASKELL__ >= 703
import Name (pprNameDefnLoc)
#endif
import OccName (occNameString)
import Outputable (showSDoc)

binding :: Javascript js => StgBinding -> Gen js
binding (StgNonRec id rhs) = nonRecDeclAndDef id rhs
binding (StgRec bs) = bindings bs

bindings :: Javascript js => [(Stg.Id, StgRhs)] -> Gen js
bindings binds = do
    notExp <- Js.declare <$> mapM notExportedDecl notExportedBindings
    exp    <- withBindings exportedDecl exportedBindings
    return $ mconcat [notExp, exp]
  where (exportedBindings, notExportedBindings) = partition (isExternalId . fst) binds
        exportedDecl id rhs = do
            i <- stgIdToJs id
            r <- creation id rhs
            return $ Js.assign (Js.var i) r

notExportedDecl :: Javascript js => (Stg.Id, StgRhs) -> Gen (Js.Id, Expression js)
notExportedDecl (id, rhs) = do
    i <- stgIdToJsId id
    r <- creation id rhs
    return (i, r)

withBindings :: Javascript js => (Stg.Id -> StgRhs -> Gen js) -> [(Stg.Id, StgRhs)] -> Gen js
withBindings f b = mconcat <$> mapM (uncurry f) b

debugInfo :: Js.Javascript js => Stg.Id -> Gen [Expression js]
debugInfo x = do
#if __GLASGOW_HASKELL__ >= 703
    case showSDoc . pprNameDefnLoc $ getName x of
        "at <no location info>" -> return [Js.string . occNameString . getOccName $ x]
        s                       -> return [Js.string s]
#else
    return [Js.string . occNameString . getOccName $ x]
#endif

creation :: Javascript js => Stg.Id -> StgRhs -> Gen (Expression js)
creation id (StgRhsCon _cc con args) = do
    a <- mapM stgArgToJs args
    dataCreation id con a
creation id rhs@(StgRhsClosure _cc _bi _fvs upd_flag _srt args body)
  | isUpdatable upd_flag = do
        a <- mapM stgIdToJsId args
        b <- expression body
        info <- debugInfo id
        return $ Js.new RTS.makeThunkLocalStub $ [Js.function a b] ++ info
  | otherwise = do
        a <- mapM stgIdToJsId args
        b <- expression body
        info <- debugInfo id
        return $ Js.new RTS.makeFuncLocalStub $ [Js.int (stgRhsArity rhs), Js.function a b] ++ info

dataCreation :: Javascript js => Stg.Id -> DataCon -> [Expression js] -> Gen (Expression js)
dataCreation id con args
  | isUnboxedTupleCon con = return $ Js.list args
  | otherwise = do
    info <- debugInfo id
    return $ Js.new RTS.makeDataLocalStub $ [Js.int (dataConTag con), Js.function [] (Js.return $ Js.list args)] ++ info

definition :: Javascript js => Stg.Id -> StgRhs -> Gen js
definition id (StgRhsCon _cc con args)
  | not (isUnboxedTupleCon con) = do
            a <- mapM stgArgToJs args
            info <- debugInfo id
            return $ Js.declare [(stgIdToJsExternalName id,
                Js.new RTS.makeDataStub $ [Js.int (dataConTag con), Js.function [] (Js.return $ Js.list a)] ++ info)]
  | otherwise = do
            return $ Js.declare [(stgIdToJsExternalName id, Js.null)]
definition id rhs@(StgRhsClosure _cc _bi _fvs upd_flag _srt args body) = do
            a <- mapM stgIdToJsId args
            b <- expression body
            info <- debugInfo id
            return $ Js.declare [(stgIdToJsExternalName id,
                if isUpdatable upd_flag
                    then Js.new (RTS.makeThunkStub) $ [Js.function a b] ++ info
                    else Js.new (RTS.makeFuncStub) $ [Js.int (stgRhsArity rhs), Js.function a b] ++ info)]

nonRecDeclAndDef :: Javascript js => Stg.Id -> StgRhs -> Gen js
nonRecDeclAndDef id (StgRhsCon _cc con args)
  | not (isUnboxedTupleCon con) = do
            i <- stgIdToJsId id
            a <- mapM stgArgToJs args
            info <- debugInfo id
            return $ Js.declare [(i,
                Js.nativeFunctionCall (RTS.makeData) $ [Js.int (dataConTag con), Js.list a] ++ info)]
  | otherwise = do
            i <- stgIdToJsId id
            a <- mapM stgArgToJs args
            return $ Js.declare [(i, Js.list a)]
nonRecDeclAndDef id rhs@(StgRhsClosure _cc _bi _fvs upd_flag _srt args body) = do
            i <- stgIdToJsId id
            a <- mapM stgIdToJsId args
            e <- expression body
            info <- debugInfo id
            return $ Js.declare [(i, if isUpdatable upd_flag
                then Js.nativeFunctionCall (RTS.makeThunk) $ [Js.function a e] ++ info
                else Js.nativeFunctionCall (RTS.makeFunc) $ [Js.int (stgRhsArity rhs), Js.function a e] ++ info)]

expression :: Javascript js => StgExpr -> Gen js
expression (StgCase expr _liveVars _liveRhsVars bndr _srt alttype alts) =
  caseExpression expr bndr alttype alts
expression (StgLet bndn body) = mconcat <$> sequence [binding bndn, expression body]
expression (StgLetNoEscape _ _ bndn body) = mconcat <$> sequence [binding bndn, expression body]
#if __GLASGOW_HASKELL__ >= 703
expression (StgSCC _ _ _ expr) = expression expr
#else
expression (StgSCC _ expr) = expression expr
#endif
expression (StgTick _ _ expr) = expression expr
expression (StgApp f []) = Js.maybeJumpToApplyMethod . Js.var <$> stgIdToJs f
expression (StgApp f args) = do
    v <- stgIdToJs f
    jsargs <- mapM stgArgToJs args
    return $ Js.jumpToApplyMethod (Js.var v) jsargs
expression (StgLit lit) = Js.return <$> stgLiteralToJs lit
expression (StgConApp con args)
  | isUnboxedTupleCon con = Js.return . Js.list <$> mapM stgArgToJs args
  | otherwise = do
    jsargs <- mapM stgArgToJs args
    return $ Js.returnValue [Js.int (dataConTag con), Js.list jsargs, Js.string . occNameString . getOccName $ dataConName con]
expression (StgOpApp (StgFCallOp f g) args _ty) = returnForeignFunctionCallResult f g args
expression (StgOpApp (StgPrimOp op) args _ty) = returnPrimitiveOperationResult op args
expression (StgOpApp (StgPrimCallOp call) args _ty) = returnPrimitiveCallResult call args
expression (StgLam{}) = panic "unexpected StgLam" -- StgLam is used *only* during CoreToStg's work (StgSyn.lhs:196)

caseExpression :: Javascript js => StgExpr -> Stg.Id -> Stg.AltType -> [StgAlt] -> Gen js
caseExpression expr bndr alttype alts = do
  altsJs <- caseExpressionAlternatives bndr alttype alts
  caseExpressionScrut bndr expr altsJs

-- | 'caseExpressionScrut' is absolutely the same as expression
-- the difference is that 'expression' "returns" result
-- but 'caseExpressionScrut' "binds" result
caseExpressionScrut :: Javascript js => Stg.Id -> StgExpr -> js -> Gen js
caseExpressionScrut binder expr altsJs = go expr
  where go (StgConApp con args)
          | isUnboxedTupleCon con = do
              v <- stgIdToJsId binder
              jsargs <- mapM stgArgToJs args
              return $ mconcat [ Js.declare [(v, (Js.list jsargs))], altsJs ]
          | otherwise = do
              v <- stgIdToJsId binder
              jsargs <- mapM stgArgToJs args
              return $ mconcat
                [ Js.declare [(v, Js.nativeFunctionCall (RTS.makeData) [Js.int (dataConTag con), Js.list jsargs, Js.string . occNameString . getOccName $ dataConName con])]
                , altsJs
                ]
        go (StgApp f []) = do
          v <- stgIdToJsId binder
          object <- stgIdToJs f
          return $ mconcat
            [ Js.maybeAssignApplyMethodCallResult v (Js.var object) altsJs]
        go (StgApp f args) = do
          v <- stgIdToJsId binder
          object <- stgIdToJs f
          jsargs <- mapM stgArgToJs args
          return $ Js.declareApplyMethodCallResult v (Js.var object) jsargs altsJs
        go (StgLit lit) = do
          v <- stgIdToJsId binder
          l <- stgLiteralToJs lit
          return $ mconcat [ Js.declare [(v, l)], altsJs ]
        go (StgOpApp (StgFCallOp f g) args _ty) = mconcat <$> sequence
            [ declareForeignFunctionCallResult binder f g args, return altsJs ]
        go (StgOpApp (StgPrimOp op) args _ty) =
            declarePrimitiveOperationResult binder op args altsJs
        go (StgOpApp (StgPrimCallOp call) args _ty) = mconcat <$> sequence
            [ declarePrimitiveCallResult binder call args, return altsJs ]
        go e = do
          v <- stgIdToJsId binder
          f <- Js.function [] <$> expression e
          return $ Js.declareFunctionCallResult v f [] altsJs

caseExpressionAlternatives :: Javascript js => Stg.Id -> Stg.AltType -> [(Stg.AltCon, [Stg.Id], [Bool], StgExpr)] -> Gen js
caseExpressionAlternatives bndr altType [(_altCon, args, useMask, expr)] =
  case altType
  of PolyAlt {} -> jsexpr
     PrimAlt {} -> jsexpr
     UbxTupAlt {} -> object >>= process
     AlgAlt {} -> object >>= (process . RTS.conAppArgVector)
  where object = Js.var <$> stgIdToJs bndr
        process obj = mconcat <$> sequence [unpackData obj useMask args, jsexpr]
        jsexpr = expression expr
caseExpressionAlternatives bndr altType alts =
  case altType
    of PolyAlt   {} -> panic "multiple case alternatives for PolyAlt"
       UbxTupAlt {} -> panic "multiple case alternatives for UbxTupAlt"
       PrimAlt   {} -> do
            n  <- name
            dc <- defaultCase
            c  <- cases
            return $ Js.switch n dc c
       AlgAlt    {} -> do
            n  <- name
            dc <- defaultCase
            c  <- cases
            return $ Js.switch (RTS.conAppTag n) dc c
  where
    name :: Javascript js => Gen (Expression js)
    name = Js.var <$> stgIdToJs bndr
    defaultCase = do
        case find isDefault alts of
            Just (_, _, _, expr) -> Just <$> expression expr
            Nothing              -> return Nothing
    isDefault (DEFAULT, _, _, _) = True
    isDefault _ = False
    cases = sequence . map alternative . filter (not . isDefault) $ alts
    alternative :: Javascript js => (Stg.AltCon, [Stg.Id], [Bool], StgExpr) -> Gen (Expression js, js)
    alternative alt = do
        c <- alternativeConst alt
        b <- alternativeBody alt
        return (c, b)
    alternativeBody :: Javascript js => (Stg.AltCon, [Stg.Id], [Bool], StgExpr) -> Gen js
    alternativeBody (alt, args, useMask, expr) =
      case alt of
        DataAlt _ -> do
            n <- name
            mconcat <$> sequence [unpackData (RTS.conAppArgVector n) useMask args, expression expr]
        LitAlt _ -> expression expr
        DEFAULT  -> panic "Default alternative!" :: Gen js
    alternativeConst :: Javascript js => (Stg.AltCon, [Stg.Id], [Bool], StgExpr) -> Gen (Expression js)
    alternativeConst (alt, _args, _useMask, _expr) =
      case alt
      of DataAlt con -> return $ Js.int (dataConTag con)
         LitAlt lit  -> stgLiteralToJs lit
         DEFAULT     -> panic "Default alternative!"

unpackData :: Javascript js => Expression js -> [Bool] -> [Stg.Id] -> Gen js
unpackData object mask args = Js.declare <$> sequence [f n arg | (n, True, arg) <- zip3 [(0::Int)..] mask args]
  where
    f n arg = do
        a <- stgIdToJsId arg
        return (a, (Js.subscript object (Js.int n)))

