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
import Name (getOccName)
#if __GLASGOW_HASKELL__ >= 703
import Name (pprNameDefnLoc, getName)
import Outputable (showSDoc)
#endif
import Gen2.StgAst
import OccName (occNameString)
import UniqSet (uniqSetToList)
import DynFlags

binding :: Javascript js => StgBinding -> Gen js
binding (StgNonRec id rhs) = nonRecDeclAndDef id rhs
binding (StgRec bs) = bindings bs

bindings :: Javascript js => [(Stg.Id, StgRhs)] -> Gen js
bindings binds = do
    notExp  <- Js.declare <$> sequence (recDeclAndDef notExportedBindings)
    fixLive <- sequence (recFixLiveList notExportedBindings)
    exp     <- withBindings exportedDecl exportedBindings
    return $ mconcat $ notExp : exp : fixLive
  where (exportedBindings, notExportedBindings) = partition (isExternalId . fst) binds
        exportedDecl id rhs = do
            i <- stgIdToJs id
            r <- creation id rhs True
            return $ Js.assign (Js.var i) r
        recDeclAndDef []     = []
        recDeclAndDef (x:[]) = [notExportedDecl True x]
        recDeclAndDef (x:xs) = notExportedDecl False x:recDeclAndDef xs

        recFixLiveList []     = []
        recFixLiveList (_:[]) = []
        recFixLiveList ((_, StgRhsCon _ _ _):xs) = recFixLiveList xs
        recFixLiveList ((id, StgRhsClosure _ _ live _ _ _ _):xs) = (do
            i <- stgIdToJsId id
            l <- map Js.var <$> mapM stgIdToJs (filter (/=id) live)
            return $ Js.expression $ RTS.setLiveList (Js.var i) l)
            :recFixLiveList xs

notExportedDecl :: Javascript js => Bool -> (Stg.Id, StgRhs) -> Gen (Js.Id, Expression js)
notExportedDecl includeLive (id, rhs) = do
    i <- stgIdToJsId id
    r <- creation id rhs includeLive
    return (i, r)

withBindings :: Javascript js => (Stg.Id -> StgRhs -> Gen js) -> [(Stg.Id, StgRhs)] -> Gen js
withBindings f b = mconcat <$> mapM (uncurry f) b

debugInfo :: Js.Javascript js => Stg.Id -> Gen [Expression js]
debugInfo x = do
#if __GLASGOW_HASKELL__ >= 703
    case showSDoc' . pprNameDefnLoc $ getName x of
        "at <no location info>" -> return [Js.string . occNameString . getOccName $ x]
        s                       -> return [Js.string s]
#else
    return [Js.string . occNameString . getOccName $ x]
#endif

creation :: Javascript js => Stg.Id -> StgRhs -> Bool -> Gen (Expression js)
creation id (StgRhsCon _cc con args) _includeLive = do
    a <- mapM stgArgToJs args
    dataCreation id con a
creation id rhs@(StgRhsClosure _cc _bi live upd_flag _srt args body) includeLive = do
    l <- if includeLive
            then Just <$> map Js.var <$> mapM stgIdToJs (filter (/=id) live)
            else return Nothing
    a <- mapM stgIdToJsId args
    b <- expression body
    let f = Js.function a b
    info <- debugInfo id
    return $ if isUpdatable upd_flag
        then RTS.makeThunk f l info
        else RTS.makeFunc (stgRhsArity rhs) f l info

dataCreation :: Javascript js => Stg.Id -> DataCon -> [Expression js] -> Gen (Expression js)
dataCreation id con args
  | isUnboxedTupleCon con = return $ Js.list args
  | otherwise = do
    info <- debugInfo id
    return $ RTS.makeDataF (dataConTag con)
        (Js.function [] (Js.return $ Js.list args)) info

definition :: Javascript js => Stg.Id -> StgRhs -> Gen js
definition id (StgRhsCon _cc con args)
  | not (isUnboxedTupleCon con) = do
            a <- mapM stgArgToJs args
            info <- debugInfo id
            return $ Js.declare [(stgIdToJsExternalName id,
                RTS.makeDataF (dataConTag con)
                    (Js.function [] (Js.return $ Js.list a)) info)]
  | otherwise = do
            return $ Js.declare [(stgIdToJsExternalName id, Js.null)]
definition id rhs@(StgRhsClosure _cc _bi live upd_flag _srt args body) = do
            l <- Just <$> map Js.var <$> mapM stgIdToJs (filter (/=id) live)
            a <- mapM stgIdToJsId args
            b <- expression body
            let f = Js.function a b
            info <- debugInfo id
            return $ Js.declare [(stgIdToJsExternalName id,
                if isUpdatable upd_flag
                    then RTS.makeThunk f l info
                    else RTS.makeFunc (stgRhsArity rhs) f l info)]

nonRecDeclAndDef :: Javascript js => Stg.Id -> StgRhs -> Gen js
nonRecDeclAndDef id (StgRhsCon _cc con args)
  | not (isUnboxedTupleCon con) = do
            i <- stgIdToJsId id
            a <- mapM stgArgToJs args
            info <- debugInfo id
            return $ Js.declare [(i,
                RTS.makeDataValue (dataConTag con) a info)]
  | otherwise = do
            i <- stgIdToJsId id
            a <- mapM stgArgToJs args
            return $ Js.declare [(i, Js.list a)]
nonRecDeclAndDef id rhs@(StgRhsClosure _cc _bi live upd_flag _srt args body) = do
            i <- stgIdToJsId id
            l <- Just <$> map Js.var <$> mapM stgIdToJs live
            a <- mapM stgIdToJsId args
            e <- expression body
            let f = Js.function a e
            info <- debugInfo id
            return $ Js.declare [(i, if isUpdatable upd_flag
                then RTS.makeThunk f l info
                else RTS.makeFunc (stgRhsArity rhs) f l info)]

expression :: Javascript js => StgExpr -> Gen js
expression (StgCase expr _liveVars liveRhsVars bndr _srt alttype alts) =
  caseExpression expr bndr alttype alts (uniqSetToList liveRhsVars)
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
    return $ Js.returnValue (dataConTag con) jsargs (Js.string . occNameString . getOccName $ dataConName con)
expression (StgOpApp (StgFCallOp f g) args _ty) = returnForeignFunctionCallResult f g args
expression (StgOpApp (StgPrimOp op) args _ty) = returnPrimitiveOperationResult op args
expression (StgOpApp (StgPrimCallOp call) args _ty) = returnPrimitiveCallResult call args
expression (StgLam{}) = panic "unexpected StgLam" -- StgLam is used *only* during CoreToStg's work (StgSyn.lhs:196)

caseExpression :: Javascript js => StgExpr -> Stg.Id -> Stg.AltType -> [StgAlt] -> [Stg.Id] -> Gen js
caseExpression expr bndr alttype alts live = do
  altsJs <- caseExpressionAlternatives bndr alttype alts
  l <- mapM stgIdToJs live
  caseExpressionScrut bndr expr altsJs (Js.list (map Js.var l))

-- | 'caseExpressionScrut' is absolutely the same as expression
-- the difference is that 'expression' "returns" result
-- but 'caseExpressionScrut' "binds" result
caseExpressionScrut :: Javascript js => Stg.Id -> StgExpr -> js -> Expression js -> Gen js
caseExpressionScrut binder expr altsJs live = go expr
  where go (StgConApp con args)
          | isUnboxedTupleCon con = do
              v <- stgIdToJsId binder
              jsargs <- mapM stgArgToJs args
              return $ mconcat [ Js.declare [(v, (Js.list jsargs))], altsJs ]
          | otherwise = do
              v <- stgIdToJsId binder
              jsargs <- mapM stgArgToJs args
              return $ mconcat
                [ Js.declare [(v, RTS.makeDataValue (dataConTag con) jsargs [Js.string . occNameString . getOccName $ dataConName con])]
                , altsJs
                ]
        go (StgApp f []) = do
          v <- stgIdToJsId binder
          object <- stgIdToJs f
          return $ Js.maybeAssignApplyMethodCallResult v (Js.var object) altsJs live
        go (StgApp f args) = do
          v <- stgIdToJsId binder
          object <- stgIdToJs f
          jsargs <- mapM stgArgToJs args
          return $ Js.declareApplyMethodCallResult v (Js.var object) jsargs altsJs live
        go (StgLit lit) = do
          v <- stgIdToJsId binder
          l <- stgLiteralToJs lit
          return $ mconcat [ Js.declare [(v, l)], altsJs ]
        go (StgOpApp (StgFCallOp f g) args _ty) = mconcat <$> sequence
            [ declareForeignFunctionCallResult binder f g args, return altsJs ]
        go (StgOpApp (StgPrimOp op) args _ty) =
            declarePrimitiveOperationResult binder op args altsJs live
        go (StgOpApp (StgPrimCallOp call) args _ty) = mconcat <$> sequence
            [ declarePrimitiveCallResult binder call args, return altsJs ]
        go e = do
          v <- stgIdToJsId binder
          f <- Js.function [] <$> expression e
          return $ Js.declareFunctionCallResult v f [] altsJs live

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
#if WORD_SIZE_IN_BITS == 32
            c  <- cases
            return $ Js.switch n dc c
#else
            c  <- stringCases
            return $ Js.switch (Js.nativeMethodCall n "toString" []) dc c
#endif
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
    stringCases = sequence . map stringAlternative . filter (not . isDefault) $ alts
    stringAlternative :: Javascript js => (Stg.AltCon, [Stg.Id], [Bool], StgExpr) -> Gen (Expression js, js)
    stringAlternative alt = do
        c <- alternativeStringConst alt
        b <- alternativeBody alt
        return (c, b)
    alternativeStringConst :: Javascript js => (Stg.AltCon, [Stg.Id], [Bool], StgExpr) -> Gen (Expression js)
    alternativeStringConst (alt, _args, _useMask, _expr) =
      case alt
      of DataAlt con -> return $ Js.int (dataConTag con)
         LitAlt lit  -> stgLiteralToJsString lit
         DEFAULT     -> panic "Default alternative!"

unpackData :: Javascript js => Expression js -> [Bool] -> [Stg.Id] -> Gen js
unpackData object mask args = Js.declare <$> sequence [f n arg | (n, True, arg) <- zip3 [(0::Int)..] mask args]
  where
    f n arg = do
        a <- stgIdToJsId arg
        return (a, (Js.subscript object (Js.int n)))

