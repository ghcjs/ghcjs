module Generator.Core (withBindings, creation, definition) where

import Data.List (find)
import Data.Monoid (mconcat)

import Panic (panic)

import DataCon as Stg (DataCon, dataConTag)
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

binding :: Javascript js => StgBinding -> js
binding = bindings . stgBindingToList

bindings :: Javascript js => [(Stg.Id, StgRhs)] -> js
bindings binds = mconcat [withBindings localDecl binds, withBindings localDef binds]
  where localDecl id rhs
          | isExternalId id = Js.assign (stgIdToJs id) (creation rhs)
          | otherwise = Js.declare (stgIdToJsId id) (creation rhs)
        localDef = definition . stgIdToJs

withBindings :: Javascript js => (Stg.Id -> StgRhs -> js) -> [(Stg.Id, StgRhs)] -> js
withBindings f = mconcat . map (uncurry f)

creation :: Javascript js => StgRhs -> Expression js
creation (StgRhsCon _cc con _args) = dataCreation con
creation rhs@(StgRhsClosure _cc _bi _fvs upd_flag _srt _args _body)
  | isUpdatable upd_flag = Js.new RTS.thunk []
  | otherwise = Js.new RTS.func [Js.int (stgRhsArity rhs)]

definition :: Javascript js => Expression js -> StgRhs -> js
definition object (StgRhsCon _cc con args) = dataDefinition con object (map stgArgToJs args)
definition object (StgRhsClosure _cc _bi _fvs upd_flag _srt args body) =
  Js.assignProperty object method $
    Js.function (map stgIdToJsId args) (expression body)
  where method
          | isUpdatable upd_flag = RTS.thunkEvalOnceFunctionName
          | otherwise = RTS.evalFunctionName

dataCreation :: Javascript js => DataCon -> Expression js
dataCreation con
  | isUnboxedTupleCon con = Js.null
  | otherwise = Js.new RTS.conApp [Js.int (dataConTag con)]

dataDefinition :: Javascript js => DataCon -> Expression js -> [Expression js] -> js
dataDefinition con object args
  | isUnboxedTupleCon con = Js.assign object (Js.list args)
  | otherwise = Js.assign (RTS.conAppArgVector object) (Js.list args)

expression :: Javascript js => StgExpr -> js
expression (StgCase expr _liveVars _liveRhsVars bndr _srt alttype alts) =
  caseExpression expr bndr alttype alts
expression (StgLet bndn body) = mconcat [binding bndn, expression body]
expression (StgLetNoEscape _ _ bndn body) = mconcat [binding bndn, expression body]
expression (StgSCC _ expr) = expression expr
expression (StgTick _ _ expr) = expression expr
expression (StgApp f []) =
  Js.ifelse (RTS.isNotEvaluatedAndNotPrimitive object)
    (Js.jumpToMethod object RTS.applyMethodName [])
    (Js.return object)
  where object = stgIdToJs f
expression (StgApp f args) = Js.jumpToMethod (stgIdToJs f) RTS.applyMethodName (map stgArgToJs args)
expression (StgLit lit) = Js.return . stgLiteralToJs $ lit
expression (StgConApp con args)
  | isUnboxedTupleCon con = Js.return (Js.list jsargs)
  | otherwise =
      mconcat
        [ Js.declare "$res" (dataCreation con)
        , dataDefinition con (Js.var "$res") jsargs
        , Js.return . Js.var $ "$res"
        ]
  where jsargs = map stgArgToJs args
expression (StgOpApp (StgFCallOp f g) args _ty) = returnForeignFunctionCallResult f g args
expression (StgOpApp (StgPrimOp op) args _ty) = returnPrimitiveOperationResult op args
expression (StgOpApp (StgPrimCallOp call) args _ty) = returnPrimitiveCallResult call args
expression (StgLam{}) = panic "unexpected StgLam" -- StgLam is used *only* during CoreToStg's work (StgSyn.lhs:196)

caseExpression :: Javascript js => StgExpr -> Stg.Id -> Stg.AltType -> [StgAlt] -> js
caseExpression expr bndr alttype alts =
  mconcat
    [ caseExpressionScrut bndr expr
    , caseExpressionAlternatives bndr alttype alts
    ]

-- | 'caseExpressionScrut' is absolutely the same as expression
-- the difference is that 'expression' "returns" result
-- but 'caseExpressionScrut' "binds" result
caseExpressionScrut :: Javascript js => Stg.Id -> StgExpr -> js
caseExpressionScrut binder expr = go expr
  where go (StgConApp con args)
          | isUnboxedTupleCon con = Js.declare (stgIdToJsId binder) (Js.list jsargs)
          | otherwise =
              mconcat
                [ Js.declare (stgIdToJsId binder) (dataCreation con)
                , dataDefinition con (stgIdToJs binder) jsargs
                ]
          where jsargs = map stgArgToJs args
        go (StgApp f []) =
          mconcat
            [ Js.declare (stgIdToJsId binder) object
            , Js.if_ (RTS.isNotEvaluatedAndNotPrimitive object) $
                Js.assignMethodCallResult (stgIdToJs binder) object RTS.applyMethodName []
            ]
          where object = stgIdToJs f
        go (StgApp f args) =
            Js.declareMethodCallResult (stgIdToJsId binder) object RTS.applyMethodName (map stgArgToJs args)
          where object = stgIdToJs f
        go (StgLit lit) = Js.declare (stgIdToJsId binder) $ stgLiteralToJs $ lit
        go (StgOpApp (StgFCallOp f g) args _ty) = bindForeignFunctionCallResult binder f g args
        go (StgOpApp (StgPrimOp op) args _ty) = bindPrimitiveOperationResult binder op args
        go (StgOpApp (StgPrimCallOp call) args _ty) = bindPrimitiveCallResult binder call args
        go e = Js.declareFunctionCallResult (stgIdToJsId binder) f []
          where f = Js.function [] (expression e)

caseExpressionAlternatives :: Javascript js => Stg.Id -> Stg.AltType -> [(Stg.AltCon, [Stg.Id], [Bool], StgExpr)] -> js
caseExpressionAlternatives bndr altType [(_altCon, args, useMask, expr)] =
  case altType
  of PolyAlt {} -> jsexpr
     PrimAlt {} -> jsexpr
     UbxTupAlt {} -> process object
     AlgAlt {} -> process (RTS.conAppArgVector object)
  where object = stgIdToJs bndr
        process obj = mconcat [unpackData obj useMask args, jsexpr]
        jsexpr = expression expr
caseExpressionAlternatives bndr altType alts =
  case altType
    of PolyAlt   {} -> panic "multiple case alternatives for PolyAlt"
       UbxTupAlt {} -> panic "multiple case alternatives for UbxTupAlt"
       PrimAlt   {} -> Js.switch name defaultCase cases
       AlgAlt    {} -> Js.switch (RTS.conAppTag name) defaultCase cases
  where
    name = stgIdToJs bndr
    defaultCase =
      do (_, _, _, expr) <- find isDefault alts
         Prelude.return $ expression expr
    isDefault (DEFAULT, _, _, _) = True
    isDefault _ = False
    cases = map alternative . filter (Prelude.not . isDefault) $ alts
    alternative alt = (alternativeConst alt, alternativeBody alt)
    alternativeBody (alt, args, useMask, expr) =
      case alt
      of DataAlt _ -> mconcat [unpackData (RTS.conAppArgVector name) useMask args, expression expr]
         LitAlt _ -> expression expr
         DEFAULT  -> panic "Default alternative!"
    alternativeConst (alt, _args, _useMask, _expr) =
      case alt
      of DataAlt con -> Js.int (dataConTag con)
         LitAlt lit -> stgLiteralToJs lit
         DEFAULT  -> panic "Default alternative!"

unpackData :: Javascript js => Expression js -> [Bool] -> [Stg.Id] -> js
unpackData object mask args = mconcat [f n arg | (n, True, arg) <- zip3 [(0::Int)..] mask args]
  where f n arg = Js.declare (stgIdToJsId arg) (Js.subscript object (Js.int n))

