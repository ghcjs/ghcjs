module Generator.Core (declarations, definitions, withBindings) where

import Data.List (find)
import Data.Monoid (mconcat)

import Panic (panic)

import DataCon as Stg (DataCon, dataConTag)
import Id as Stg (Id)
import CoreSyn as Stg (AltCon (DataAlt, LitAlt, DEFAULT))
import StgSyn as Stg

import qualified Javascript.Language as Js
import Javascript.Language (Javascript, Expression)

import Generator.Helpers
import Generator.PrimOp
import Generator.FFI

binding :: Javascript js => StgBinding -> js
binding = bindings . stgBindingToList

bindings :: Javascript js => [(Stg.Id, StgRhs)] -> js
bindings binds = mconcat [declarations binds, definitions binds]

withBindings :: Javascript js => (Stg.Id -> StgRhs -> js) -> [(Stg.Id, StgRhs)] -> js
withBindings f = mconcat . map (uncurry f)

declarations :: Javascript js => [(Stg.Id, StgRhs)] -> js
declarations = withBindings declaration

definitions :: Javascript js => [(Stg.Id, StgRhs)] -> js
definitions = withBindings definition

declaration :: Javascript js => Stg.Id -> StgRhs -> js
declaration id rhs = stgIdToJsDecl id (creation rhs)

creation :: Javascript js => StgRhs -> Expression js
creation (StgRhsCon _cc con _args) = dataCreation con
creation rhs@(StgRhsClosure _cc _bi _fvs upd_flag _srt _args _body)
  | isUpdatable upd_flag = Js.new (Js.property haskellRoot "Thunk") []
  | otherwise = Js.new (Js.property haskellRoot "Func") [Js.int (stgRhsArity rhs)]

definition :: Javascript js => Stg.Id -> StgRhs -> js
definition id (StgRhsCon _cc _con args) = dataDefinition (stgIdToJs id) (map stgArgToJs args)
definition id (StgRhsClosure _cc _bi _fvs upd_flag _srt args body) =
  Js.assignProperty object evalFunctionName $
    Js.function (map stgIdToJsId args) (expression body)
  where object = (stgIdToJs id)
        evalFunctionName
          | isUpdatable upd_flag = "evaluateOnce"
          | otherwise = "evaluate"

dataCreation :: Javascript js => DataCon -> Expression js
dataCreation con = Js.new (Js.property haskellRoot "Data") [Js.int (dataConTag con)]

dataDefinition :: Javascript js => Expression js -> [Expression js] -> js
dataDefinition object args = Js.assignProperty object "data" (Js.list args)

expression :: Javascript js => StgExpr -> js
expression (StgCase expr _liveVars _liveRhsVars bndr _srt alttype alts) =
  caseExpression expr bndr alttype alts
expression (StgLet bndn body) = mconcat [binding bndn, expression body]
expression (StgLetNoEscape _ _ bndn body) = mconcat [binding bndn, expression body]
expression (StgSCC _ expr) = expression expr
expression (StgTick _ _ expr) = expression expr
expression (StgApp f args) = Js.jumpToMethod (stgIdToJs f) "hscall" (map stgArgToJs args)
expression (StgLit lit) = Js.return . stgLiteralToJs $ lit
expression (StgConApp con args) =
  mconcat
    [ Js.declare "$res" (dataCreation con)
    , dataDefinition (Js.var "$res") (map stgArgToJs args)
    , Js.return . Js.var $ "$res"
    ]
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

caseExpressionScrut :: Javascript js => Stg.Id -> StgExpr -> js
caseExpressionScrut binder expr = go expr
  where go (StgConApp con args) =
          mconcat
            [ stgIdToJsDecl binder (dataCreation con)
            , dataDefinition (stgIdToJs binder) (map stgArgToJs args)
            ]
        go (StgApp f args) =
          case args
          of [] ->
               mconcat
                 [ stgIdToJsDecl binder name
                 , Js.if_ (Js.not $ Js.property name "evaluated") $
                     Js.assignMethodCallResult (stgIdToJs binder) name "hscall" []
                 ]
             _ -> stgIdToJsDeclareMethodCallResult binder name "hscall" (map stgArgToJs args)
          where name = stgIdToJs f
        go (StgLit lit) = stgIdToJsDecl binder $ stgLiteralToJs $ lit
        go (StgOpApp (StgFCallOp f g) args _ty) = bindForeignFunctionCallResult binder f g args
        go (StgOpApp (StgPrimOp op) args _ty) = bindPrimitiveOperationResult binder op args
        go (StgOpApp (StgPrimCallOp call) args _ty) = bindPrimitiveCallResult binder call args
        go e = stgIdToJsDeclareFunctionCallResult binder f []
          where f = Js.function [] (expression e)

caseExpressionAlternatives :: Javascript js => Stg.Id -> Stg.AltType -> [(Stg.AltCon, [Stg.Id], [Bool], StgExpr)] -> js
caseExpressionAlternatives bndr altType [(_altCon, args, useMask, expr)] =
  case altType
  of PolyAlt {} -> jsexpr
     PrimAlt {} -> jsexpr
     UbxTupAlt {} -> argsAndExpr
     AlgAlt {} -> argsAndExpr
  where argsAndExpr = mconcat [unpackData (stgIdToJs bndr) useMask args, jsexpr]
        jsexpr = expression expr
caseExpressionAlternatives bndr altType alts =
  case altType
    of PolyAlt   {} -> panic "multiple case alternatives for PolyAlt"
       UbxTupAlt {} -> panic "multiple case alternatives for UbxTupAlt"
       PrimAlt   {} -> Js.switch name defaultCase cases
       AlgAlt    {} -> Js.switch (Js.property name "tag") defaultCase cases
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
      of DataAlt _ -> mconcat [unpackData name useMask args, expression expr]
         LitAlt _ -> expression expr
         DEFAULT  -> panic "Default alternative!"
    alternativeConst (alt, _args, _useMask, _expr) =
      case alt
      of DataAlt con -> Js.int (dataConTag con)
         LitAlt lit -> stgLiteralToJs lit
         DEFAULT  -> panic "Default alternative!"

unpackData :: Javascript js => Expression js -> [Bool] -> [Stg.Id] -> js
unpackData name mask args = mconcat [f n arg | (n, True, arg) <- zip3 [(0::Int)..] mask args]
  where f n arg = stgIdToJsDecl arg (Js.subscript (Js.property name "data") (Js.int n))

