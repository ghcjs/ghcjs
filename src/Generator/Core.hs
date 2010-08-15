module Generator.Core (declarations, definitions, withBindings) where

import Data.List (find)

import Panic (panic)

import DataCon as Stg (DataCon, dataConTag)
import Id as Stg (Id)
import CoreSyn as Stg (AltCon (DataAlt, LitAlt, DEFAULT))
import StgSyn as Stg

import qualified Javascript.Language as Js

import Generator.Helpers
import Generator.PrimOp (primitiveOperation)
import Generator.FFI (foreignFunctionCall, primitiveCall)

binding :: StgBinding -> Js.Program
binding = bindings . stgBindingToList

bindings :: [(Id, StgRhs)] -> Js.Program
bindings binds = Js.sequence [declarations binds, definitions binds]

withBindings :: (Id -> StgRhs -> Js.Program) -> [(Id, StgRhs)] -> Js.Program
withBindings f = Js.sequence . map (uncurry f)

declarations :: [(Id, StgRhs)] -> Js.Program
declarations = withBindings declaration

definitions :: [(Id, StgRhs)] -> Js.Program
definitions = withBindings definition

declaration :: Id -> StgRhs -> Js.Program
declaration id rhs = stgIdToJsDecl id (creation rhs)

creation :: StgRhs -> Js.Expression
creation (StgRhsCon _cc con _args) = dataCreation con
creation rhs@(StgRhsClosure _cc _bi _fvs upd_flag _srt _args _body)
  | isUpdatable upd_flag = Js.new (Js.property haskellRoot "Thunk") []
  | otherwise = Js.new (Js.property haskellRoot "Func") [Js.int (stgRhsArity rhs)]

definition :: Stg.Id -> StgRhs -> Js.Program
definition id (StgRhsCon _cc _con args) = dataEvaluation (stgIdToJs id) (map stgArgToJs args)
definition id (StgRhsClosure _cc _bi _fvs upd_flag _srt args body) =
  Js.sequence
    [ Js.assignProperty object "evaluated" (Js.bool $ isUpdatable upd_flag)
    , Js.assignProperty object evalFunctionName $
        Js.function (map stgIdToJsId args) (expression body)
    ]
  where object = (stgIdToJs id)
        evalFunctionName
          | isUpdatable upd_flag = "evaluateOnce"
          | otherwise = "evaluate"

dataCreation :: DataCon -> Js.Expression
dataCreation con = Js.new (Js.property haskellRoot "Data") [Js.int (dataConTag con)]

dataEvaluation :: Js.Expression -> [Js.Expression] -> Js.Program
dataEvaluation object args =
  Js.sequence
    [ Js.assignProperty object "evaluated" Js.true
    , Js.assignProperty object "data" (Js.list args)
    ]

expression :: StgExpr -> Js.Program
expression (StgCase expr _liveVars _liveRhsVars bndr _srt alttype alts) =
  caseExpression expr bndr alttype alts
expression (StgLet bndn body) = Js.sequence [binding bndn, expression body]
expression (StgLetNoEscape _ _ bndn body) = Js.sequence [binding bndn, expression body]
expression (StgSCC _ expr) = expression expr
expression (StgTick _ _ expr) = expression expr
expression (StgApp f args) = Js.jumpToMethod (stgIdToJs f) "hscall" (map stgArgToJs args)
expression (StgLit lit) = Js.return . stgLiteralToJs $ lit
expression (StgConApp con args) =
  Js.sequence
    [ Js.declare "$res" (dataCreation con)
    , dataEvaluation (Js.var "$res") (map stgArgToJs args)
    , Js.return . Js.var $ "$res"
    ]
expression (StgOpApp (StgFCallOp f g) args _ty) = Js.return $ foreignFunctionCall f g args
expression (StgOpApp (StgPrimOp op) args _ty) = Js.return $ primitiveOperation op args
expression (StgOpApp (StgPrimCallOp call) args _ty) = Js.return $ primitiveCall call args
expression (StgLam{}) = panic "unexpected StgLam" -- StgLam is used *only* during CoreToStg's work (StgSyn.lhs:196)

caseExpression :: StgExpr -> Stg.Id -> Stg.AltType -> [StgAlt] -> Js.Program
caseExpression expr bndr alttype alts =
  Js.sequence
    [ caseExpressionScrut bndr expr
    , caseExpressionAlternatives bndr alttype alts
    ]

caseExpressionScrut :: Stg.Id -> StgExpr -> Js.Program
caseExpressionScrut binder expr = go expr
  where go (StgConApp con args) =
          Js.sequence
            [ stgIdToJsDecl binder (dataCreation con)
            , dataEvaluation (stgIdToJs binder) (map stgArgToJs args)
            ]
        go (StgApp f args) =
          case args
          of [] ->
               Js.sequence
                 [ stgIdToJsDecl binder name
                 , Js.if_ (Js.not $ Js.property name "evaluated") $
                     Js.assignMethodCallResult (stgIdToJs binder) name "hscall" []
                 ]
             _ -> stgIdToJsDeclareMethodCallResult binder name "hscall" (map stgArgToJs args)
          where name = stgIdToJs f
        go (StgLit lit) = stgIdToJsDecl binder $ stgLiteralToJs $ lit
        go (StgOpApp (StgFCallOp f g) args _ty) = stgIdToJsDecl binder $ foreignFunctionCall f g args
        go (StgOpApp (StgPrimOp op) args _ty) = stgIdToJsDecl binder $ primitiveOperation op args
        go (StgOpApp (StgPrimCallOp call) args _ty) = stgIdToJsDecl binder $ primitiveCall call args
        go e = stgIdToJsDeclareFunctionCallResult binder f []
          where f = Js.function [] (expression e)

caseExpressionAlternatives :: Stg.Id -> Stg.AltType -> [(Stg.AltCon, [Stg.Id], [Bool], StgExpr)] -> Js.Program
caseExpressionAlternatives bndr altType [(_altCon, args, useMask, expr)] =
  case altType
  of PolyAlt {} -> jsexpr
     PrimAlt {} -> jsexpr
     UbxTupAlt {} -> argsAndExpr
     AlgAlt {} -> argsAndExpr
  where argsAndExpr = Js.sequence [unpackData (stgIdToJs bndr) useMask args, jsexpr]
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
         return $ expression expr
    isDefault (DEFAULT, _, _, _) = True
    isDefault _ = False
    cases = map alternative . filter (not . isDefault) $ alts
    alternative alt = (alternativeConst alt, alternativeBody alt)
    alternativeBody (alt, args, useMask, expr) =
      case alt
      of DataAlt _ -> Js.sequence [unpackData name useMask args, expression expr]
         LitAlt _ -> expression expr
         DEFAULT  -> panic "Default alternative!"
    alternativeConst (alt, _args, _useMask, _expr) =
      case alt
      of DataAlt con -> Js.int (dataConTag con)
         LitAlt lit -> stgLiteralToJs lit
         DEFAULT  -> panic "Default alternative!"

unpackData :: Js.Expression -> [Bool] -> [Stg.Id] -> Js.Program
unpackData name mask args = Js.sequence [f n arg | (n, True, arg) <- zip3 [(0::Int)..] mask args]
  where f n arg = stgIdToJsDecl arg (Js.subscript (Js.property name "data") (Js.int n))

