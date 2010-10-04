module Generator.PrimOp
  ( returnPrimitiveOperationResult
  , bindPrimitiveOperationResult
  ) where

import Id as Stg
import StgSyn as Stg
import PrimOp
import Javascript.Language as Js

import Generator.Helpers

returnPrimitiveOperationResult :: Javascript js => PrimOp -> [StgArg] -> js
returnPrimitiveOperationResult op args =
  case primOp op args
  of Just e -> Js.return e
     Nothing -> Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

bindPrimitiveOperationResult :: Javascript js => Stg.Id -> PrimOp -> [StgArg] -> js
bindPrimitiveOperationResult id op args =
  case primOp op args
  of Just e -> stgIdToJsDecl id e
     Nothing -> Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

-- | primOp tries to implement GHC primitive operations as described at
--   http://www.haskell.org/ghc/docs/6.12.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html
primOp :: Javascript js => PrimOp -> [StgArg] -> Maybe (Expression js)
-- char:
primOp CharGtOp [a, b] = Just $ boolOp Js.greater a b
primOp CharGeOp [a, b] = Just $ boolOp Js.greaterOrEqual a b
primOp CharEqOp [a, b] = Just $ boolOp Js.equal a b
primOp CharNeOp [a, b] = Just $ boolOp Js.notEqual a b
primOp CharLtOp [a, b] = Just $ boolOp Js.less  a b
primOp CharLeOp [a, b] = Just $ boolOp Js.lessOrEqual a b
primOp OrdOp    [a]    = Just $ Js.nativeMethodCall (stgArgToJs a) "charCodeAt" [Js.int (0 :: Int)]
primOp ChrOp    [a]    = Just $ Js.nativeMethodCall (Js.var "String") "fromCharCode" [stgArgToJs a]

-- int:
primOp IntGtOp  [a, b] = Just $ boolOp Js.greater  a b
primOp IntGeOp  [a, b] = Just $ boolOp Js.greaterOrEqual a b
primOp IntEqOp  [a, b] = Just $ boolOp Js.equal a b
primOp IntNeOp  [a, b] = Just $ boolOp Js.notEqual a b
primOp IntLtOp  [a, b] = Just $ boolOp Js.less  a b
primOp IntLeOp  [a, b] = Just $ boolOp Js.lessOrEqual a b
primOp IntAddOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "add" [stgArgToJs a, stgArgToJs b]
primOp IntSubOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "sub" [stgArgToJs a, stgArgToJs b]
primOp IntMulOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp IntNegOp [a]    = Just $ Js.unaryMinus (stgArgToJs a)

-- overflow sensitive operations:
primOp IntMulMayOfloOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "mulIntMayOflo" [stgArgToJs a, stgArgToJs b]
primOp IntAddCOp       [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addC" [stgArgToJs a, stgArgToJs b]
primOp IntSubCOp       [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "subC" [stgArgToJs a, stgArgToJs b]

primOp IntQuotOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "quot" [stgArgToJs a, stgArgToJs b]
primOp IntRemOp  [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "rem" [stgArgToJs a, stgArgToJs b]
primOp ISllOp     [a, b] = Just $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp ISrlOp     [a, b] = Just $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp ISraOp     [a, b] = Just $ Js.shiftRA (stgArgToJs a) (stgArgToJs b)
primOp Int2WordOp  [a] = Just $ stgArgToJs a

-- word:
-- imlement word as a bit reinterpretation of int
primOp WordGtOp  [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Word") "gt" [stgArgToJs a, stgArgToJs b]
primOp WordGeOp  [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Word") "ge" [stgArgToJs a, stgArgToJs b]
primOp WordEqOp  [a, b] = Just $ boolOp Js.equal a b
primOp WordNeOp  [a, b] = Just $ boolOp Js.notEqual a b
primOp WordLtOp  [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Word") "lt" [stgArgToJs a, stgArgToJs b]
primOp WordLeOp  [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Word") "le" [stgArgToJs a, stgArgToJs b]
primOp WordAddOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "add" [stgArgToJs a, stgArgToJs b]
primOp WordSubOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "sub" [stgArgToJs a, stgArgToJs b]
primOp WordMulOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp WordRemOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "rem" [stgArgToJs a, stgArgToJs b]
primOp SllOp     [a, b] = Just $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp SrlOp     [a, b] = Just $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp AndOp     [a, b] = Just $ Js.bitAnd (stgArgToJs a) (stgArgToJs b)
primOp OrOp      [a, b] = Just $ Js.bitOr (stgArgToJs a) (stgArgToJs b)
primOp XorOp     [a, b] = Just $ Js.bitXOr (stgArgToJs a) (stgArgToJs b)
primOp NotOp     [a] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "bitNot" [stgArgToJs a]
primOp Word2IntOp[a] = Just $ stgArgToJs a

primOp IndexOffAddrOp_Char [a, b] = Just $ Js.subscript (stgArgToJs a) (stgArgToJs b)
primOp DataToTagOp [a] = Just $ Js.property (stgArgToJs a) "tag"
primOp _ _ = Nothing

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> StgArg -> StgArg -> Expression js
boolOp op a b = Js.ternary (op (stgArgToJs a) (stgArgToJs b)) haskellTrue haskellFalse

