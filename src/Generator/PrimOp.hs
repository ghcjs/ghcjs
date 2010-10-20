module Generator.PrimOp
  ( returnPrimitiveOperationResult
  , bindPrimitiveOperationResult
  ) where

import Id as Stg
import StgSyn as Stg
import PrimOp
import Javascript.Language as Js

import Generator.Helpers
import RTS.Objects

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
primOp IntGtOp  [a, b] = Just $ boolOp Js.greater a b
primOp IntGeOp  [a, b] = Just $ boolOp Js.greaterOrEqual a b
primOp IntEqOp  [a, b] = Just $ boolOp Js.equal a b
primOp IntNeOp  [a, b] = Just $ boolOp Js.notEqual a b
primOp IntLtOp  [a, b] = Just $ boolOp Js.less  a b
primOp IntLeOp  [a, b] = Just $ boolOp Js.lessOrEqual a b

-- $hs.Int.addCarry(a, b, 0)[0]
primOp IntAddOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp IntSubOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]
primOp IntMulOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp IntNegOp [a]    = Just $ Js.unaryMinus (stgArgToJs a)

-- overflow sensitive operations:
-- (a >>> 16) == 0 && (b >>> 16) == 0
primOp IntMulMayOfloOp [a, b] = Just $ Js.and (Js.equal (Js.shiftRA (stgArgToJs a) (Js.int (16 :: Int))) (Js.int (0 :: Int))) (Js.equal (Js.shiftRA (stgArgToJs b) (Js.int (16 :: Int))) (Js.int (0 :: Int)))

-- $hs.Int.addCarry(a, b, 0)
primOp IntAddCOp       [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)
primOp IntSubCOp       [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]

-- (a / b) & ~0
primOp IntQuotOp [a, b] = Just $ Js.bitAnd (Js.divide (stgArgToJs a) (stgArgToJs b)) (Js.bitNot $ Js.int (0 :: Int))
primOp IntRemOp  [a, b] = Just $ Js.mod (stgArgToJs a) (stgArgToJs b)
primOp ISllOp     [a, b] = Just $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp ISrlOp     [a, b] = Just $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp ISraOp     [a, b] = Just $ Js.shiftRA (stgArgToJs a) (stgArgToJs b)
primOp Int2WordOp  [a] = Just $ stgArgToJs a

-- word:
-- imlement word as a bit reinterpretation of int
primOp WordGtOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property haskellRoot "Word") "gt" [stgArgToJs a, stgArgToJs b]
primOp WordGeOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property haskellRoot "Word") "ge" [stgArgToJs a, stgArgToJs b]
primOp WordEqOp  [a, b] = Just $ boolOp Js.equal a b
primOp WordNeOp  [a, b] = Just $ boolOp Js.notEqual a b
primOp WordLtOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property haskellRoot "Word") "lt" [stgArgToJs a, stgArgToJs b]
primOp WordLeOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property haskellRoot "Word") "le" [stgArgToJs a, stgArgToJs b]

-- $hs.Int.addCarry(a, b, 0)[0]
primOp WordAddOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp WordSubOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property haskellRoot "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]
primOp WordMulOp [a, b] = Just $ Js.nativeMethodCall (Js.property haskellRoot "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp SllOp     [a, b] = Just $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp SrlOp     [a, b] = Just $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp AndOp     [a, b] = Just $ Js.bitAnd (stgArgToJs a) (stgArgToJs b)
primOp OrOp      [a, b] = Just $ Js.bitOr (stgArgToJs a) (stgArgToJs b)
primOp XorOp     [a, b] = Just $ Js.bitXOr (stgArgToJs a) (stgArgToJs b)
primOp NotOp     [a] = Just $ Js.bitNot (stgArgToJs a)
primOp Word2IntOp[a] = Just $ stgArgToJs a

primOp IndexOffAddrOp_Char [a, b] = Just $ Js.subscript (stgArgToJs a) (stgArgToJs b)
primOp DataToTagOp [a] = Just $ haskellConAppTag (stgArgToJs a)
primOp _ _ = Nothing

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> StgArg -> StgArg -> Expression js
boolOp op a b = jsBoolToHs $ op (stgArgToJs a) (stgArgToJs b)

