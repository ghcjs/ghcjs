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
primOp IntAddOp [a, b] = Just $ Js.plus (stgArgToJs a) (stgArgToJs b)
primOp IntSubOp [a, b] = Just $ Js.minus (stgArgToJs a) (stgArgToJs b)
primOp IntMulOp [a, b] = Just $ Js.multiply (stgArgToJs a) (stgArgToJs b)
primOp IntNegOp [a]    = Just $ Js.unaryMinus (stgArgToJs a)

-- overflow sensitive operations:
primOp IntMulMayOfloOp [_, _] = Just $ Js.int (0 :: Int) -- State that no overflows are possible what so ever
primOp IntAddCOp       [a, b] = Just $ Js.list [Js.plus (stgArgToJs a) (stgArgToJs b), Js.int (0 :: Int)] -- Second member is alway zero, meaning that there are no overflows
primOp IntSubCOp       [a, b] = Just $ Js.list [Js.minus (stgArgToJs a) (stgArgToJs b), Js.int (0 :: Int)] -- Second member is alway zero, meaning that there are no overflows

-- FIXME: is there native Javascript mechanism for this?
primOp IntQuotOp [a, b] = Just $ Generator.PrimOp.floor a b
primOp IntRemOp  [a, b] = Just $ Js.minus (stgArgToJs a) (Js.multiply (Generator.PrimOp.floor a b) (stgArgToJs b))

primOp IndexOffAddrOp_Char [a, b] = Just $ Js.subscript (stgArgToJs a) (stgArgToJs b)
primOp DataToTagOp [a] = Just $ Js.property (stgArgToJs a) "tag"
primOp _ _ = Nothing

floor :: Javascript js => StgArg -> StgArg -> Expression js
floor a b = Js.nativeMethodCall (Js.var "Math") "floor" [Js.divide (stgArgToJs a) (stgArgToJs b)]

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> StgArg -> StgArg -> Expression js
boolOp op a b = Js.ternary (op (stgArgToJs a) (stgArgToJs b)) haskellTrue haskellFalse

