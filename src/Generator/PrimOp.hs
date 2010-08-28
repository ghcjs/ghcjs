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

primOp :: Javascript js => PrimOp -> [StgArg] -> Maybe (Expression js)
-- char:
primOp CharGtOp [a, b] = Just $ boolOp ">" a b
primOp CharGeOp [a, b] = Just $ boolOp ">=" a b
primOp CharEqOp [a, b] = Just $ boolOp "==" a b
primOp CharNeOp [a, b] = Just $ boolOp "!=" a b
primOp CharLtOp [a, b] = Just $ boolOp "<"  a b
primOp CharLeOp [a, b] = Just $ boolOp "<=" a b
primOp OrdOp    [a]    = Just $ Js.nativeMethodCall (stgArgToJs a) "charCodeAt" [Js.int (0 :: Int)]
primOp ChrOp    [a]    = Just $ Js.nativeMethodCall (Js.var "String") "fromCharCode" [stgArgToJs a]

-- int:
primOp IntGtOp  [a, b] = Just $ boolOp ">"  a b
primOp IntGeOp  [a, b] = Just $ boolOp ">=" a b
primOp IntEqOp  [a, b] = Just $ boolOp "==" a b
primOp IntNeOp  [a, b] = Just $ boolOp "!=" a b
primOp IntLtOp  [a, b] = Just $ boolOp "<"  a b
primOp IntLeOp  [a, b] = Just $ boolOp "<=" a b
primOp IntAddOp [a, b] = Just $ Js.binOp "+" (stgArgToJs a) (stgArgToJs b)
primOp IntSubOp [a, b] = Just $ Js.binOp "-" (stgArgToJs a) (stgArgToJs b)
primOp IntMulOp [a, b] = Just $ Js.binOp "*" (stgArgToJs a) (stgArgToJs b)
primOp IntNegOp [a]    = Just $ Js.leftUnaryOp "-" (stgArgToJs a)

-- overflow sensitive operations:
primOp IntMulMayOfloOp [_, _] = Just $ Js.int (0 :: Int) -- State that no overflows are possible what so ever
primOp IntAddCOp       [a, b] = Just $ Js.list [Js.binOp "+" (stgArgToJs a) (stgArgToJs b), Js.int (0 :: Int)] -- Second member is alway zero, meaning that there are no overflows
primOp IntSubCOp       [a, b] = Just $ Js.list [Js.binOp "-" (stgArgToJs a) (stgArgToJs b), Js.int (0 :: Int)] -- Second member is alway zero, meaning that there are no overflows

-- FIXME: is there native Javascript mechanism for this?
primOp IntQuotOp [a, b] = Just $ Generator.PrimOp.floor a b
primOp IntRemOp  [a, b] = Just $ Js.binOp "-" (stgArgToJs a) (Js.binOp "*" (Generator.PrimOp.floor a b) (stgArgToJs b))

primOp IndexOffAddrOp_Char [a, b] = Just $ Js.subscript (stgArgToJs a) (stgArgToJs b)
primOp DataToTagOp [a] = Just $ Js.property (stgArgToJs a) "tag"
primOp _ _ = Nothing

floor :: Javascript js => StgArg -> StgArg -> Expression js
floor a b = Js.nativeMethodCall (Js.var "Math") "floor" [Js.binOp "/" (stgArgToJs a) (stgArgToJs b)]

boolOp :: Javascript js => String -> StgArg -> StgArg -> Expression js
boolOp op a b = Js.ternary (Js.binOp op (stgArgToJs a) (stgArgToJs b)) haskellTrue haskellFalse

