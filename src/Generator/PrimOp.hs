-- | Tries to implement GHC primitive operations as described at
--   http://www.haskell.org/ghc/docs/6.12.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html
-- Char# is Javascript string
-- Int# is Javascript number
-- Word# is Javascript number
-- Float# is Javascript number
-- Doable# is Javascript number
-- Addr# is Javascript string
-- MutableByteArray# s is Javascript string
-- ByteArray# s is Javascript string
module Generator.PrimOp
  ( returnPrimitiveOperationResult
  , declarePrimitiveOperationResult
  ) where

import Id as Stg
import StgSyn as Stg
import PrimOp
import Javascript.Language as Js

import qualified RTS.Objects as RTS
import Generator.Helpers

returnPrimitiveOperationResult :: Javascript js => PrimOp -> [StgArg] -> js
returnPrimitiveOperationResult op args =
  case primOp op args
  of Just e -> Js.return e
     Nothing -> Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

declarePrimitiveOperationResult :: Javascript js => Stg.Id -> PrimOp -> [StgArg] -> js
declarePrimitiveOperationResult id op args =
  case primOp op args
  of Just e -> Js.declare (stgIdToJsId id) e
     Nothing -> Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

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
primOp IntAddOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp IntSubOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]
primOp IntMulOp [a, b] = Just $ Js.nativeMethodCall (Js.property RTS.root "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp IntNegOp [a]    = Just $ Js.unaryMinus (stgArgToJs a)

-- overflow sensitive operations:
-- (a >>> 16) == 0 && (b >>> 16) == 0
primOp IntMulMayOfloOp [a, b] = Just $ Js.and (test a) (test b)
  where zero = Js.int (0 :: Int)
        sixteen = Js.int (16 :: Int)
        test x = Js.equal (Js.shiftRA (stgArgToJs x) sixteen) zero

-- $hs.Int.addCarry(a, b, 0)
primOp IntAddCOp       [a, b] = Just $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)
primOp IntSubCOp       [a, b] = Just $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]

-- (a / b) & ~0
primOp IntQuotOp [a, b] = Just $ Js.bitAnd (Js.divide (stgArgToJs a) (stgArgToJs b)) (Js.bitNot $ Js.int (0 :: Int))
primOp IntRemOp  [a, b] = Just $ Js.mod (stgArgToJs a) (stgArgToJs b)
primOp ISllOp     [a, b] = Just $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp ISrlOp     [a, b] = Just $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp ISraOp     [a, b] = Just $ Js.shiftRA (stgArgToJs a) (stgArgToJs b)
primOp Int2WordOp  [a] = Just $ stgArgToJs a

-- word:
-- imlement word as a bit reinterpretation of int
primOp WordGtOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "gt" [stgArgToJs a, stgArgToJs b]
primOp WordGeOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "ge" [stgArgToJs a, stgArgToJs b]
primOp WordEqOp  [a, b] = Just $ boolOp Js.equal a b
primOp WordNeOp  [a, b] = Just $ boolOp Js.notEqual a b
primOp WordLtOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "lt" [stgArgToJs a, stgArgToJs b]
primOp WordLeOp  [a, b] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "le" [stgArgToJs a, stgArgToJs b]

-- $hs.Int.addCarry(a, b, 0)[0]
primOp WordAddOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp WordSubOp [a, b] = Just $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]
primOp WordMulOp [a, b] = Just $ Js.nativeMethodCall (Js.property RTS.root "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp SllOp     [a, b] = Just $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp SrlOp     [a, b] = Just $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp AndOp     [a, b] = Just $ Js.bitAnd (stgArgToJs a) (stgArgToJs b)
primOp OrOp      [a, b] = Just $ Js.bitOr (stgArgToJs a) (stgArgToJs b)
primOp XorOp     [a, b] = Just $ Js.bitXOr (stgArgToJs a) (stgArgToJs b)
primOp NotOp     [a] = Just $ Js.bitNot (stgArgToJs a)
primOp Word2IntOp[a] = Just $ stgArgToJs a

primOp Narrow8IntOp [a] = Just $
  Js.ternary (Js.greaterOrEqual arg zero)
    (Js.bitAnd arg bitMask7)
    (inv (inv arg `Js.bitAnd` bitMask7))
  where arg = stgArgToJs a
        inv f = Js.bitXOr f (Js.bitNot zero)
        bitMask7 = Js.int (127 :: Int)
        zero = Js.int (0 :: Int)
primOp Narrow16IntOp [a] = Just $
  Js.ternary (Js.greaterOrEqual arg zero)
    (Js.bitAnd arg bitMask15)
    (inv (inv arg `Js.bitAnd` bitMask15))
  where arg = stgArgToJs a
        inv f = Js.bitXOr f (Js.bitNot zero)
        bitMask15 = Js.int (32767 :: Int)
        zero = Js.int (0 :: Int)
primOp Narrow32IntOp [a] = Just $ stgArgToJs a
primOp Narrow8WordOp [a] = Just $ Js.bitAnd (stgArgToJs a) (Js.int (0xFF :: Int))
primOp Narrow16WordOp [a] = Just $ Js.bitAnd (stgArgToJs a) (Js.int (0xFFFF :: Int))
primOp Narrow32WordOp [a] = Just $ stgArgToJs a

primOp DataToTagOp [a] = Just $ RTS.conAppTag (stgArgToJs a)

primOp IndexOffAddrOp_Char [a, b] = Just $ Js.nativeMethodCall (stgArgToJs a) "charAt" [stgArgToJs b]

primOp NewMutVarOp   [a, s]    = Just $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "newMutVar" [stgArgToJs a, stgArgToJs s]
primOp ReadMutVarOp  [a, s]    = Just $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "read" [stgArgToJs a, stgArgToJs s]
primOp WriteMutVarOp [a, b, s] = Just $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "write" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp SameMutVarOp  [a, b, s] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "same" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp AtomicModifyMutVarOp  [a, b, s] = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "atomicModify" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
-- primOp CasMutVarOp   [a, b, c, s] = Just $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "cas" [stgArgToJs a, stgArgToJs b, stgArgToJs c, stgArgToJs s]

primOp ForkOp [a, s]      = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "Thread") "fork" [stgArgToJs a, stgArgToJs s]
primOp ForkOnOp [a, b, s] = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "Thread") "forkOn" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp YieldOp [s]        = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "Thread") "yieldThread" [stgArgToJs s]
primOp MyThreadIdOp [s]   = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "Thread") "myThreadId" [stgArgToJs s]
primOp NoDuplicateOp [s]  = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "Thread") "noDuplicate" [stgArgToJs s]

primOp CatchOp [a, b, s] = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "Exception") "tryCatch" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp RaiseOp [a] = Just $ Js.nativeMethodCall (Js.property RTS.root "Exception") "raise" [stgArgToJs a]
primOp RaiseIOOp [a, s] = Just $ Js.nativeMethodCall (Js.property RTS.root "Exception") "raiseIO" [stgArgToJs a, stgArgToJs s]
-- primOp MaskAsyncExceptionsOp [a] = Just $ Js.nativeMethodCall (Js.property RTS.root "Exception") "maskAsyncExceptions" [stgArgToJs a]
-- primOp MaskUninterruptibleOp [a] = Just $ Js.nativeMethodCall (Js.property RTS.root "Exception") "maskUninterruptible" [stgArgToJs a]
-- primOp UnmaskAsyncExceptionsOp [a] = Just $ Js.nativeMethodCall (Js.property RTS.root "Exception") "unmaskAsyncExceptions" [stgArgToJs a]
-- primOp MaskStatus [s] = Just $ Js.nativeMethodCall (Js.property RTS.root "Exception") "getMaskingState" [stgArgToJs s]

primOp NewMVarOp     [s]       = Just $ Js.nativeMethodCall (Js.property RTS.root "MVar") "newMVar" [stgArgToJs s]
primOp TakeMVarOp    [a, s]    = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "MVar") "take" [stgArgToJs a, stgArgToJs s]
primOp TryTakeMVarOp [a, s]    = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "MVar") "tryTake" [stgArgToJs a, stgArgToJs s]
primOp PutMVarOp     [a, b, s] = Just $ yield $ Js.nativeMethodCall (Js.property RTS.root "MVar") "put" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp SameMVarOp    [a, b, s] = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MVar") "same" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp IsEmptyMVarOp [a, s]    = Just $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MVar") "isEmpty" [stgArgToJs a, stgArgToJs s]

primOp _ _ = Nothing

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> StgArg -> StgArg -> Expression js
boolOp op a b = jsBoolToHs $ op (stgArgToJs a) (stgArgToJs b)
