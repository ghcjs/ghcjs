{-# LANGUAGE CPP #-}
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
import Javascript.Language as Js hiding(return)
import qualified Javascript.Language as Js (return)

import qualified RTS.Objects as RTS
import Generator.Helpers
import Data.Monoid (Monoid(..))
import Encoding (zEncodeString)

data Javascript js => PrimOpExp js =
    PlainE (Expression js)
  | TrampolineM (Expression js) Js.Id [Expression js]
  | UnknownOp

returnPrimitiveOperationResult :: Javascript js => PrimOp -> [StgArg] -> Gen js
returnPrimitiveOperationResult op args = do
  exp <- primOp' op args
  case exp of
    PlainE e             -> return $ Js.return e
    TrampolineM o m args -> return $ jumpToMethod o m args
    UnknownOp            -> return $ Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

declarePrimitiveOperationResult :: Javascript js => Stg.Id -> PrimOp -> [StgArg] -> js -> Expression js -> Gen js
declarePrimitiveOperationResult id op args rest live = do
  exp <- primOp' op args
  case exp of
     PlainE e             -> do
        i   <- stgIdToJsId id
        return $ mconcat [Js.declare [(i, e)], rest]
     TrampolineM o m args -> do
        i    <- stgIdToJsId id
        return $ declareMethodCallResult i o m args rest live
     UnknownOp            -> return $ Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

primOp' :: Javascript js => PrimOp -> [StgArg] -> Gen (PrimOpExp js)
primOp' op a = do
    args <- mapM stgArgToJs a
    return $ primOp op args

primOp :: Javascript js => PrimOp -> [Expression js] -> PrimOpExp js
-- char:
primOp CharGtOp [a, b] = PlainE $ boolOp Js.greater a b
primOp CharGeOp [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp CharEqOp [a, b] = PlainE $ boolOp Js.equal a b
primOp CharNeOp [a, b] = PlainE $ boolOp Js.notEqual a b
primOp CharLtOp [a, b] = PlainE $ boolOp Js.less  a b
primOp CharLeOp [a, b] = PlainE $ boolOp Js.lessOrEqual a b
primOp OrdOp    [a]    = PlainE $ Js.nativeMethodCall a "charCodeAt" [Js.int (0 :: Int)]
primOp ChrOp    [a]    = PlainE $ Js.nativeMethodCall (Js.var "String") "fromCharCode" [a]

-- int:
primOp IntGtOp  [a, b] = PlainE $ boolOp Js.greater a b
primOp IntGeOp  [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp IntEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp IntNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp IntLtOp  [a, b] = PlainE $ boolOp Js.less  a b
primOp IntLeOp  [a, b] = PlainE $ boolOp Js.lessOrEqual a b

-- $hs.Int.addCarry(a, b, 0)[0]
primOp IntAddOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [a, b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp IntSubOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [a, Js.bitNot b, Js.int (1 :: Int)]
primOp IntMulOp [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "mul" [a, b]
primOp IntNegOp [a]    = PlainE $ Js.unaryMinus a

-- overflow sensitive operations:
-- (a >>> 16) + (b >>> 16)
primOp IntMulMayOfloOp [a, b] = PlainE $ Js.plus (test a) (test b)
  where sixteen = Js.int (16 :: Int)
        test x = Js.shiftRA x sixteen

-- $hs.Int.addCarry(a, b, 0)
primOp IntAddCOp [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [a, b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)
primOp IntSubCOp [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [a, Js.bitNot b, Js.int (1 :: Int)]

-- (a / b) & ~0
primOp IntQuotOp [a, b] = PlainE $ Js.bitAnd (Js.divide a b) (Js.bitNot $ Js.int (0 :: Int))
primOp IntRemOp  [a, b] = PlainE $ Js.mod a b
primOp ISllOp     [a, b] = PlainE $ Js.shiftLL a b
primOp ISrlOp     [a, b] = PlainE $ Js.shiftRL a b
primOp ISraOp     [a, b] = PlainE $ Js.shiftRA a b
primOp Int2WordOp  [a] = PlainE $ a

-- word:
-- imlement word as a bit reinterpretation of int
primOp WordGtOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "gt" [a, b]
primOp WordGeOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "ge" [a, b]
primOp WordEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp WordNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp WordLtOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "lt" [a, b]
primOp WordLeOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "le" [a, b]

-- $hs.Int.addCarry(a, b, 0)[0]
primOp WordAddOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [a, b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp WordSubOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [a, Js.bitNot b, Js.int (1 :: Int)]
primOp WordMulOp [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "mul" [a, b]
primOp SllOp     [a, b] = PlainE $ Js.shiftLL a b
primOp SrlOp     [a, b] = PlainE $ Js.shiftRL a b
primOp AndOp     [a, b] = PlainE $ Js.bitAnd a b
primOp OrOp      [a, b] = PlainE $ Js.bitOr a b
primOp XorOp     [a, b] = PlainE $ Js.bitXOr a b
primOp NotOp     [a] = PlainE $ Js.bitNot a
primOp Word2IntOp[a] = PlainE $ a

primOp Narrow8IntOp [arg] = PlainE $
  Js.minus bits signBit
  where bits = Js.bitAnd arg (Js.int (0x7F :: Int))
        signBit = Js.bitAnd arg (Js.int (0x80 :: Int))
primOp Narrow16IntOp [arg] = PlainE $
  Js.minus bits signBit
  where bits = Js.bitAnd arg (Js.int (0x7FFF :: Int))
        signBit = Js.bitAnd arg (Js.int (0x8000 :: Int))
primOp Narrow32IntOp [a] = PlainE $ a
primOp Narrow8WordOp [arg] = PlainE $
  Js.minus bits signBit
  where bits = Js.bitAnd arg (Js.int (0x7F :: Int))
        signBit = Js.bitAnd arg (Js.int (0x80 :: Int))
primOp Narrow16WordOp [arg] = PlainE $
  Js.minus bits signBit
  where bits = Js.bitAnd arg (Js.int (0x7FFF :: Int))
        signBit = Js.bitAnd arg (Js.int (0x8000 :: Int))
primOp Narrow32WordOp [a] = PlainE $ a

primOp DataToTagOp [a] = PlainE $ RTS.conAppTag a

primOp IndexOffAddrOp_Char [a, b] = PlainE $ Js.nativeMethodCall a "charAt" [b]

-- StablePtr:
primOp MakeStablePtrOp [a, b] = PlainE $ Js.list [b, a]
primOp DeRefStablePtrOp [a, b] = PlainE $ Js.list [b, a]

primOp NewArrayOp    [n, a, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "newArray"    [n, a, s]
primOp SameMutableArrayOp [a, b]  = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "same"        [a, b]
primOp ReadArrayOp   [a, n, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "read"        [a, n, s]
primOp WriteArrayOp  [a, n, b, s] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "write"       [a, n, b, s]
--primOp SizeofArrayOp [a]          = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "sizeof"      [a]
--primOp SizeofMutableArrayOp [a]   = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "sizeofMut"   [a]
primOp IndexArrayOp  [a, n]       = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "index"       [a, n]
primOp UnsafeFreezeArrayOp [a, s] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "unsafeFreeze"[a, s]
primOp UnsafeThawArrayOp [a, s]   = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "unsafeThaw"  [a, s]

primOp NewMutVarOp   [a, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "newMutVar" [a, s]
primOp ReadMutVarOp  [a, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "read" [a, s]
primOp WriteMutVarOp [a, b, s] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "write" [a, b, s]
primOp SameMutVarOp  [a, b]    = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "same" [a, b]
primOp AtomicModifyMutVarOp  [a, b, s] = TrampolineM (Js.property RTS.root "MutVar") "atomicModify" [a, b, s]
#if __GLASGOW_HASKELL__ >= 702
primOp CasMutVarOp   [a, b, c, s] = TrampolineM (Js.property RTS.root "MutVar") "cas" [a, b, c, s]
#endif

primOp ForkOp [a, s]      = TrampolineM (Js.property RTS.root "Thread") "fork" [a, s]
primOp ForkOnOp [a, b, s] = TrampolineM (Js.property RTS.root "Thread") "forkOn" [a, b, s]
primOp YieldOp [s]        = TrampolineM (Js.property RTS.root "Thread") "yieldThread" [s]
primOp MyThreadIdOp [s]   = TrampolineM (Js.property RTS.root "Thread") "myThreadId" [s]
primOp NoDuplicateOp [s]  = TrampolineM (Js.property RTS.root "Thread") "noDuplicate" [s]
primOp DelayOp [a, s]     = TrampolineM (Js.property RTS.root "Thread") "delay" [a, s]

primOp CatchOp [a, b, s] = TrampolineM (Js.property RTS.root "Exception") "tryCatch" [a, b, s]
primOp RaiseOp [a]       = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Exception") "raise" [a]
primOp RaiseIOOp [a, s]  = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Exception") "raiseIO" [a, s]

primOp NewMVarOp     [s]       = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MVar") "newMVar" [s]
primOp TakeMVarOp    [a, s]    = TrampolineM (Js.property RTS.root "MVar") "take" [a, s]
primOp TryTakeMVarOp [a, s]    = TrampolineM (Js.property RTS.root "MVar") "tryTake" [a, s]
primOp PutMVarOp     [a, b, s] = TrampolineM (Js.property RTS.root "MVar") "put" [a, b, s]
primOp SameMVarOp    [a, b, s] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MVar") "same" [a, b, s]
primOp IsEmptyMVarOp [a, s]    = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MVar") "isEmpty" [a, s]

primOp op args = TrampolineM RTS.root (zEncodeString (show op))  args

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> Expression js -> Expression js -> Expression js
boolOp op a b = jsBoolToHs $ op a b
