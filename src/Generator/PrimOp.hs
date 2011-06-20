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
import Data.Monoid (Monoid(..))
import Data.Char (isAlphaNum)
import Encoding (zEncodeString)

data Javascript js => PrimOpExp js =
    PlainE (Expression js) | TrampolineM (Expression js) Js.Id [Expression js] | UnknownOp

returnPrimitiveOperationResult :: Javascript js => PrimOp -> [StgArg] -> js
returnPrimitiveOperationResult op args =
  case primOp op args
  of PlainE e             -> Js.return e
     TrampolineM o m args -> jumpToMethod o m args
     UnknownOp            -> Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

declarePrimitiveOperationResult :: Javascript js => Stg.Id -> PrimOp -> [StgArg] -> js -> js
declarePrimitiveOperationResult id op args rest =
  case primOp op args
  of PlainE e             -> mconcat [Js.declare (stgIdToJsId id) e, rest]
     TrampolineM o m args -> declareMethodCallResult (stgIdToJsId id) o m args rest
     UnknownOp            -> Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

primOp :: Javascript js => PrimOp -> [StgArg] -> PrimOpExp js
-- char:
primOp CharGtOp [a, b] = PlainE $ boolOp Js.greater a b
primOp CharGeOp [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp CharEqOp [a, b] = PlainE $ boolOp Js.equal a b
primOp CharNeOp [a, b] = PlainE $ boolOp Js.notEqual a b
primOp CharLtOp [a, b] = PlainE $ boolOp Js.less  a b
primOp CharLeOp [a, b] = PlainE $ boolOp Js.lessOrEqual a b
primOp OrdOp    [a]    = PlainE $ Js.nativeMethodCall (stgArgToJs a) "charCodeAt" [Js.int (0 :: Int)]
primOp ChrOp    [a]    = PlainE $ Js.nativeMethodCall (Js.var "String") "fromCharCode" [stgArgToJs a]

-- int:
primOp IntGtOp  [a, b] = PlainE $ boolOp Js.greater a b
primOp IntGeOp  [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp IntEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp IntNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp IntLtOp  [a, b] = PlainE $ boolOp Js.less  a b
primOp IntLeOp  [a, b] = PlainE $ boolOp Js.lessOrEqual a b

-- $hs.Int.addCarry(a, b, 0)[0]
primOp IntAddOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp IntSubOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]
primOp IntMulOp [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp IntNegOp [a]    = PlainE $ Js.unaryMinus (stgArgToJs a)

-- overflow sensitive operations:
-- (a >>> 16) == 0 && (b >>> 16) == 0
primOp IntMulMayOfloOp [a, b] = PlainE $ Js.and (test a) (test b)
  where zero = Js.int (0 :: Int)
        sixteen = Js.int (16 :: Int)
        test x = Js.equal (Js.shiftRA (stgArgToJs x) sixteen) zero

-- $hs.Int.addCarry(a, b, 0)
primOp IntAddCOp       [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)
primOp IntSubCOp       [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]

-- (a / b) & ~0
primOp IntQuotOp [a, b] = PlainE $ Js.bitAnd (Js.divide (stgArgToJs a) (stgArgToJs b)) (Js.bitNot $ Js.int (0 :: Int))
primOp IntRemOp  [a, b] = PlainE $ Js.mod (stgArgToJs a) (stgArgToJs b)
primOp ISllOp     [a, b] = PlainE $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp ISrlOp     [a, b] = PlainE $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp ISraOp     [a, b] = PlainE $ Js.shiftRA (stgArgToJs a) (stgArgToJs b)
primOp Int2WordOp  [a] = PlainE $ stgArgToJs a

-- word:
-- imlement word as a bit reinterpretation of int
primOp WordGtOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "gt" [stgArgToJs a, stgArgToJs b]
primOp WordGeOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "ge" [stgArgToJs a, stgArgToJs b]
primOp WordEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp WordNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp WordLtOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "lt" [stgArgToJs a, stgArgToJs b]
primOp WordLeOp  [a, b] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "Word") "le" [stgArgToJs a, stgArgToJs b]

-- $hs.Int.addCarry(a, b, 0)[0]
primOp WordAddOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, stgArgToJs b, Js.int (0 :: Int)]

-- $hs.Int.addCarry(a, ~b, 1)[0]
primOp WordSubOp [a, b] = PlainE $ flip Js.subscript (Js.int (0 :: Int)) $ Js.nativeMethodCall (Js.property RTS.root "Int") "addCarry" [stgArgToJs a, Js.bitNot (stgArgToJs b), Js.int (1 :: Int)]
primOp WordMulOp [a, b] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Int") "mul" [stgArgToJs a, stgArgToJs b]
primOp SllOp     [a, b] = PlainE $ Js.shiftLL (stgArgToJs a) (stgArgToJs b)
primOp SrlOp     [a, b] = PlainE $ Js.shiftRL (stgArgToJs a) (stgArgToJs b)
primOp AndOp     [a, b] = PlainE $ Js.bitAnd (stgArgToJs a) (stgArgToJs b)
primOp OrOp      [a, b] = PlainE $ Js.bitOr (stgArgToJs a) (stgArgToJs b)
primOp XorOp     [a, b] = PlainE $ Js.bitXOr (stgArgToJs a) (stgArgToJs b)
primOp NotOp     [a] = PlainE $ Js.bitNot (stgArgToJs a)
primOp Word2IntOp[a] = PlainE $ stgArgToJs a

primOp Narrow8IntOp [a] = PlainE $
  Js.ternary (Js.greaterOrEqual arg zero)
    (Js.bitAnd arg bitMask7)
    (inv (inv arg `Js.bitAnd` bitMask7))
  where arg = stgArgToJs a
        inv f = Js.bitXOr f (Js.bitNot zero)
        bitMask7 = Js.int (127 :: Int)
        zero = Js.int (0 :: Int)
primOp Narrow16IntOp [a] = PlainE $
  Js.ternary (Js.greaterOrEqual arg zero)
    (Js.bitAnd arg bitMask15)
    (inv (inv arg `Js.bitAnd` bitMask15))
  where arg = stgArgToJs a
        inv f = Js.bitXOr f (Js.bitNot zero)
        bitMask15 = Js.int (32767 :: Int)
        zero = Js.int (0 :: Int)
primOp Narrow32IntOp [a] = PlainE $ stgArgToJs a
primOp Narrow8WordOp [a] = PlainE $ Js.bitAnd (stgArgToJs a) (Js.int (0xFF :: Int))
primOp Narrow16WordOp [a] = PlainE $ Js.bitAnd (stgArgToJs a) (Js.int (0xFFFF :: Int))
primOp Narrow32WordOp [a] = PlainE $ stgArgToJs a

primOp DataToTagOp [a] = PlainE $ RTS.conAppTag (stgArgToJs a)

primOp IndexOffAddrOp_Char [a, b] = PlainE $ Js.nativeMethodCall (stgArgToJs a) "charAt" [stgArgToJs b]

primOp NewArrayOp    [n, a, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "newArray"    [stgArgToJs n, stgArgToJs a, stgArgToJs s]
primOp SameMutableArrayOp [a, b]  = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "same"        [stgArgToJs a, stgArgToJs b]
primOp ReadArrayOp   [a, n, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "read"        [stgArgToJs a, stgArgToJs n, stgArgToJs s]
primOp WriteArrayOp  [a, n, b, s] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "write"       [stgArgToJs a, stgArgToJs n, stgArgToJs b, stgArgToJs s]
--primOp SizeofArrayOp [a]          = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "sizeof"      [stgArgToJs a]
--primOp SizeofMutableArrayOp [a]   = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "sizeofMut"   [stgArgToJs a]
primOp IndexArrayOp  [a, n]       = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "index"       [stgArgToJs a, stgArgToJs n]
primOp UnsafeFreezeArrayOp [a, s] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "unsafeFreeze"[stgArgToJs a, stgArgToJs s]
primOp UnsafeThawArrayOp [a, s]   = PlainE $ Js.nativeMethodCall (Js.property RTS.root "_Array") "unsafeThaw"  [stgArgToJs a, stgArgToJs s]

primOp NewMutVarOp   [a, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "newMutVar" [stgArgToJs a, stgArgToJs s]
primOp ReadMutVarOp  [a, s]    = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "read" [stgArgToJs a, stgArgToJs s]
primOp WriteMutVarOp [a, b, s] = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "write" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp SameMutVarOp  [a, b]    = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MutVar") "same" [stgArgToJs a, stgArgToJs b]
primOp AtomicModifyMutVarOp  [a, b, s] = TrampolineM (Js.property RTS.root "MutVar") "atomicModify" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp CasMutVarOp   [a, b, c, s] = TrampolineM (Js.property RTS.root "MutVar") "cas" [stgArgToJs a, stgArgToJs b, stgArgToJs c, stgArgToJs s]

primOp ForkOp [a, s]      = TrampolineM (Js.property RTS.root "Thread") "fork" [stgArgToJs a, stgArgToJs s]
primOp ForkOnOp [a, b, s] = TrampolineM (Js.property RTS.root "Thread") "forkOn" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp YieldOp [s]        = TrampolineM (Js.property RTS.root "Thread") "yieldThread" [stgArgToJs s]
primOp MyThreadIdOp [s]   = TrampolineM (Js.property RTS.root "Thread") "myThreadId" [stgArgToJs s]
primOp NoDuplicateOp [s]  = TrampolineM (Js.property RTS.root "Thread") "noDuplicate" [stgArgToJs s]
primOp DelayOp [a, s]     = TrampolineM (Js.property RTS.root "Thread") "delay" [stgArgToJs a, stgArgToJs s]

primOp CatchOp [a, b, s] = TrampolineM (Js.property RTS.root "Exception") "tryCatch" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp RaiseOp [a]       = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Exception") "raise" [stgArgToJs a]
primOp RaiseIOOp [a, s]  = PlainE $ Js.nativeMethodCall (Js.property RTS.root "Exception") "raiseIO" [stgArgToJs a, stgArgToJs s]

primOp NewMVarOp     [s]       = PlainE $ Js.nativeMethodCall (Js.property RTS.root "MVar") "newMVar" [stgArgToJs s]
primOp TakeMVarOp    [a, s]    = TrampolineM (Js.property RTS.root "MVar") "take" [stgArgToJs a, stgArgToJs s]
primOp TryTakeMVarOp [a, s]    = TrampolineM (Js.property RTS.root "MVar") "tryTake" [stgArgToJs a, stgArgToJs s]
primOp PutMVarOp     [a, b, s] = TrampolineM (Js.property RTS.root "MVar") "put" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp SameMVarOp    [a, b, s] = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MVar") "same" [stgArgToJs a, stgArgToJs b, stgArgToJs s]
primOp IsEmptyMVarOp [a, s]    = PlainE $ jsBoolToHs $ Js.nativeMethodCall (Js.property RTS.root "MVar") "isEmpty" [stgArgToJs a, stgArgToJs s]

primOp op args = TrampolineM RTS.root (zEncodeString (show op)) $ map stgArgToJs args

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> StgArg -> StgArg -> Expression js
boolOp op a b = jsBoolToHs $ op (stgArgToJs a) (stgArgToJs b)
