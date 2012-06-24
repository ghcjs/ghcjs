{-# LANGUAGE CPP #-}
#include "MachDeps.h"
-- | Tries to implement GHC primitive operations as described at
--   http://www.haskell.org/ghc/docs/6.12.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html
-- Char# is Javascript string
-- Int# is Javascript number
-- Word# is Javascript number (positive)
-- Float# is Javascript number
-- Doable# is Javascript number
-- Addr# is not defined here
-- MutableByteArray# is not defined here
-- ByteArray# is not defined here
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
  | TrampolineM (Expression js) [Expression js]
  | UnknownOp

returnPrimitiveOperationResult :: Javascript js => PrimOp -> [StgArg] -> Gen js
returnPrimitiveOperationResult op args = do
  exp <- primOp' op args
  case exp of
    PlainE e           -> return $ Js.return e
    TrampolineM f args -> return $ jumpToFunction f args
    UnknownOp          -> return $ Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

declarePrimitiveOperationResult :: Javascript js => Stg.Id -> PrimOp -> [StgArg] -> js -> Expression js -> Gen js
declarePrimitiveOperationResult id op args rest live = do
  exp <- primOp' op args
  case exp of
     PlainE e           -> do
        i   <- stgIdToJsId id
        return $ mconcat [Js.declare [(i, e)], rest]
     TrampolineM f args -> do
        i    <- stgIdToJsId id
        return $ declareFunctionCallResult i f args rest live
     UnknownOp          -> return $ Js.throw . Js.string . concat $ ["primitive operation ", show op, ". Not implemeted yet."]

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

#if WORD_SIZE_IN_BITS == 32
-- int:
primOp IntGtOp  [a, b] = PlainE $ boolOp Js.greater a b
primOp IntGeOp  [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp IntEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp IntNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp IntLtOp  [a, b] = PlainE $ boolOp Js.less  a b
primOp IntLeOp  [a, b] = PlainE $ boolOp Js.lessOrEqual a b

primOp IntAddOp [a, b] = PlainE $ Js.bitOr (Js.plus a b) (Js.int (0 :: Int))
primOp IntSubOp [a, b] = PlainE $ Js.bitOr (Js.minus a b) (Js.int (0 :: Int))
-- This could overflow the 53 bits
-- primOp IntMulOp [a, b] = PlainE $ Js.bitOr (Js.multiply a b) (Js.int (0 :: Int))

primOp IntNegOp [a]    = PlainE $ Js.unaryMinus a

-- (a / b) |0
primOp IntQuotOp [a, b] = PlainE $ Js.bitOr (Js.divide a b) (Js.int (0 :: Int))
primOp IntRemOp  [a, b] = PlainE $ Js.mod a b
primOp ISllOp     [a, b] = PlainE $ Js.shiftLL a b
primOp ISrlOp     [a, b] = PlainE $ Js.shiftRL a b
primOp ISraOp     [a, b] = PlainE $ Js.shiftRA a b
primOp Int2WordOp  [a] = PlainE $ Js.shiftRL a (Js.int (0 :: Int))
primOp Int2FloatOp  [a] = PlainE $ a
primOp Int2DoubleOp  [a] = PlainE $ a

-- word:
primOp WordAddOp  [a, b] = PlainE $ Js.shiftRL (Js.plus a b) (Js.int (0 :: Int))
primOp WordSubOp  [a, b] = PlainE $ Js.shiftRL (Js.minus a b) (Js.int (0 :: Int))
primOp WordMulOp  [a, b] = PlainE $ Js.shiftRL (Js.multiply a b) (Js.int (0 :: Int))
primOp WordQuotOp [a, b] = PlainE $ Js.shiftRL (Js.divide a b) (Js.int (0 :: Int))
primOp WordRemOp  [a, b] = PlainE $ Js.mod a b

-- imlement word as a bit reinterpretation of int
primOp SllOp     [a, b] = PlainE $ Js.shiftLL a b
primOp SrlOp     [a, b] = PlainE $ Js.shiftRL a b
primOp AndOp     [a, b] = PlainE $ Js.bitAnd a b
primOp OrOp      [a, b] = PlainE $ Js.bitOr a b
primOp XorOp     [a, b] = PlainE $ Js.bitXOr a b
primOp NotOp     [a] = PlainE $ Js.bitNot a

primOp Word2IntOp[a] = PlainE $ Js.bitOr a (Js.int (0 :: Int))

primOp WordGtOp  [a, b] = PlainE $ boolOp Js.greater a b
primOp WordGeOp  [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp WordEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp WordNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp WordLtOp  [a, b] = PlainE $ boolOp Js.less  a b
primOp WordLeOp  [a, b] = PlainE $ boolOp Js.lessOrEqual a b

primOp Narrow8IntOp [arg] = PlainE $
  Js.minus bits signBit
  where bits = Js.bitAnd arg (Js.int (0x7F :: Int))
        signBit = Js.bitAnd arg (Js.int (0x80 :: Int))
primOp Narrow16IntOp [arg] = PlainE $
  Js.minus bits signBit
  where bits = Js.bitAnd arg (Js.int (0x7FFF :: Int))
        signBit = Js.bitAnd arg (Js.int (0x8000 :: Int))
primOp Narrow32IntOp [a] = PlainE $ a
primOp Narrow8WordOp [a] = PlainE $ Js.bitAnd a (Js.int (0xFF :: Int))
primOp Narrow16WordOp [a] = PlainE $ Js.bitAnd a (Js.int (0xFFFF :: Int))
primOp Narrow32WordOp [a] = PlainE $ a
#endif

#if WORD_SIZE_IN_BITS == 64
-- Only inlining methods that are the same for goog.math.Long
-- and goog.math.Integer.  That way we can easily switch
-- as between them for the 64bit Int and Word as it is not clear
-- which will be a better fit.

-- int:
primOp IntGtOp  [a, b] = compareMethod a "greaterThan" b
primOp IntGeOp  [a, b] = compareMethod a "greaterThanOrEqual" b
primOp IntEqOp  [a, b] = compareMethod a "equals" b
primOp IntNeOp  [a, b] = compareMethod a "notEquals" b
primOp IntLtOp  [a, b] = compareMethod a "lessThan" b
primOp IntLeOp  [a, b] = compareMethod a "lessThanOrEqual" b

primOp IntAddOp [a, b] = PlainE $ Js.nativeMethodCall a "add" [b]
primOp IntSubOp [a, b] = PlainE $ Js.nativeMethodCall a "subtract" [b]
primOp IntMulOp [a, b] = PlainE $ Js.nativeMethodCall a "multiply" [b]

primOp IntNegOp [a]    = PlainE $ Js.nativeMethodCall a "negate" []

-- word:
primOp WordAddOp  [a, b] = PlainE $ Js.nativeMethodCall a "add" [b]
primOp WordSubOp  [a, b] = PlainE $ Js.nativeMethodCall a "subtract" [b]
primOp WordMulOp  [a, b] = PlainE $ Js.nativeMethodCall a "multiply" [b]

-- imlement word as a bit reinterpretation of int
primOp AndOp     [a, b] = PlainE $ Js.nativeMethodCall a "and" [b]
primOp OrOp      [a, b] = PlainE $ Js.nativeMethodCall a "or" [b]
primOp XorOp     [a, b] = PlainE $ Js.nativeMethodCall a "xor" [b]
primOp NotOp     [a] = PlainE $ Js.nativeMethodCall a "not" []
#endif

-- double:
primOp DoubleGtOp  [a, b] = PlainE $ boolOp Js.greater a b
primOp DoubleGeOp  [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp DoubleEqOp  [a, b] = PlainE $ boolOp Js.equal a b
primOp DoubleNeOp  [a, b] = PlainE $ boolOp Js.notEqual a b
primOp DoubleLtOp  [a, b] = PlainE $ boolOp Js.less  a b
primOp DoubleLeOp  [a, b] = PlainE $ boolOp Js.lessOrEqual a b

primOp DoubleAddOp [a, b] = PlainE $ Js.plus a b
primOp DoubleSubOp [a, b] = PlainE $ Js.minus a b
primOp DoubleMulOp [a, b] = PlainE $ Js.multiply a b
primOp DoubleDivOp [a, b] = PlainE $ Js.divide a b

primOp DoubleNegOp [a]    = PlainE $ Js.unaryMinus a

primOp Double2IntOp [a]   = PlainE $ Js.bitOr a (Js.int (0 :: Int))

primOp Double2FloatOp [a] = PlainE $ a

-- float:
primOp FloatGtOp   [a, b] = PlainE $ boolOp Js.greater a b
primOp FloatGeOp   [a, b] = PlainE $ boolOp Js.greaterOrEqual a b
primOp FloatEqOp   [a, b] = PlainE $ boolOp Js.equal a b
primOp FloatNeOp   [a, b] = PlainE $ boolOp Js.notEqual a b
primOp FloatLtOp   [a, b] = PlainE $ boolOp Js.less  a b
primOp FloatLeOp   [a, b] = PlainE $ boolOp Js.lessOrEqual a b

primOp FloatAddOp  [a, b] = PlainE $ Js.plus a b
primOp FloatSubOp  [a, b] = PlainE $ Js.minus a b
primOp FloatMulOp  [a, b] = PlainE $ Js.multiply a b
primOp FloatDivOp  [a, b] = PlainE $ Js.divide a b

primOp FloatNegOp  [a]    = PlainE $ Js.unaryMinus a

primOp Float2IntOp [a]    = PlainE $ Js.bitOr a (Js.int (0 :: Int))

primOp Float2DoubleOp [a] = PlainE $ a

primOp DataToTagOp [a] = PlainE $ RTS.conAppTag a

-- StablePtr:
primOp MakeStablePtrOp [a, s] = PlainE $ Js.list [s, a]
primOp DeRefStablePtrOp [a, s] = PlainE $ Js.list [s, a]

-- We can't inline these
primOp op@YieldOp args                 = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@AtomicModifyMutVarOp args    = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@TakeMVarOp args              = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@TryTakeMVarOp args           = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@PutMVarOp args               = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@CatchOp args                 = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@MaskAsyncExceptionsOp args   = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@UnmaskAsyncExceptionsOp args = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@WaitReadOp args              = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@WaitWriteOp args             = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args
primOp op@SeqOp args             = TrampolineM (Js.var ("$hs_" ++ zEncodeString (show op))) args

primOp op args = PlainE $ Js.nativeFunctionCall (Js.var ("$hs_" ++ zEncodeString (show op))) args

boolOp :: Javascript js => (Expression js -> Expression js -> Expression js) -> Expression js -> Expression js -> Expression js
boolOp op a b = jsBoolToHs $ op a b

#if WORD_SIZE_IN_BITS == 64
compareMethod a m b = PlainE $ jsBoolToHs $ Js.nativeMethodCall a m [b]
#endif
