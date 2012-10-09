{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Gen2.Prim where

import Gen2.Utils
import Gen2.StgAst
import Language.Javascript.JMacro
import Language.Javascript.JMacro.Types

import PrimOp
import Panic

data PrimRes = PrimInline  JStat -- arguments used once
             | PRPrimCall  JStat

-- char = javascript number    'c' (fixme change to number?)
-- op, results, arguments, result
genPrim :: PrimOp -> [JExpr] -> [JExpr] -> PrimRes
genPrim CharGtOp          [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0; |]
genPrim CharGeOp          [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0; |]
genPrim CharEqOp          [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0; |]
genPrim CharNeOp          [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0; |]
genPrim CharLtOp          [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0; |]
genPrim CharLeOp          [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0; |]
genPrim OrdOp             [r] [x]   = PrimInline [j| `r` = `x` |]

-- int = javascript number (fixme 64 bit?)
genPrim IntAddOp          [r] [x,y] = PrimInline [j| `r` = `x` + `y` |]
genPrim IntSubOp          [r] [x,y] = PrimInline [j| `r` = (`x` - `y`)|0 |]
genPrim IntMulOp          [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]
genPrim IntMulMayOfloOp   [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]  -- fixme loss of precision
genPrim IntQuotOp         [r] [x,y] = PrimInline [j| `r` = (`x`/`y`)|0 |] -- fixme |0
genPrim IntRemOp          [r] [x,y] = PrimInline [j| `r` = `x` % `y` |]
-- genPrim IntQuotRemOp  [r1,r2] [x,y] = PrimInline [j| `r1` = (`x`/`y`); `r` = (`x`%`y`); |]
genPrim IntNegOp          [r] [x]   = PrimInline [j| `r` = 0 - `x` |] -- fixme?
genPrim IntAddCOp         [r] [x,c] = PrimInline [j| `r` = `x`+`c` |]
genPrim IntSubCOp         [r] [x,c] = PrimInline [j| `r` = `x`-`c` |]
genPrim IntGtOp           [r] [x,y] = PrimInline [j| `r` = (`x` > `y`)|0 |]
genPrim IntGeOp           [r] [x,y] = PrimInline [j| `r`= (`x` >= `y`) ? 1 : 0 |]
genPrim IntEqOp           [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim IntNeOp           [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim IntLtOp           [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim IntLeOp           [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
genPrim ChrOp             [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim Int2WordOp        [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim Int2FloatOp       [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim Int2DoubleOp      [r] [x]   = PrimInline [j| `r` = `x` |]
-- genPrim ISllOp              [x,y] = PrimInline [je| `x` << `y` |]
-- genPrim ISraOp              [x,y] = PrimInline [je| `x` >> `y` |]  -- fixme check: arith shift r
-- genPrim ISrlOp              [x,y] = PrimInline [je| `x` >>> `y` |] -- fixme check: logical shift r
genPrim WordAddOp         [r] [x,y] = PrimInline [j| `r` = `x` + `y` |]
-- genPrim WordAdd2Op          [x,y] = PrimInline [je| `x`+2 |] -- ?
genPrim WordSubOp         [r] [x,y] = PrimInline [j| `r` = `x` - `y` |]
genPrim WordMulOp         [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]
--genPrim WordMul2Op          [x]   = PrimInline [je| `x` * 2 |]
-- genPrim WordQuotOp          [x,y] = PrimInline [je| (`x`/`y`)|0 |]
genPrim WordRemOp         [r] [x,y] = PrimInline [j| `r`= `x` % `y` |]
-- genPrim WordQuotRemOp    [x,y] = PrimInline [je|
{-
genPrim AndOp               [x,y] = PrimInline [je| `x` & `y` |]
genPrim OrOp                [x,y] = PrimInline [je| `x` | `y` |]
genPrim XorOp               [x,y] = PrimInline [je| `x` ^ `y` |]
genPrim NotOp               [x]   = PrimInline [je| ~`x` |]
genPrim SllOp               [x,y] = PrimInline [je| `x` << `y` |]
genPrim SrlOp               [x,y] = PrimInline [je| `x` >> `y` |]
-}
genPrim Word2IntOp        [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim WordGtOp          [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0 |]
genPrim WordGeOp          [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0 |]
genPrim WordEqOp          [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim WordNeOp          [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim WordLtOp          [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim WordLeOp          [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
{-
genPrim PopCnt8Op           [x]   = PrimInline [je| popCntTab[`x` & 0xFF] |]
genPrim PopCnt16Op          [x]   = PrimInlineN  [je| popCntTab[`x`&0xFF] + popCntTab[(`x`>>>8)&0xFF] |]
genPrim PopCnt32Op          [x]   = PrimInlineN [je| popCntTab[`x`&0xFF] + popCntTab[(`x`>>>8)&0xFF] + popCntTab[(`x`>>>16)&0xFF] + popCntTab[(`x`>>>24)&0xFF] |]
-- genPrim PopCnt64Op       [x]   = PrimInline [je|      |]
genPrim PopCntOp            [x]   = PrimInlineN [je|   popCntTab[`x`&0xFF] + popCntTab[(`x`>>>8)&0xFF] + popCntTab[(`x`>>>16)&0xFF] + popCntTab[(`x`>>>24)&0xFF] |]
genPrim Narrow8IntOp        [x]   = PrimInlineN [je| (`x`|0x7F)-(`x`|0x80) |]
genPrim Narrow16IntOp       [x]   = PrimInlineN [je| (`x`|0x7FFF)-(`x`|0x8000) |]
genPrim Narrow32IntOp       [x]   = PrimInline [je| `x` |]
genPrim Narrow8WordOp       [x]   = PrimInline [je| `x` & 0xFF |]
genPrim Narrow16WordOp      [x]   = PrimInline [je| `x` & 0xFFFF |]
genPrim Narrow32WordOp      [x]   = PrimInline [je| `x` |]
-}
genPrim DoubleGtOp        [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0 |]
genPrim DoubleGeOp        [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0 |]
genPrim DoubleEqOp        [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim DoubleNeOp        [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim DoubleLtOp        [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim DoubleLeOp        [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
genPrim DoubleAddOp       [r] [x,y] = PrimInline [j| `r` = `x` + `y` |]
genPrim DoubleSubOp       [r] [x,y] = PrimInline [j| `r` = `x` - `y` |]
genPrim DoubleMulOp       [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]
genPrim DoubleDivOp       [r] [x,y] = PrimInline [j| `r` = `x` / `y` |]
genPrim DoubleNegOp       [r] [x]   = PrimInline [j| `r` = `jneg x` |] -- fixme negate
-- genPrim Double2IntOp        [x]   = PrimInline [je| `x`|0 |]
genPrim Double2FloatOp    [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim DoubleExpOp       [r] [x]   = PrimInline [j| `r` = Math.exp(`x`) |]
genPrim DoubleLogOp       [r] [x]   = PrimInline [j| `r` = Math.ln(`x`) |]
genPrim DoubleSqrtOp      [r] [x]   = PrimInline [j| `r` = Math.sqrt(`x`) |]
genPrim DoubleSinOp       [r] [x]   = PrimInline [j| `r` = Math.sin(`x`) |]
genPrim DoubleCosOp       [r] [x]   = PrimInline [j| `r` = Math.cos(`x`) |]
genPrim DoubleTanOp       [r] [x]   = PrimInline [j| `r` = Math.tan(`x`) |]
genPrim DoubleAsinOp      [r] [x]   = PrimInline [j| `r` = Math.asin(`x`) |]
genPrim DoubleAcosOp      [r] [x]   = PrimInline [j| `r` = Math.acos(`x`) |]
genPrim DoubleAtanOp      [r] [x]   = PrimInline [j| `r` = Math.atan(`x`) |]
genPrim DoubleSinhOp      [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)-Math.exp(`jneg x`))/2 |]
genPrim DoubleCoshOp      [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)+Math.exp(`jneg x`))/2 |]
genPrim DoubleTanhOp      [r] [x]   = PrimInline [j| `r` = (Math.exp(2*`x`)-1)/(Math.exp(2*`x`)+1) |]
genPrim DoublePowerOp     [r] [x,y] = PrimInline [j| `r` = Math.pow(`x`,`y`) |]
-- genPrim DoubleDecode_2IntOp [x] =
genPrim FloatGtOp         [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0 |]
genPrim FloatGeOp         [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0 |]
genPrim FloatEqOp         [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim FloatNeOp         [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim FloatLtOp         [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim FloatLeOp         [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
genPrim FloatAddOp        [r] [x,y] = PrimInline [j| `r` = `x` + `y` |]
genPrim FloatSubOp        [r] [x,y] = PrimInline [j| `r` = `x` - `y` |]
genPrim FloatMulOp        [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]
genPrim FloatDivOp        [r] [x,y] = PrimInline [j| `r` = `x` / `y` |]
genPrim FloatNegOp        [r] [x,y] = PrimInline [j| `r` = `jneg x`  |]
-- genPrim Float2IntOp         [x]   = PrimInline [je| `x`|0 |]
genPrim FloatExpOp        [r] [x]   = PrimInline [j| `r` = Math.exp(`x`) |]
genPrim FloatLogOp        [r] [x]   = PrimInline [j| `r` = Math.ln(`x`) |]
genPrim FloatSqrtOp       [r] [x]   = PrimInline [j| `r` = Math.sqrt(`x`) |]
genPrim FloatSinOp        [r] [x]   = PrimInline [j| `r` = Math.sin(`x`) |]
genPrim FloatCosOp        [r] [x]   = PrimInline [j| `r` = Math.cos(`x`) |]
genPrim FloatTanOp        [r] [x]   = PrimInline [j| `r` = Math.tan(`x`) |]
genPrim FloatAsinOp       [r] [x]   = PrimInline [j| `r` = Math.asin(`x`) |]
genPrim FloatAcosOp       [r] [x]   = PrimInline [j| `r` = Math.acos(`x`) |]
genPrim FloatAtanOp       [r] [x]   = PrimInline [j| `r` = Math.atan(`x`) |]
genPrim FloatSinhOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)-Math.exp(`jneg x`))/2 |] -- fixme neg
genPrim FloatCoshOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)+Math.exp(`jneg x`))/2 |] -- fixme neg
genPrim FloatTanhOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(2*`x`)-1)/(Math.exp(2*`x`)+1) |]
genPrim FloatPowerOp      [r] [x,y] = PrimInline [j| `r` = Math.pow(`x`,`y`) |]
genPrim Float2DoubleOp    [r] [x]   = PrimInline [j| `r` = `x` |]
-- genPrim FloatDecode_IntOp
{-
genPrim NewArrayOp          [n,a] =
genPrim SameMutableArrayOp
genPrim ReadArrayOp
genPrim WriteArrayOp
genPrim SizeofArrayOp
genPrim SizeofMutableArrayOp
genPrim IndexArrayOp
genPrim UnsafeFreezeArrayOp
genPrim UnsafeThawArrayOp
genPrim CopyArrayOp
genPrim CopyMutableArrayOp
genPrim CloneArrayOp
genPrim CloneMutableArrayOp
genPrim FreezeArrayOp
genPrim ThawArrayOp
genPrim NewByteArrayOp_Char
genPrim NewPinnedByteArrayOp_Char
genPrim NewAlignedPinnedByteArrayOp_Char
genPrim ByteArrayContents_Char
genPrim SameMutableByteArrayOp
genPrim UnsafeFreezeByteArrayOp
genPrim SizeofByteArrayOp
genPrim SizeofMutableByteArrayOp
genPrim IndexByteArrayOp_Char
genPrim IndexByteArrayOp_WideChar
genPrim IndexByteArrayOp_Int
genPrim IndexByteArrayOp_Word
genPrim IndexByteArrayOp_Addr
genPrim IndexByteArrayOp_Float
genPrim IndexByteArrayOp_Double
genPrim IndexByteArrayOp_StablePtr
genPrim IndexByteArrayOp_Int8
genPrim IndexByteArrayOp_Int16
genPrim IndexByteArrayOp_Int32
genPrim IndexByteArrayOp_Int64
genPrim IndexByteArrayOp_Word8
genPrim IndexByteArrayOp_Word16
genPrim IndexByteArrayOp_Word32
genPrim IndexByteArrayOp_Word64
genPrim ReadByteArrayOp_Char
genPrim ReadByteArrayOp_WideChar
genPrim ReadByteArrayOp_Int
genPrim ReadByteArrayOp_Word
genPrim ReadByteArrayOp_Addr
genPrim ReadByteArrayOp_Float
genPrim ReadByteArrayOp_Double
genPrim ReadByteArrayOp_StablePtr
genPrim ReadByteArrayOp_Int8
genPrim ReadByteArrayOp_Int16
genPrim ReadByteArrayOp_Int32
genPrim ReadByteArrayOp_Int64
genPrim ReadByteArrayOp_Word8
genPrim ReadByteArrayOp_Word16
genPrim ReadByteArrayOp_Word32
genPrim ReadByteArrayOp_Word64
genPrim WriteByteArrayOp_Char
genPrim WriteByteArrayOp_WideChar
genPrim WriteByteArrayOp_Int
genPrim WriteByteArrayOp_Word
genPrim WriteByteArrayOp_Addr
genPrim WriteByteArrayOp_Float
genPrim WriteByteArrayOp_Double
genPrim WriteByteArrayOp_StablePtr
genPrim WriteByteArrayOp_Int8
genPrim WriteByteArrayOp_Int16
genPrim WriteByteArrayOp_Int32
genPrim WriteByteArrayOp_Int64
genPrim WriteByteArrayOp_Word8
genPrim WriteByteArrayOp_Word16
genPrim WriteByteArrayOp_Word32
genPrim WriteByteArrayOp_Word64
genPrim CopyByteArrayOp
genPrim CopyMutableByteArrayOp
genPrim NewArrayArrayOp
genPrim SameMutableArrayArrayOp
genPrim UnsafeFreezeArrayArrayOp
genPrim SizeofArrayArrayOp
genPrim SizeofMutableArrayArrayOp
genPrim IndexArrayArrayOp_ByteArray
genPrim IndexArrayArrayOp_ArrayArray
genPrim ReadArrayArrayOp_ByteArray
genPrim ReadArrayArrayOp_MutableByteArray
genPrim ReadArrayArrayOp_ArrayArray
genPrim ReadArrayArrayOp_MutableArrayArray
genPrim WriteArrayArrayOp_ByteArray
genPrim WriteArrayArrayOp_MutableByteArray
genPrim WriteArrayArrayOp_ArrayArray
genPrim WriteArrayArrayOp_MutableArrayArray
genPrim CopyArrayArrayOp
genPrim CopyMutableArrayArrayOp

-- addr: array, index?
genPrim AddrAddOp
genPrim AddrSubOp
genPrim AddrRemOp
genPrim Addr2IntOp
genPrim Int2AddrOp
genPrim AddrGtOp
genPrim AddrGeOp
genPrim AddrEqOp
genPrim AddrNeOp
genPrim AddrLtOp
genPrim AddrLeOp
-}

-- addr indexing: unboxed arrays
{-
IndexOffAddrOp_Char
IndexOffAddrOp_WideChar
IndexOffAddrOp_Int
IndexOffAddrOp_Word
IndexOffAddrOp_Addr
IndexOffAddrOp_Float
IndexOffAddrOp_Double
IndexOffAddrOp_StablePtr
IndexOffAddrOp_Int8
IndexOffAddrOp_Int16
IndexOffAddrOp_Int32
IndexOffAddrOp_Int64
IndexOffAddrOp_Word8
IndexOffAddrOp_Word16
IndexOffAddrOp_Word32
IndexOffAddrOp_Word64
ReadOffAddrOp_Char
ReadOffAddrOp_WideChar
ReadOffAddrOp_Int
ReadOffAddrOp_Word
ReadOffAddrOp_Addr
ReadOffAddrOp_Float
ReadOffAddrOp_Double
ReadOffAddrOp_StablePtr
ReadOffAddrOp_Int8
ReadOffAddrOp_Int16
ReadOffAddrOp_Int32
ReadOffAddrOp_Int64
ReadOffAddrOp_Word8
ReadOffAddrOp_Word16
ReadOffAddrOp_Word32
ReadOffAddrOp_Word64
WriteOffAddrOp_Char
WriteOffAddrOp_WideChar
WriteOffAddrOp_Int
WriteOffAddrOp_Word
WriteOffAddrOp_Addr
WriteOffAddrOp_Float
WriteOffAddrOp_Double
WriteOffAddrOp_StablePtr
WriteOffAddrOp_Int8
WriteOffAddrOp_Int16
WriteOffAddrOp_Int32
WriteOffAddrOp_Int64
WriteOffAddrOp_Word8
WriteOffAddrOp_Word16
WriteOffAddrOp_Word32
WriteOffAddrOp_Word64
NewMutVarOp
ReadMutVarOp
WriteMutVarOp
SameMutVarOp
AtomicModifyMutVarOp
CasMutVarOp
CatchOp
RaiseOp
RaiseIOOp
MaskAsyncExceptionsOp
MaskUninterruptibleOp
UnmaskAsyncExceptionsOp
MaskStatus
AtomicallyOp
RetryOp
CatchRetryOp
CatchSTMOp
Check
NewTVarOp
ReadTVarOp
ReadTVarIOOp
WriteTVarOp
SameTVarOp
NewMVarOp
TakeMVarOp
TryTakeMVarOp
PutMVarOp
TryPutMVarOp
SameMVarOp
IsEmptyMVarOp
DelayOp
WaitReadOp
WaitWriteOp
ForkOp
ForkOnOp
KillThreadOp
YieldOp
MyThreadIdOp
LabelThreadOp
IsCurrentThreadBoundOp
NoDuplicateOp
ThreadStatusOp
MkWeakOp
MkWeakForeignEnvOp
DeRefWeakOp
FinalizeWeakOp
TouchOp
MakeStablePtrOp
DeRefStablePtrOp
EqStablePtrOp
MakeStableNameOp
EqStableNameOp
StableNameToIntOp
ReallyUnsafePtrEqualityOp
ParOp
SparkOp
SeqOp
GetSparkOp
NumSparks
ParGlobalOp
ParLocalOp
ParAtOp
ParAtAbsOp
ParAtRelOp
ParAtForNowOp
DataToTagOp
TagToEnumOp
AddrToAnyOp
MkApUpd0_Op
NewBCOOp
UnpackClosureOp
GetApStackValOp
GetCCSOfOp
GetCurrentCCSOp
TraceEventOp
-}

genPrim op rs as = PrimInline [j| error_msg = `"unhandled primop:"++show op++" "++show (length rs, length as)`; |]
-- genPrim op _ = panic ("unhandled primop: " ++ show op)