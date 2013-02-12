{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}
module Gen2.Prim where

{-
  unboxed representations:
  Int#    -> number
  Double# -> number
  Float#  -> number
  Char#   -> number
  Word#   -> number
  Addr#   -> DataView / offset    [array/object, offset (number)]
    properties: region: number, unique increasing number for comparisons?
  MutVar# -> [one-element array]
  TVar#   -> [?]
  MVar#   -> [mvar object] /* contains list of waiting threads */
  Weak#   -> [weak object]
  ThreadId -> [number ?]
  State#  -> nothing
  StablePtr# -> [one-elem array]
  MutableArrayArray# -> DataView
  MutableByteArray#  -> DataView
  ByteArray#         -> DataView
  Array#             -> Int32Array
  
  Pointers to pointers use a special representation with the .arr property
-}

import           Gen2.RtsTypes
import           Gen2.StgAst
import           Gen2.Utils
import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Types

import           Data.Monoid

import           Panic
import           PrimOp

data PrimRes = PrimInline JStat  -- ^ primop is inline
             | PRPrimCall JStat  -- ^ primop is async call

genPrim :: PrimOp   -- ^ the primitive operation
        -> [JExpr]  -- ^ where to store the result
        -> [JExpr]  -- ^ arguments
        -> PrimRes
genPrim CharGtOp          [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0; |]
genPrim CharGeOp          [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0; |]
genPrim CharEqOp          [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0; |]
genPrim CharNeOp          [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0; |]
genPrim CharLtOp          [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0; |]
genPrim CharLeOp          [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0; |]
genPrim OrdOp             [r] [x]   = PrimInline [j| `r` = `x` |]

genPrim IntAddOp          [r] [x,y] = PrimInline [j| `r` = (`x` + `y`)|0 |]
genPrim IntSubOp          [r] [x,y] = PrimInline [j| `r` = (`x` - `y`)|0 |]
genPrim IntMulOp          [r] [x,y] =
    PrimInline [j| `r` = $hs_mulInt32(`x`,`y`); |]
genPrim IntMulMayOfloOp   [r] [x,y] =
    PrimInline [j| var tmp = (`x`*`y`); `r` = (tmp===(tmp|0))?0:1; |]
genPrim IntQuotOp         [r] [x,y] = PrimInline [j| `r` = (`x`/`y`)|0; |]
genPrim IntRemOp          [r] [x,y] = PrimInline [j| `r` = `x` % `y` |]
genPrim IntQuotRemOp    [q,r] [x,y] = PrimInline [j| `q` = (`x`/`y`)|0;
                                                     `r` = `x`-`y`*`q`;
                                                   |]
genPrim IntNegOp          [r] [x]   = PrimInline [j| `r` = `jneg x`; |]
-- add with carry: overflow == 0 iff no overflow
genPrim IntAddCOp         [r,overf] [x,y] =
  PrimInline [j| var rt = `x`+`y`; `r` = rt|0; `overf` = (`r`!=rt)?1:0; |]
genPrim IntSubCOp         [r,overf] [x,y] =
  PrimInline [j| var rt = `x`-`y`; `r` = rt|0; `overf` = (`r`!=rt)?1:0; |]
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
genPrim ISllOp            [r] [x,y] = PrimInline [j| `r` = `x` << `y` |]
genPrim ISraOp            [r] [x,y] = PrimInline [j| `r` = `x` >> `y` |]
genPrim ISrlOp            [r] [x,y] = PrimInline [j| `r` = `x` >>> `y` |]
genPrim WordAddOp         [r] [x,y] = PrimInline [j| `r` = (`x` + `y`)|0; |]
-- fixme this thing is wrong
genPrim WordAdd2Op      [h,l] [x,y] = PrimInline [j| `l` = (`x` + `y`)|0;
                                                     `h` = (`x` >>> 32) + (`y` >>> 32);
                                                   |]
genPrim WordSubOp         [r] [x,y] = PrimInline [j| `r` = (`x` - `y`)|0 |]
genPrim WordMulOp         [r] [x,y] =
  PrimInline [j| `r` = $hs_mulWord32(`x`,`y`); |]
genPrim WordMul2Op      [h,l] [x,y] =
  PrimInline [j| `h` = $hs_mul2Word32(`x`,`y`);
                 `l` = ret1;
               |]
genPrim WordQuotOp        [r] [x,y] = PrimInline [j| `r` = $hs_quotWord32(`x`,`y`); |]
genPrim WordRemOp         [r] [x,y] = PrimInline [j| `r`= $hs_remWord32(`x`,`y`); |]
genPrim WordQuotRemOp   [q,r] [x,y] = PrimInline [j| `q` = $hs_quotWord32(`x`,`y`);
                                                     `r` = $hs_remWord32(`x`, `y`);
                                                  |]
genPrim AndOp             [r] [x,y] = PrimInline [j| `r` = `x` & `y` |]
genPrim OrOp              [r] [x,y] = PrimInline [j| `r` = `x` | `y` |]
genPrim XorOp             [r] [x,y] = PrimInline [j| `r` = `x` ^ `y` |]
genPrim NotOp             [r] [x]   = PrimInline [j| `r` = ~`x` |]
genPrim SllOp             [r] [x,y] = PrimInline [j| `r` = `x` << `y` |]
genPrim SrlOp             [r] [x,y] = PrimInline [j| `r` = `x` >>> `y` |]
genPrim Word2IntOp        [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim WordGtOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) > (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) > (`y`&1))) ? 1 : 0 |]
genPrim WordGeOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) > (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) >= (`y`&1))) ? 1 : 0 |]
genPrim WordEqOp          [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim WordNeOp          [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim WordLtOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) < (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) < (`y`&1))) ? 1 : 0 |]
genPrim WordLeOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) < (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) <= (`y`&1))) ? 1 : 0 |]
#if __GLASGOW_HASKELL__ >= 707
genPrim Word2DoubleOp     [r] [x] = PrimInline [j| `r` = (`x` & 0x7FFFFFFF) + (`x` >>> 31) * 2147483648 |]
genPrim Word2FloatOp      [r] [x] = PrimInline [j| `r` = (`x` & 0x7FFFFFFF) + (`x` >>> 31) * 2147483648 |]
#endif
genPrim PopCnt8Op         [r] [x]   = PrimInline [j| `r` = $hs_popCntTab[`x` & 0xFF] |]
genPrim PopCnt16Op        [r] [x]   =
  PrimInline [j| `r` = $hs_popCntTab[`x`&0xFF] +
                       $hs_popCntTab[(`x`>>>8)&0xFF]
               |]
genPrim PopCnt32Op        [r] [x]   =
  PrimInline [j| `r` = $hs_popCntTab[`x`&0xFF] +
                       $hs_popCntTab[(`x`>>>8)&0xFF] +
                       $hs_popCntTab[(`x`>>>16)&0xFF] +
                       $hs_popCntTab[(`x`>>>24)&0xFF]
               |]
genPrim PopCnt64Op        [r] [x1,x2] =
  PrimInline [j| `r` = $hs_popCntTab[`x1`&0xFF] +
                       $hs_popCntTab[(`x1`>>>8)&0xFF] +
                       $hs_popCntTab[(`x1`>>>16)&0xFF] +
                       $hs_popCntTab[(`x1`>>>24)&0xFF] +
                       $hs_popCntTab[`x2`&0xFF] +
                       $hs_popCntTab[(`x2`>>>8)&0xFF] +
                       $hs_popCntTab[(`x2`>>>16)&0xFF] +
                       $hs_popCntTab[(`x2`>>>24)&0xFF]
               |]
genPrim PopCntOp          [r] [x]   = genPrim PopCnt32Op [r] [x]
genPrim Narrow8IntOp      [r] [x]   = PrimInline [j| `r` = (`x` & 0x7F)-(`x` & 0x80) |]
genPrim Narrow16IntOp     [r] [x]   = PrimInline [j| `r` = (`x` & 0x7FFF)-(`x` & 0x8000) |]
genPrim Narrow32IntOp     [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim Narrow8WordOp     [r] [x]   = PrimInline [j| `r` = (`x` & 0xFF) |]
genPrim Narrow16WordOp    [r] [x]   = PrimInline [j| `r` = (`x` & 0xFFFF) |]
genPrim Narrow32WordOp    [r] [x]   = PrimInline [j| `r` = `x` |]
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
genPrim Double2IntOp      [r] [x]   = PrimInline [j| `r` = `x`|0; |]
genPrim Double2FloatOp    [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim DoubleExpOp       [r] [x]   = PrimInline [j| `r` = Math.exp(`x`) |]
genPrim DoubleLogOp       [r] [x]   = PrimInline [j| `r` = Math.log(`x`) |]
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
genPrim DoubleDecode_2IntOp [s,h,l,e] [x] =
  PrimInline [j| `s` = $hs_decodeDouble2Int(`x`);
                 `h` = ret1;
                 `l` = ret2;
                 `e` = ret3;
               |]
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
genPrim FloatNegOp        [r] [x]   = PrimInline [j| `r` = `jneg x`  |]
genPrim Float2IntOp       [r] [x]   = PrimInline [j| `r` = `x`|0 |]
genPrim FloatExpOp        [r] [x]   = PrimInline [j| `r` = Math.exp(`x`) |]
genPrim FloatLogOp        [r] [x]   = PrimInline [j| `r` = Math.ln(`x`) |]
genPrim FloatSqrtOp       [r] [x]   = PrimInline [j| `r` = Math.sqrt(`x`) |]
genPrim FloatSinOp        [r] [x]   = PrimInline [j| `r` = Math.sin(`x`) |]
genPrim FloatCosOp        [r] [x]   = PrimInline [j| `r` = Math.cos(`x`) |]
genPrim FloatTanOp        [r] [x]   = PrimInline [j| `r` = Math.tan(`x`) |]
genPrim FloatAsinOp       [r] [x]   = PrimInline [j| `r` = Math.asin(`x`) |]
genPrim FloatAcosOp       [r] [x]   = PrimInline [j| `r` = Math.acos(`x`) |]
genPrim FloatAtanOp       [r] [x]   = PrimInline [j| `r` = Math.atan(`x`) |]
genPrim FloatSinhOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)-Math.exp(`jneg x`))/2 |]
genPrim FloatCoshOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)+Math.exp(`jneg x`))/2 |]
genPrim FloatTanhOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(2*`x`)-1)/(Math.exp(2*`x`)+1) |]
genPrim FloatPowerOp      [r] [x,y] = PrimInline [j| `r` = Math.pow(`x`,`y`) |]
genPrim Float2DoubleOp    [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim FloatDecode_IntOp [s,e] [x] = PrimInline [j| `s` = $hs_decodeFloatInt(`x`); `e` = ret1; |]
genPrim NewArrayOp          [r] [l,e]   =
  PrimInline [j| `newArray r l`;
                 for(var i=`l` - 1; i>=0;i--) {
                   `r`[i] = `e`;
                 }
               |]
genPrim SameMutableArrayOp  [r] [a1,a2] = PrimInline [j| `r` = (`a1` === `a2`) ? 1 : 0; |]
genPrim ReadArrayOp         [r] [a,i]   = PrimInline [j| `r` = `a`[`i`]; |]
-- FIXME: this, and also MVar/IORef needs to update the modified list for the GC!
genPrim WriteArrayOp        []  [a,i,v] = PrimInline [j| `a`[`i`] = `v`; |]
genPrim SizeofArrayOp       [r] [a]     = PrimInline [j| `r` = `a`.length; |]
genPrim SizeofMutableArrayOp [r] [a]    = PrimInline [j| `r` = `a`.length; |]
genPrim IndexArrayOp        [r] [a,i]   = PrimInline [j| `r` = `a`[`i`]; |]
genPrim UnsafeFreezeArrayOp [r] [a]     = PrimInline [j| `r` = `a`; |]
genPrim UnsafeThawArrayOp   [r] [a]     = PrimInline [j| `r` = `a`; |]
genPrim CopyArrayOp         [] [a,o1,ma,o2,n] =
  PrimInline [j| for(var i=`n` - 1;i >= 0;i--) {
                   `ma`[i+`o2`] = `a`[i+`o1`];
                 }
               |]
genPrim CopyMutableArrayOp  [] [a1,o1,a2,o2,n] = genPrim CopyArrayOp [] [a1,o1,a2,o2,n]
genPrim CloneArrayOp        [r] [a,start,n] =
  PrimInline [j| `newArray r n`;
                 for(var i=`n` - 1; i >= 0; i--) {
                   `r`[i] = `a`[i+`start`];
                 }
               |]
genPrim CloneMutableArrayOp [r] [a,start,n] = genPrim CloneArrayOp [r] [a,start,n]
genPrim FreezeArrayOp       [r] [a,start,n] =
  PrimInline [j| `r` = new DataView(`a`.buffer.slice(`start`, `start`+`n`)); |]
genPrim ThawArrayOp         [r] [a,start,n] =
  PrimInline [j| `r` = new DataView(`a`.buffer.slice(`start`, `start`+`n`)); |]
genPrim NewByteArrayOp_Char [r] [l] = PrimInline (newByteArray r l)
genPrim NewPinnedByteArrayOp_Char [r] [l] = PrimInline (newByteArray r l)
genPrim NewAlignedPinnedByteArrayOp_Char [r] [l,align] = PrimInline (newByteArray r l)
genPrim ByteArrayContents_Char [a,o] [b] = PrimInline [j| `a` = `b`; `o` = 0; |]
genPrim SameMutableByteArrayOp [r] [a,b] = PrimInline [j| `r` = (`a` === `b`) ? 1 : 0; |]
genPrim UnsafeFreezeByteArrayOp [a] [b] = PrimInline [j| `a` = `b`; |]
genPrim SizeofByteArrayOp [r] [a] = PrimInline [j| `r` = `a`.byteLength; |]
genPrim SizeofMutableByteArrayOp [r] [a] = PrimInline [j| `r` = `a`.byteLength; |]
genPrim IndexByteArrayOp_Char [r] [a,i] = PrimInline [j| `r` = `a`.getUint8(`i`); |]
genPrim IndexByteArrayOp_WideChar [r] [a,i] = PrimInline [j| `r` = `a`.getInt32(`i`<<2); |]
genPrim IndexByteArrayOp_Int [r] [a,i] = PrimInline [j| `r` = `a`.getInt32(`i`<<2); |]
genPrim IndexByteArrayOp_Word [r] [a,i] = PrimInline [j| `r` = `a`.getUint32(`i`<<2); |]
genPrim IndexByteArrayOp_Addr [r1,r2] [a,i] = PrimInline [j| if(`a`.arr && `a`.arr[`i`<<2]) {
                                                               `r1` = `a`.arr[`i`<<2][0];
                                                               `r2` = `a`.arr[`i`<<2][1];
                                                             } else {
                                                               `r1` = null;
                                                               `r2` = 0;
                                                             }
                                                           |]
genPrim IndexByteArrayOp_Float [r] [a,i] = PrimInline [j| `r` = `a`.getFloat32(`i`<<2); |]
genPrim IndexByteArrayOp_Double [r] [a,i] = PrimInline [j| `r` = `a`.getFloat64(`i`<<3); |]
-- genPrim IndexByteArrayOp_StablePtr
genPrim IndexByteArrayOp_Int8 [r] [a,i] = PrimInline [j| `r` = `a`.getInt8(`i`); |]
genPrim IndexByteArrayOp_Int16 [r] [a,i] = PrimInline [j| `r` = `a`.getInt16(`i`<<1); |]
genPrim IndexByteArrayOp_Int32 [r] [a,i] = PrimInline [j| `r` = `a`.getInt32(`i`<<2); |]
genPrim IndexByteArrayOp_Int64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.getInt32(`i`<<3);
                 `r2` = `a`.getInt32((`i`<<3)+4);
               |]
genPrim IndexByteArrayOp_Word8 [r] [a,i] = PrimInline [j| `r` = `a`.getUint8(`i`); |]
genPrim IndexByteArrayOp_Word16 [r] [a,i] = PrimInline [j| `r` = `a`.getUint16(`i`<<1); |]
genPrim IndexByteArrayOp_Word32 [r] [a,i] = PrimInline [j| `r` = `a`.getUint32(`i`<<2); |]
genPrim IndexByteArrayOp_Word64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.getUint32(`i`<<3);
                 `r2` = `a`.getUint32((`i`<<3)+4);
               |]
genPrim ReadByteArrayOp_Char [r] [a,i] = PrimInline [j| `r` = `a`.getUint8(`i`); |]
genPrim ReadByteArrayOp_WideChar [r] [a,i] = PrimInline [j| `r` = `a`.getUint32(`i`); |]
genPrim ReadByteArrayOp_Int [r] [a,i] = PrimInline [j| `r` = `a`.getInt32(`i`); |]
genPrim ReadByteArrayOp_Word [r] [a,i] = PrimInline [j| `r` = `a`.getUint32(`i`); |]
genPrim ReadByteArrayOp_Addr [r1,r2] [a,i] = PrimInline [j| if(`a`.arr && `a`.arr[`i`]) {
                                                              `r1` = `a`.arr[`i`][0];
                                                              `r2` = `a`.arr[`i`][1];
                                                            } else {
                                                              `r1` = null;
                                                              `r2` = 0;
                                                            }
                                                          |]
genPrim ReadByteArrayOp_Float [r] [a,i] = PrimInline [j| `r` = `a`.getFloat32(`i`); |]
genPrim ReadByteArrayOp_Double [r] [a,i] = PrimInline [j| `r` = `a`.getFloat64(`i`); |]
-- genPrim ReadByteArrayOp_StablePtr
genPrim ReadByteArrayOp_Int8 [r] [a,i] = PrimInline [j| `r` = `a`.getInt8(`i`); |]
genPrim ReadByteArrayOp_Int16 [r] [a,i] = PrimInline [j| `r` = `a`.getInt16(`i`); |]
genPrim ReadByteArrayOp_Int32 [r] [a,i] = PrimInline [j| `r` = `a`.getInt32(`i`); |]
genPrim ReadByteArrayOp_Int64 [r1,r2] [a,i] = -- fixme need uint here in one
  PrimInline [j| `r1` = `a`.getInt32(`i`);
                 `r2` = `a`.getInt32(`i`+4);
              |]
genPrim ReadByteArrayOp_Word8 [r] [a,i] = PrimInline [j| `r` = `a`.getUint8(`i`); |]
genPrim ReadByteArrayOp_Word16 [r] [a,i] = PrimInline [j| `r` = `a`.getUint16(`i`); |]
genPrim ReadByteArrayOp_Word32 [r] [a,i] = PrimInline [j| `r` = `a`.getUint32(`i`); |]
genPrim ReadByteArrayOp_Word64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.getUint32(`i`);
                 `r2` = `a`.getUint32(`i`+4);
               |]
genPrim WriteByteArrayOp_Char [] [a,i,e] = PrimInline [j| `a`.setUint8(`i`, `e`); |]
genPrim WriteByteArrayOp_WideChar [] [a,i,e] = PrimInline [j| `a`.setUint32(`i`<<2,`e`); |]
genPrim WriteByteArrayOp_Int [] [a,i,e] = PrimInline [j| `a`.setInt32(`i`<<2, `e`); |]
genPrim WriteByteArrayOp_Word [] [a,i,e] = PrimInline [j| `a`.setUint32(`i`<<2, `e`); |]
genPrim WriteByteArrayOp_Addr [] [a,i,e1,e2] = PrimInline [j| if(!`a`.arr) { `a`.arr = []; }
                                                              `a`.arr[`i`<<2] = [`e1`,`e2`];
                                                            |]
genPrim WriteByteArrayOp_Float [] [a,i,e] = PrimInline [j| `a`.setFloat32(`i`<<2, `e`); |]
genPrim WriteByteArrayOp_Double [] [a,i,e] = PrimInline [j| `a`.setFloat64(`i`<<3, `e`); |]
-- genPrim WriteByteArrayOp_StablePtr
genPrim WriteByteArrayOp_Int8 [] [a,i,e] = PrimInline [j| `a`.setInt8(`i`, `e`); |]
genPrim WriteByteArrayOp_Int16 [] [a,i,e]     = PrimInline [j| `a`.setInt16(`i`<<1, `e`); |]
genPrim WriteByteArrayOp_Int32 [] [a,i,e]     = PrimInline [j| `a`.setInt32(`i`<<2, `e`); |]
genPrim WriteByteArrayOp_Int64 [] [a,i,e1,e2] =
  PrimInline [j| `a`.setInt32(`i`<<3, `e1`);
                 `a`.setInt32(`i`<<3+4, `e2`);
               |]
genPrim WriteByteArrayOp_Word8 [] [a,i,e]     = PrimInline [j| `a`.setUint8(`i`, `e`); |]
genPrim WriteByteArrayOp_Word16 [] [a,i,e]     = PrimInline [j| `a`.setUint16(`i`<<1, `e`); |]
genPrim WriteByteArrayOp_Word32 [] [a,i,e]     = PrimInline [j| `a`.setUint32(`i`<<2, `e`); |]
genPrim WriteByteArrayOp_Word64 [] [a,i,e1,e2] =
  PrimInline [j| `a`.setUint32(`i`<<3, `e1`);
                 `a`.setUint32(`i`<<3+4, `e2`);
               |]
-- fixme we can do faster by copying 32 bit ints or doubles
genPrim CopyByteArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline [j| for(var i=`n` - 1; i >= 0; i--) {
                   `a2`.setUint8(i+`o2`, `a1`.getUint8(i+`o1`));
                 }
               |]
genPrim CopyMutableByteArrayOp [] xs@[a1,o1,a2,o2,n] = genPrim CopyByteArrayOp [] xs
#if __GLASGOW_HASKELL__ >= 707
genPrim SetByteArrayOp [] [a,o,n,v] =
  PrimInline [j| for(var i=0;i<`n`;i++) {
                   `a`.setUint8(`o`+i,`v`);
                 }
               |]
#endif
genPrim NewArrayArrayOp [r] [n] =
  PrimInline [j| `r` = []; for(var i=0;i<`n`;i++) { `r`[i] = `r`; } |]
genPrim SameMutableArrayArrayOp [r] [a1,a2] = PrimInline [j| `r` = (`a1` === `a2`) ? 1 : 0; |]
genPrim UnsafeFreezeArrayArrayOp [r] [a] = PrimInline [j| `r` = `a` |]
genPrim SizeofArrayArrayOp [r] [a] = PrimInline [j| `r` = `a`.length |]
genPrim SizeofMutableArrayArrayOp [r] [a] = PrimInline [j| `r` = `a`.length |]
genPrim IndexArrayArrayOp_ByteArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim IndexArrayArrayOp_ArrayArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim ReadArrayArrayOp_ByteArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim ReadArrayArrayOp_MutableByteArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim ReadArrayArrayOp_ArrayArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim ReadArrayArrayOp_MutableArrayArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim WriteArrayArrayOp_ByteArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim WriteArrayArrayOp_MutableByteArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim WriteArrayArrayOp_ArrayArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim WriteArrayArrayOp_MutableArrayArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim CopyArrayArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline [j| for(var i=0;i<`n`;i++) { `a2`[i+`o2`]=`a1`[i+`o1`]; } |]
genPrim CopyMutableArrayArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline [j| for(var i=0;i<`n`;i++) { `a2`[i+`o2`]=`a1`[i+`o1`]; } |]

-- fixme do we want global addr numbering? (third arg?)
genPrim AddrAddOp  [a',o'] [a,o,i]   = PrimInline [j| `a'` = `a`; `o'` = `o` + `i`;|]
genPrim AddrSubOp  [a',o'] [a,o,i]   = PrimInline [j| `a'` = `a`; `o'` = `o` - `i`;|]
genPrim AddrRemOp  [a',o'] [a,o,i]   = PrimInline [j| `a'` = `a`; `o'` = `o` - `i`;|]
genPrim Addr2IntOp [i]     [a,o]     = PrimInline [j| `i` = `o`; |] -- fimxe need global ints?
genPrim Int2AddrOp [a,o]   [i]       = PrimInline [j| `a` = []; `o` = `i`; |] -- fixme
genPrim AddrGtOp   [r] [a1,o1,a2,o2] = PrimInline [j| `r` = (`o1` >  `o2`) ? 1 : 0; |]
genPrim AddrGeOp   [r] [a1,o1,a2,o2] = PrimInline [j| `r` = (`o1` >= `o2`) ? 1 : 0; |]
genPrim AddrEqOp   [r] [a1,o1,a2,o2] = PrimInline [j| `r` = (`a1` === `a2` && `o1` === `o2`) ? 1 : 0; |]
genPrim AddrNeOp   [r] [a1,o1,a2,o2] = PrimInline [j| `r` = (`a1` === `a2` && `o1` === `o2`) ? 0 : 1; |]
genPrim AddrLtOp   [r] [a1,o1,a2,o2] = PrimInline [j| `r` = (`o1` <  `o2`) ? 1 : 0; |]
genPrim AddrLeOp   [r] [a1,o1,a2,o2] = PrimInline [j| `r` = (`o1` <= `o2`) ? 1 : 0; |]

-- addr indexing: unboxed arrays
genPrim IndexOffAddrOp_Char [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint8(`o`+`i`); |]
genPrim IndexOffAddrOp_WideChar [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint32(`o`+(`i`<<2)); |]

genPrim IndexOffAddrOp_Int [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt32(`o`+(`i`<<2)); |]
genPrim IndexOffAddrOp_Word [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint32(`o`+(`i`<<2)); |]
genPrim IndexOffAddrOp_Addr [ca,co] [a,o,i] =
  PrimInline [j| if(`a`.arr && `a`.arr[`o`+(`i`<<2)]) {
                   `ca` = `a`.arr[`o`+(`i`<<2)][0];
                   `co` = `a`.arr[`o`+(`i`<<2)][1];
                 } else {
                   `ca` = null;
                   `co` = 0;
                 }
              |]
genPrim IndexOffAddrOp_Float [c] [a,o,i] = PrimInline [j| `c` = `a`.getFloat32(`o`+(`i`<<2)); |]
genPrim IndexOffAddrOp_Double [c] [a,o,i] = PrimInline [j| `c` = `a`.getFloat64(`o`+(`i`<<3)); |]
{-
IndexOffAddrOp_StablePtr
-}
genPrim IndexOffAddrOp_Int8 [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt8(`o`+`i`); |]
genPrim IndexOffAddrOp_Int16 [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt16(`o`+(`i`<<1)); |]
genPrim IndexOffAddrOp_Int32 [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt32(`o`+(`i`<<2)); |]
genPrim IndexOffAddrOp_Int64 [c1,c2] [a,o,i] =  -- fixme one of these should be unsigned
   PrimInline [j| `c1` = `a`.getInt32(`o`+(`i`<<3));
                  `c2` = `a`.getInt32(`o`+(`i`<<3)+4);
                |]
genPrim IndexOffAddrOp_Word8 [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint8(`o`+`i`); |]
genPrim IndexOffAddrOp_Word16 [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint16(`o`+(`i`<<1)); |]
genPrim IndexOffAddrOp_Word32 [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint32(`o`+(`i`<<2)); |]
genPrim IndexOffAddrOp_Word64 [c1,c2] [a,o,i] =
   PrimInline [j| `c1` = `a`.getUint32(`o`+(`i`<<3));
                  `c2` = `a`.getUint32(`o`+(`i`<<3)+4);
                |]
genPrim ReadOffAddrOp_Char [c] [a,o,i] =
   PrimInline [j| `c` = `a`.getUint8(`o`+`i`); |]
genPrim ReadOffAddrOp_WideChar [c] [a,o,i] =
   PrimInline [j| `c` = `a`.getUint32(`o`+(`i`<<2)); |]
genPrim ReadOffAddrOp_Int [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt32(`o`+(`i`<<2)); |]
genPrim ReadOffAddrOp_Word [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint32(`o`+(`i`<<2)); |]
genPrim ReadOffAddrOp_Addr [c1,c2] [a,o,i] =
  PrimInline [j| var x = `i`<<2;
                 if(`a`.arr && `a`.arr[`o`+ x]) {
                   `c1` = `a`.arr[`o`+ x][0];
                   `c2` = `a`.arr[`o`+ x][1];
                 } else {
                   `c1` = null;
                   `c2` = 0;
                 }
               |]
genPrim ReadOffAddrOp_Float [c] [a,o,i] = PrimInline [j| `c` = `a`.getFloat32(`o`+(`i`<2)); |]
genPrim ReadOffAddrOp_Double [c] [a,o,i] = PrimInline [j| `c` = `a`.getFloat64(`o`+(`i`<<3)); |]
-- ReadOffAddrOp_StablePtr -- fixme
genPrim ReadOffAddrOp_Int8   [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt8(`o`+`i`); |]
genPrim ReadOffAddrOp_Int16  [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt16(`o`+(`i`<<1)); |]
genPrim ReadOffAddrOp_Int32  [c] [a,o,i] = PrimInline [j| `c` = `a`.getInt32(`o`+(`i`<<2)); |]
genPrim ReadOffAddrOp_Word8  [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint8(`o`+`i`); |]
genPrim ReadOffAddrOp_Word16 [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint16(`o`+(`i`<<1)); |]
genPrim ReadOffAddrOp_Word32 [c] [a,o,i] = PrimInline [j| `c` = `a`.getUint32(`o`+(`i`<<2)); |]
genPrim ReadOffAddrOp_Word64 [c1,c2] [a,o,i] =
   PrimInline [j| `c1` = `a`.getUint32(`o`+(`i`<<3));
                  `c2` = `a`.getUint32(`o`+(`i`<<3)+4);
                |]
genPrim WriteOffAddrOp_Char [] [a,o,i,v]     = PrimInline [j| `a`.setUint8(`o`+`i`, `v`); |]
genPrim WriteOffAddrOp_WideChar [] [a,o,i,v] = PrimInline [j| `a`.setUint32(`o`+(`i`<<2), `v`); |]
genPrim WriteOffAddrOp_Int [] [a,o,i,v]     = PrimInline [j| `a`.setInt32(`o`+(`i`<<2), `v`); |]
genPrim WriteOffAddrOp_Word [] [a,o,i,v]    = PrimInline [j| `a`.setUint32(`o`+(`i`<<2), `v`); |]
genPrim WriteOffAddrOp_Addr [] [a,o,i,va,vo] =
  PrimInline [j| if(!`a`.arr) { `a`.arr = []; }
                 `a`.arr[`o`+(`i`<<2)] = [`va`,`vo`];
               |]
genPrim WriteOffAddrOp_Float [] [a,o,i,v]   = PrimInline [j| `a`.setFloat32(`o`+(`i`<<2), `v`); |]
genPrim WriteOffAddrOp_Double [] [a,o,i,v]  = PrimInline [j| `a`.setFloat64(`o`+(`i`<<3),`v`); |]
-- WriteOffAddrOp_StablePtr
genPrim WriteOffAddrOp_Int8 [] [a,o,i,v]    = PrimInline [j| `a`.setInt8(`o`+`i`, `v`); |]
genPrim WriteOffAddrOp_Int16 [] [a,o,i,v]   = PrimInline [j| `a`.setInt16(`o`+(`i`<<1), `v`); |]
genPrim WriteOffAddrOp_Int32 [] [a,o,i,v]   = PrimInline [j| `a`.setInt32(`o`+(`i`<<2), `v`); |]
genPrim WriteOffAddrOp_Int64 [] [a,o,i,v1,v2] = PrimInline [j| `a`.setInt32(`o`+(`i`<<3), `v1`);
                                                               `a`.setInt32(`o`+(`i`<<3)+4, `v2`);
                                                             |]
genPrim WriteOffAddrOp_Word8 [] [a,o,i,v]   = PrimInline [j| `a`.setUint8(`o`+`i`, `v`); |]
genPrim WriteOffAddrOp_Word16 [] [a,o,i,v]  = PrimInline [j| `a`.setUint16(`o`+(`i`<<1), `v`); |]
genPrim WriteOffAddrOp_Word32 [] [a,o,i,v]  = PrimInline [j| `a`.setUint32(`o`+(`i`<<2), `v`); |]
genPrim WriteOffAddrOp_Word64 [] [a,o,i,v1,v2] = PrimInline [j| `a`.setUint32(`o`+(`i`<<3), `v1`);
                                                                 `a`.setUint32(`o`+(`i`<<3)+4, `v2`);
                                                               |]
-- fixme add GC writebars
genPrim NewMutVarOp       [r] [x]   = PrimInline [j| `r` = [`x`];  |]
genPrim ReadMutVarOp      [r] [m]   = PrimInline [j| `r` = `m`[0]; |]
-- fimxe, update something to inform gc that this thing can point to newer stuff
genPrim WriteMutVarOp     [] [m,x]  = PrimInline [j| `m`[0] = `x`; |]
genPrim SameMutVarOp      [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0; |]
-- genPrim AtomicModifyMutVarOp [r] [x,y] =
-- CasMutVarOp
-- actual exception handling: push catch handling stack frame
-- [stg_catch, handler]
-- raise: unwind stack until first stg_catch
genPrim CatchOp [r] [a,handler] = PRPrimCall
  [j| return stg_catch(`a`, `handler`); |]
genPrim RaiseOp         [b] [a] = PRPrimCall [j| return stg_throw(`a`); |]
genPrim RaiseIOOp       [b] [a] = PRPrimCall [j| return stg_throw(`a`); |]

genPrim MaskAsyncExceptionsOp [r] [a] =
  PRPrimCall [j| mask = 1; `R1` = `a`; return stg_ap_v_fast(); |]
-- MaskUninterruptibleOp
genPrim UnmaskAsyncExceptionsOp [r] [a] =
  PRPrimCall [j| mask = 0; `R1` = `a`; return stg_ap_v_fast(); |]

genPrim MaskStatus [r] [] = PrimInline [j| `r` = mask; |]
{-
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
-}
genPrim NewMVarOp [r] []   = PrimInline [j| `r` = [null]; |]
genPrim TakeMVarOp [r] [m] =
  PrimInline [j| if(`m`[0] !== null) {
                   `r` = `m`[0];
                   `m`[0] = null;
                 } else {
                   throw "blocked on MVar";
                 }
               |]
genPrim TryTakeMVarOp [v,r] [m] =
  PrimInline [j| if(`m`[0] !== null) {
                   `v` = 1;
                   `r` = `m`[0];
                   `m`[0] = null;
                 } else {
                   `v` = 0;
                 }
               |]
genPrim PutMVarOp [] [m,v] =
  PrimInline [j| if(`m`[0] !== null) {
                   throw "blocked on MVar";
                 } else {
                   `m`[0] = `v`;
                 }
               |]
genPrim TryPutMVarOp [r] [m,v] =
  PrimInline [j| if(`m`[0] !== null) {
                   `r` = 0;
                 } else {
                   `r` = 1;
                   `m`[0] = `v`;
                 }
               |]
genPrim SameMVarOp [r] [m1,m2] = PrimInline [j| `r` = (`m1` === `m2`) ? 1 : 0; |]
genPrim IsEmptyMVarOp [r] [m]  = PrimInline [j| `r` = (`m`[0] === null) ? 1 : 0; |]
{-
DelayOp
WaitReadOp
WaitWriteOp
ForkOp
ForkOnOp
KillThreadOp
YieldOp
MyThreadIdOp
LabelThreadOp
-}
genPrim IsCurrentThreadBoundOp [r] [] = PrimInline [j| `r` = 1; |]
genPrim NoDuplicateOp [] [] = PrimInline mempty -- fixme what to do here?
{-
ThreadStatusOp
-}
genPrim MkWeakOp [r] [o,b,c] = PrimInline [j| `r` = [`b`,`c`]; |] -- fixme c = finalizer, what is o?
genPrim MkWeakForeignEnvOp [r] [o,b,a1a,a1o,a2a,a2o,i,a3a,a3o] = PrimInline [j| `r` = [`b`]; |]
genPrim DeRefWeakOp        [r] [w] = PrimInline [j| `r` = `w`[0]; |]
genPrim FinalizeWeakOp     [i,a] [w] =
  PrimInline [j| if(`w`.length > 1 && `w`[1] !== null) {
                   `a` = `w`[1];
                   `i` = 1;
                 } else {
                   `i` = 0;
                 }
               |]
genPrim TouchOp [] [e] = PrimInline mempty -- fixme what to do?
{-
MakeStablePtrOp
DeRefStablePtrOp
EqStablePtrOp
MakeStableNameOp
EqStableNameOp
StableNameToIntOp
ReallyUnsafePtrEqualityOp
ParOp
SparkOp
-}
genPrim SeqOp [r] [e] = PRPrimCall [j| var ht = `Heap`[`e`];
                                       return (ht.t === `Thunk`) ? ht : `Stack`[`Sp`];
                                     |]
{-
GetSparkOp
NumSparks
ParGlobalOp
ParLocalOp
ParAtOp
ParAtAbsOp
ParAtRelOp
ParAtForNowOp
-}
genPrim DataToTagOp [r] [d] = PrimInline [j| `r` = `Heap`[`d`].a - 1 |]
genPrim TagToEnumOp [r] [t] = PrimInline [j| `r` = $hs_tagToEnum(`t`) |]
{-
AddrToAnyOp
MkApUpd0_Op
NewBCOOp
UnpackClosureOp
GetApStackValOp
GetCCSOfOp
GetCurrentCCSOp
TraceEventOp
-}

-- genPrim op rs as = PrimInline [j| throw `"unhandled primop:"++show op++" "++show (length rs, length as)`; |]
genPrim op rs as = PrimInline [j| log(`"warning, unhandled primop: "++show op++" "++show (length rs, length as)`);
  `f`;
  `copyRes`;
|]
  where
    f = ApplStat (iex . StrI $ "$hs_prim_" ++ show op) as
    copyRes = mconcat $ zipWith (\r reg -> [j| `r` = `reg`; |]) rs (enumFrom Ret1)


-- fixme add fallback for untyped browsers
newByteArray :: JExpr -> JExpr -> JStat
newByteArray tgt len = [j| `tgt` = new DataView(new ArrayBuffer(Math.max(`len`,1)),0,`len`); |]

newArray :: JExpr -> JExpr -> JStat
newArray tgt len = [j| `tgt` = new Int32Array(new ArrayBuffer(Math.max(4*`len`,1)),0,`len`); |]

two_24 :: Int
two_24 = 2^24
