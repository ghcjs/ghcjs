{-# LANGUAGE QuasiQuotes,
             TemplateHaskell,
             CPP #-}
module Gen2.Prim where

{-
  unboxed representations:

    Int#               -> number
    Double#            -> number
    Float#             -> number
    Char#              -> number
    Word#              -> number (values > 2^31 are mapped to negative numbers)
    Addr#              -> wrapped buffer + offset (number)
        (with some hacks for pointers to pointers in the .arr property)
    MutVar#            -> h$MutVar object
    TVar#              -> h$TVar object
    MVar#              -> h$MVar object
    Weak#              -> h$Weak object
    ThreadId#          -> h$Thread object
    State#             -> nothing
    StablePtr#         -> wrapped buffer / offset (base pkg expects unsafeCoerce to Addr# to work)
    MutableArrayArray# -> array
    MutableByteArray#  -> wrapped buffer
    ByteArray#         -> wrapped buffer
    Array#             -> array

  Pointers to pointers use a special representation with the .arr property
-}

import           DynFlags
import           PrimOp
import           CoreSyn
import           DataCon
import           TcType hiding (Check)
import           Type
import           TyCon

import qualified Data.Set as S

import           Compiler.JMacro (j, je, JExpr(..), JStat(..), Ident(..))

import           Gen2.Profiling
import           Gen2.RtsTypes
import           Gen2.Utils

import qualified Data.Text as T

data PrimRes = PrimInline JStat  -- ^ primop is inline, result is assigned directly
             | PRPrimCall JStat  -- ^ primop is async call, primop returns the next
                                 --     function to run. result returned to stack top in registers

isInlinePrimOp :: PrimOp -> Bool
isInlinePrimOp p = p `S.notMember` notInlinePrims
  where
    -- all primops that might block the thread or manipulate stack directly
    -- (and therefore might return PRPrimCall) must be listed here
    notInlinePrims = S.fromList
      [ CatchOp, RaiseOp, RaiseIOOp
      , MaskAsyncExceptionsOp, MaskUninterruptibleOp, UnmaskAsyncExceptionsOp
      , AtomicallyOp, RetryOp, CatchRetryOp, CatchSTMOp
      , TakeMVarOp, PutMVarOp, ReadMVarOp
      , DelayOp
      , WaitReadOp, WaitWriteOp
      , ForkOp, ForkOnOp
      , KillThreadOp
      , YieldOp
      , CompactAdd, CompactAddWithSharing
      , SeqOp
      , DataToTagOp
      ]


genPrim :: DynFlags
        -> Type
        -> PrimOp   -- ^ the primitive operation
        -> [JExpr]  -- ^ where to store the result
        -> [JExpr]  -- ^ arguments
        -> PrimRes
genPrim _ _ CharGtOp          [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0; |]
genPrim _ _ CharGeOp          [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0; |]
genPrim _ _ CharEqOp          [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0; |]
genPrim _ _ CharNeOp          [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0; |]
genPrim _ _ CharLtOp          [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0; |]
genPrim _ _ CharLeOp          [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0; |]
genPrim _ _ OrdOp             [r] [x]   = PrimInline [j| `r` = `x` |]

genPrim _ _ IntAddOp          [r] [x,y] = PrimInline [j| `r` = (`x` + `y`)|0 |]
genPrim _ _ IntSubOp          [r] [x,y] = PrimInline [j| `r` = (`x` - `y`)|0 |]
genPrim _ _ IntMulOp          [r] [x,y] =
    PrimInline [j| `r` = h$mulInt32(`x`,`y`); |]
-- fixme may will give the wrong result in case of overflow
genPrim _ _ IntMulMayOfloOp   [r] [x,y] =
    PrimInline [j| var tmp = (`x`*`y`); `r` = (tmp===(tmp|0))?0:1; |]
genPrim _ _ IntQuotOp         [r] [x,y] = PrimInline [j| `r` = (`x`/`y`)|0; |]
genPrim _ _ IntRemOp          [r] [x,y] = PrimInline [j| `r` = `x` % `y` |]
genPrim _ _ IntQuotRemOp    [q,r] [x,y] = PrimInline [j| `q` = (`x`/`y`)|0;
                                                         `r` = `x`-`y`*`q`;
                                                       |]
genPrim _ _ AndIOp [r] [x,y]            = PrimInline [j| `r` = `x` & `y`; |]
genPrim _ _ OrIOp  [r] [x,y]            = PrimInline [j| `r` = `x` | `y`; |]
genPrim _ _ XorIOp [r] [x,y]            = PrimInline [j| `r` = `x` ^ `y`; |]
genPrim _ _ NotIOp [r] [x]              = PrimInline [j| `r` = ~`x`; |]

genPrim _ _ IntNegOp          [r] [x]   = PrimInline [j| `r` = `jneg x`|0; |]
-- add with carry: overflow == 0 iff no overflow
genPrim _ _ IntAddCOp         [r,overf] [x,y] =
  PrimInline [j| var rt = `x`+`y`; `r` = rt|0; `overf` = (`r`!=rt)?1:0; |]
genPrim _ _ IntSubCOp         [r,overf] [x,y] =
  PrimInline [j| var rt = `x`-`y`; `r` = rt|0; `overf` = (`r`!=rt)?1:0; |]
genPrim _ _ IntGtOp           [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0 |]
genPrim _ _ IntGeOp           [r] [x,y] = PrimInline [j| `r`= (`x` >= `y`) ? 1 : 0 |]
genPrim _ _ IntEqOp           [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim _ _ IntNeOp           [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim _ _ IntLtOp           [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim _ _ IntLeOp           [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
genPrim _ _ ChrOp             [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim _ _ Int2WordOp        [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim _ _ Int2FloatOp       [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim _ _ Int2DoubleOp      [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim _ _ ISllOp            [r] [x,y] = PrimInline [j| `r` = `x` << `y` |]
genPrim _ _ ISraOp            [r] [x,y] = PrimInline [j| `r` = `x` >> `y` |]
genPrim _ _ ISrlOp            [r] [x,y] = PrimInline [j| `r` = (`x` >>> `y`)|0; |]
genPrim _ _ WordAddOp         [r] [x,y] = PrimInline [j| `r` = (`x` + `y`)|0; |]
genPrim _ _ WordAddCOp      [r,c] [x,y] = PrimInline [j| var t = (`x`>>>0) + (`y`>>>0);
                                                         `r` = t|0;
                                                         `c` = t>4294967295?1:0;
                                                       |]
genPrim _ _ WordAdd2Op      [h,l] [x,y] = PrimInline [j| `h` = h$wordAdd2(`x`,`y`);
                                                         `l` = `Ret1`;
                                                       |]
genPrim _ _ WordSubOp         [r] [x,y] = PrimInline [j| `r` = (`x` - `y`)|0 |]
genPrim _ _ WordSubCOp        [r,c] [x,y] =
  PrimInline [j| `r` = (`x` - `y`)|0;
                 `c` = ((`y` >>> 0) > (`x` >>> 0)) ? 1 : 0;
               |]
genPrim _ _ WordMulOp         [r] [x,y] =
  PrimInline [j| `r` = h$mulWord32(`x`,`y`); |]
genPrim _ _ WordMul2Op      [h,l] [x,y] =
  PrimInline [j| `h` = h$mul2Word32(`x`,`y`);
                 `l` = `Ret1`;
               |]
genPrim _ _ WordQuotOp        [q] [x,y] = PrimInline [j| `q` = h$quotWord32(`x`,`y`); |]
genPrim _ _ WordRemOp         [r] [x,y] = PrimInline [j| `r`= h$remWord32(`x`,`y`); |]
genPrim _ _ WordQuotRemOp   [q,r] [x,y] = PrimInline [j| `q` = h$quotWord32(`x`,`y`);
                                                         `r` = h$remWord32(`x`, `y`);
                                                       |]
genPrim _ _ WordQuotRem2Op   [q,r] [xh,xl,y] = PrimInline [j| `q` = h$quotRem2Word32(`xh`,`xl`,`y`);
                                                              `r` = `Ret1`;
                                                            |]
genPrim _ _ AndOp             [r] [x,y] = PrimInline [j| `r` = `x` & `y` |]
genPrim _ _ OrOp              [r] [x,y] = PrimInline [j| `r` = `x` | `y` |]
genPrim _ _ XorOp             [r] [x,y] = PrimInline [j| `r` = `x` ^ `y` |]
genPrim _ _ NotOp             [r] [x]   = PrimInline [j| `r` = ~`x` |]
genPrim _ _ SllOp             [r] [x,y] = PrimInline [j| `r` = `x` << `y` |]
genPrim _ _ SrlOp             [r] [x,y] = PrimInline [j| `r` = (`x` >>> `y`)|0; |]
genPrim _ _ Word2IntOp        [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim _ _ WordGtOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) > (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) > (`y`&1))) ? 1 : 0 |]
genPrim _ _ WordGeOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) > (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) >= (`y`&1))) ? 1 : 0 |]
genPrim _ _ WordEqOp          [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim _ _ WordNeOp          [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim _ _ WordLtOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) < (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) < (`y`&1))) ? 1 : 0 |]
genPrim _ _ WordLeOp          [r] [x,y] =
  PrimInline [j| `r` = ((`x`>>>1) < (`y`>>>1) || ((`x`>>>1) == (`y`>>>1) && (`x`&1) <= (`y`&1))) ? 1 : 0 |]
genPrim _ _ Word2DoubleOp     [r] [x] = PrimInline [j| `r` = (`x` & 0x7FFFFFFF) + (`x` >>> 31) * 2147483648 |]
genPrim _ _ Word2FloatOp      [r] [x] = PrimInline [j| `r` = (`x` & 0x7FFFFFFF) + (`x` >>> 31) * 2147483648 |]
genPrim _ _ PopCnt8Op         [r] [x]   = PrimInline [j| `r` = h$popCntTab[`x` & 0xFF] |]
genPrim _ _ PopCnt16Op        [r] [x]   =
  PrimInline [j| `r` = h$popCntTab[`x`&0xFF] +
                       h$popCntTab[(`x`>>>8)&0xFF]
               |]
genPrim _ _ PopCnt32Op        [r] [x]   = PrimInline [j| `r` = h$popCnt32(`x`); |]
genPrim _ _ PopCnt64Op        [r] [x1,x2] = PrimInline [j| `r` = h$popCnt64(`x1`,`x2`); |]
genPrim d t PopCntOp          [r] [x]   = genPrim d t PopCnt32Op [r] [x]
genPrim _ _ Pdep8Op           [r] [s,m] = PrimInline [j| `r` = h$pdep8(`s`, `m`); |]
genPrim _ _ Pdep16Op          [r] [s,m] = PrimInline [j| `r` = h$pdep16(`s`, `m`); |]
genPrim _ _ Pdep32Op          [r] [s,m] = PrimInline [j| `r` = h$pdep32(`s`, `m`); |]
genPrim _ _ Pdep64Op          [ra,rb] [sa,sb,ma,mb] = PrimInline
  [j| `ra` = h$pdep64(`sa`,`sb`,`ma`,`mb`); `rb` = `Ret1`; |]
genPrim d t PdepOp            rs xs = genPrim d t Pdep32Op rs xs
genPrim _ _ Pext8Op           [r] [s,m] = PrimInline [j| `r` = h$pext8(`s`, `m`); |]
genPrim _ _ Pext16Op          [r] [s,m] = PrimInline [j| `r` = h$pext16(`s`, `m`); |]
genPrim _ _ Pext32Op          [r] [s,m] = PrimInline [j| `r` = h$pext32(`s`, `m`); |]
genPrim _ _ Pext64Op          [ra,rb] [sa,sb,ma,mb] = PrimInline
  [j| `ra` = h$pext64(`sa`,`sb`,`ma`,`mb`); `rb` = `Ret1`; |]
genPrim d t PextOp            rs xs     = genPrim d t Pext32Op rs xs
genPrim _ _ BSwap16Op         [r] [x]   = PrimInline [j| `r` = ((`x` & 0xFF) << 8) | ((`x` & 0xFF00) >> 8); |] -- ab -> ba
genPrim _ _ BSwap32Op         [r] [x]   = PrimInline [j| `r` = (`x` << 24) | ((`x` & 0xFF00) << 8)
                                                             | ((`x` & 0xFF0000) >> 8) | (`x` >>> 24);
                                                       |] -- abcd -> dcba
genPrim _ _ BSwap64Op     [r1,r2] [x,y] = PrimInline [j| `r1` = h$bswap64(`x`,`y`);
                                                         `r2` = `Ret1`;
                                                       |]
genPrim d t BSwapOp           [r] [x]   = genPrim d t BSwap32Op [r] [x]

genPrim _ _ Narrow8IntOp      [r] [x]   = PrimInline [j| `r` = (`x` & 0x7F)-(`x` & 0x80) |]
genPrim _ _ Narrow16IntOp     [r] [x]   = PrimInline [j| `r` = (`x` & 0x7FFF)-(`x` & 0x8000) |]
genPrim _ _ Narrow32IntOp     [r] [x]   = PrimInline [j| `r` = `x`|0 |]
genPrim _ _ Narrow8WordOp     [r] [x]   = PrimInline [j| `r` = (`x` & 0xFF) |]
genPrim _ _ Narrow16WordOp    [r] [x]   = PrimInline [j| `r` = (`x` & 0xFFFF) |]
genPrim _ _ Narrow32WordOp    [r] [x]   = PrimInline [j| `r` = `x`|0 |]
genPrim _ _ DoubleGtOp        [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0 |]
genPrim _ _ DoubleGeOp        [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0 |]
genPrim _ _ DoubleEqOp        [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim _ _ DoubleNeOp        [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim _ _ DoubleLtOp        [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim _ _ DoubleLeOp        [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
genPrim _ _ DoubleAddOp       [r] [x,y] = PrimInline [j| `r` = `x` + `y` |]
genPrim _ _ DoubleSubOp       [r] [x,y] = PrimInline [j| `r` = `x` - `y` |]
genPrim _ _ DoubleMulOp       [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]
genPrim _ _ DoubleDivOp       [r] [x,y] = PrimInline [j| `r` = `x` / `y` |]
genPrim _ _ DoubleNegOp       [r] [x]   = PrimInline [j| `r` = `jneg x` |] -- fixme negate
genPrim _ _ DoubleFabsOp      [r] [x]   = PrimInline [j| `r` = Math.abs(`x`) |]
genPrim _ _ Double2IntOp      [r] [x]   = PrimInline [j| `r` = `x`|0; |]
genPrim _ _ Double2FloatOp    [r] [x]   = PrimInline [j| `r` = h$fround(`x`) |]
genPrim _ _ DoubleExpOp       [r] [x]   = PrimInline [j| `r` = Math.exp(`x`) |]
genPrim _ _ DoubleLogOp       [r] [x]   = PrimInline [j| `r` = Math.log(`x`) |]
genPrim _ _ DoubleSqrtOp      [r] [x]   = PrimInline [j| `r` = Math.sqrt(`x`) |]
genPrim _ _ DoubleSinOp       [r] [x]   = PrimInline [j| `r` = Math.sin(`x`) |]
genPrim _ _ DoubleCosOp       [r] [x]   = PrimInline [j| `r` = Math.cos(`x`) |]
genPrim _ _ DoubleTanOp       [r] [x]   = PrimInline [j| `r` = Math.tan(`x`) |]
genPrim _ _ DoubleAsinOp      [r] [x]   = PrimInline [j| `r` = Math.asin(`x`) |]
genPrim _ _ DoubleAcosOp      [r] [x]   = PrimInline [j| `r` = Math.acos(`x`) |]
genPrim _ _ DoubleAtanOp      [r] [x]   = PrimInline [j| `r` = Math.atan(`x`) |]
genPrim _ _ DoubleSinhOp      [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)-Math.exp(`jneg x`))/2 |]
genPrim _ _ DoubleCoshOp      [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)+Math.exp(`jneg x`))/2 |]
genPrim _ _ DoubleTanhOp      [r] [x]   = PrimInline [j| `r` = (Math.exp(2*`x`)-1)/(Math.exp(2*`x`)+1) |]
genPrim _ _ DoublePowerOp     [r] [x,y] = PrimInline [j| `r` = Math.pow(`x`,`y`) |]
genPrim _ _ DoubleDecode_2IntOp [s,h,l,e] [x] =
  PrimInline [j| `s` = h$decodeDouble2Int(`x`);
                 `h` = `Ret1`;
                 `l` = `Ret2`;
                 `e` = `Ret3`;
               |]
genPrim _ _ DoubleDecode_Int64Op [s1,s2,e] [d] =
  PrimInline [j| `e`  = h$decodeDoubleInt64(`d`);
                 `s1` = `Ret1`;
                 `s2` = `Ret2`;
               |]
genPrim _ _ FloatGtOp         [r] [x,y] = PrimInline [j| `r` = (`x` > `y`) ? 1 : 0 |]
genPrim _ _ FloatGeOp         [r] [x,y] = PrimInline [j| `r` = (`x` >= `y`) ? 1 : 0 |]
genPrim _ _ FloatEqOp         [r] [x,y] = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0 |]
genPrim _ _ FloatNeOp         [r] [x,y] = PrimInline [j| `r` = (`x` !== `y`) ? 1 : 0 |]
genPrim _ _ FloatLtOp         [r] [x,y] = PrimInline [j| `r` = (`x` < `y`) ? 1 : 0 |]
genPrim _ _ FloatLeOp         [r] [x,y] = PrimInline [j| `r` = (`x` <= `y`) ? 1 : 0 |]
genPrim _ _ FloatAddOp        [r] [x,y] = PrimInline [j| `r` = `x` + `y` |]
genPrim _ _ FloatSubOp        [r] [x,y] = PrimInline [j| `r` = `x` - `y` |]
genPrim _ _ FloatMulOp        [r] [x,y] = PrimInline [j| `r` = `x` * `y` |]
genPrim _ _ FloatDivOp        [r] [x,y] = PrimInline [j| `r` = `x` / `y` |]
genPrim _ _ FloatNegOp        [r] [x]   = PrimInline [j| `r` = `jneg x`  |]
genPrim _ _ FloatFabsOp       [r] [x]   = PrimInline [j| `r` = Math.abs(`x`) |]
genPrim _ _ Float2IntOp       [r] [x]   = PrimInline [j| `r` = `x`|0 |]
genPrim _ _ FloatExpOp        [r] [x]   = PrimInline [j| `r` = Math.exp(`x`) |]
genPrim _ _ FloatLogOp        [r] [x]   = PrimInline [j| `r` = Math.log(`x`) |]
genPrim _ _ FloatSqrtOp       [r] [x]   = PrimInline [j| `r` = Math.sqrt(`x`) |]
genPrim _ _ FloatSinOp        [r] [x]   = PrimInline [j| `r` = Math.sin(`x`) |]
genPrim _ _ FloatCosOp        [r] [x]   = PrimInline [j| `r` = Math.cos(`x`) |]
genPrim _ _ FloatTanOp        [r] [x]   = PrimInline [j| `r` = Math.tan(`x`) |]
genPrim _ _ FloatAsinOp       [r] [x]   = PrimInline [j| `r` = Math.asin(`x`) |]
genPrim _ _ FloatAcosOp       [r] [x]   = PrimInline [j| `r` = Math.acos(`x`) |]
genPrim _ _ FloatAtanOp       [r] [x]   = PrimInline [j| `r` = Math.atan(`x`) |]
genPrim _ _ FloatSinhOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)-Math.exp(`jneg x`))/2 |]
genPrim _ _ FloatCoshOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(`x`)+Math.exp(`jneg x`))/2 |]
genPrim _ _ FloatTanhOp       [r] [x]   = PrimInline [j| `r` = (Math.exp(2*`x`)-1)/(Math.exp(2*`x`)+1) |]
genPrim _ _ FloatPowerOp      [r] [x,y] = PrimInline [j| `r` = Math.pow(`x`,`y`) |]
genPrim _ _ Float2DoubleOp    [r] [x]   = PrimInline [j| `r` = `x` |]
genPrim _ _ FloatDecode_IntOp [s,e] [x] = PrimInline [j| `s` = h$decodeFloatInt(`x`); `e` = `Ret1`; |]
genPrim _ _ NewArrayOp          [r] [l,e]   = PrimInline (newArray r l e)
genPrim _ _ SameMutableArrayOp  [r] [a1,a2] = PrimInline [j| `r` = (`a1` === `a2`) ? 1 : 0 |]
genPrim _ _ ReadArrayOp         [r] [a,i]   = PrimInline [j| `r` = `a`[`i`]; |]
genPrim _ _ WriteArrayOp        []  [a,i,v] = PrimInline [j| `a`[`i`] = `v`; |]
genPrim _ _ SizeofArrayOp       [r] [a]     = PrimInline [j| `r` = `a`.length; |]
genPrim _ _ SizeofMutableArrayOp [r] [a]    = PrimInline [j| `r` = `a`.length; |]
genPrim _ _ IndexArrayOp        [r] [a,i]   = PrimInline [j| `r` = `a`[`i`]; |]
genPrim _ _ UnsafeFreezeArrayOp [r] [a]     = PrimInline [j| `r` = `a`; |]
genPrim _ _ UnsafeThawArrayOp   [r] [a]     = PrimInline [j| `r` = `a`; |]
genPrim _ _ CopyArrayOp         [] [a,o1,ma,o2,n] =
  PrimInline [j| for(var i=0;i<`n`;i++) {
                   `ma`[i+`o2`] = `a`[i+`o1`];
                 }
               |]
genPrim _ _ ResizeMutableByteArrayOp_Char [r] [a,n] =
  PrimInline [j| `r` = h$resizeMutableByteArray(`a`, `n`); |]
genPrim _ _ ShrinkMutableByteArrayOp_Char [] [a,n] =
  PrimInline [j| h$shrinkMutableByteArray(`a`, `n`); |]
genPrim _ _ CompareByteArraysOp [r] [a1,o1,a2,o2,n] =
  PrimInline [j| `r` = h$compareByteArrays(`a1`,`o1`,`a2`,`o2`,`n`); |]
genPrim d t CopyMutableArrayOp  [] [a1,o1,a2,o2,n] =
  genPrim d t CopyArrayOp [] [a1,o1,a2,o2,n]
genPrim _ _ CloneArrayOp        [r] [a,start,n] =
  PrimInline [j| `r` = h$sliceArray(`a`,`start`,`n`) |]
genPrim d t CloneMutableArrayOp [r] [a,start,n] =
  genPrim d t CloneArrayOp [r] [a,start,n]
genPrim _ _ FreezeArrayOp       [r] [a,start,n] =
  PrimInline [j| `r` = h$sliceArray(`a`,`start`,`n`); |]
genPrim _ _ ThawArrayOp         [r] [a,start,n] =
  PrimInline [j| `r` = h$sliceArray(`a`,`start`,`n`); |]
genPrim _ _ NewByteArrayOp_Char [r] [l] = PrimInline (newByteArray r l)
genPrim _ _ NewPinnedByteArrayOp_Char [r] [l] = PrimInline (newByteArray r l)
genPrim _ _ NewAlignedPinnedByteArrayOp_Char [r] [l,_align] =
  PrimInline (newByteArray r l)
genPrim _ _ MutableByteArrayIsPinnedOp [r] [_] = PrimInline [j| `r` = 1; |]
genPrim _ _ ByteArrayIsPinnedOp [r] [_] = PrimInline [j| `r` = 1; |]
genPrim _ _ ByteArrayContents_Char [a,o] [b] =
  PrimInline [j| `a` = `b`; `o` = 0; |]
genPrim _ _ SameMutableByteArrayOp [r] [a,b] =
  PrimInline [j| `r` = (`a` === `b`) ? 1 : 0 |]
genPrim _ _ UnsafeFreezeByteArrayOp [a] [b] = PrimInline [j| `a` = `b`; |]
genPrim _ _ SizeofByteArrayOp [r] [a] = PrimInline [j| `r` = `a`.len; |]
genPrim _ _ SizeofMutableByteArrayOp [r] [a] = PrimInline [j| `r` = `a`.len; |]
genPrim _ _ GetSizeofMutableByteArrayOp [r] [a] = PrimInline [j| `r` = `a`.len; |]
genPrim _ _ IndexByteArrayOp_Char [r] [a,i] = PrimInline [j| `r` = `a`.u8[`i`]; |]
genPrim _ _ IndexByteArrayOp_WideChar [r] [a,i] = PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Int [r] [a,i] = PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Word [r] [a,i] = PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Addr [r1,r2] [a,i] =
  PrimInline [j| if(`a`.arr && `a`.arr[`i`<<2]) {
                   `r1` = `a`.arr[`i`<<2][0];
                   `r2` = `a`.arr[`i`<<2][1];
                 } else {
                   `r1` = null;
                   `r2` = 0;
                 }
               |]
genPrim _ _ IndexByteArrayOp_Float [r] [a,i] =
  PrimInline [j| `r` = `a`.f3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Double [r] [a,i] =
  PrimInline [j| `r` = `a`.f6[`i`]; |]
genPrim _ _ IndexByteArrayOp_StablePtr [r1,r2] [a,i] =
  PrimInline [j| `r1` = h$stablePtrBuf; `r2` = `a`.i3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Int8 [r] [a,i] =
  PrimInline [j| `r` = `a`.dv.getInt8(`i`,true); |]
genPrim _ _ IndexByteArrayOp_Int16 [r] [a,i] =
  PrimInline [j| `r` = `a`.dv.getInt16(`i`<<1,true); |]
genPrim _ _ IndexByteArrayOp_Int32 [r] [a,i] =
  PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Int64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.i3[(`i`<<1)+1];
                 `r2` = `a`.i3[`i`<<1];
               |]
genPrim _ _ IndexByteArrayOp_Word8 [r] [a,i] =
  PrimInline [j| `r` = `a`.u8[`i`]; |]
genPrim _ _ IndexByteArrayOp_Word16 [r] [a,i] =
  PrimInline [j| `r` = `a`.dv.getUint16(`i`<<1,true); |]
genPrim _ _ IndexByteArrayOp_Word32 [r] [a,i] =
  PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ IndexByteArrayOp_Word64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.i3[(`i`<<1)+1];
                 `r2` = `a`.i3[`i`<<1];
               |]
{- new ops in 8.6
   , IndexByteArrayOp_Word8AsChar
   , IndexByteArrayOp_Word8AsWideChar
   , IndexByteArrayOp_Word8AsAddr
   , IndexByteArrayOp_Word8AsFloat
   , IndexByteArrayOp_Word8AsDouble
   , IndexByteArrayOp_Word8AsStablePtr
   , IndexByteArrayOp_Word8AsInt16
   , IndexByteArrayOp_Word8AsInt32
   , IndexByteArrayOp_Word8AsInt64
   , IndexByteArrayOp_Word8AsInt
   , IndexByteArrayOp_Word8AsWord16
   , IndexByteArrayOp_Word8AsWord32
   , IndexByteArrayOp_Word8AsWord64
   , IndexByteArrayOp_Word8AsWord
 -}
genPrim _ _ ReadByteArrayOp_Char [r] [a,i] =
  PrimInline [j| `r` = `a`.u8[`i`]; |]
genPrim _ _ ReadByteArrayOp_WideChar [r] [a,i] =
  PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Int [r] [a,i] =
  PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Word [r] [a,i] =
  PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Addr [r1,r2] [a,i] =
  PrimInline [j| var x = `i`<<2;
                 if(`a`.arr && `a`.arr[x]) {
                   `r1` = `a`.arr[x][0];
                   `r2` = `a`.arr[x][1];
                 } else {
                   `r1` = null;
                   `r2` = 0;
                 }
               |]
genPrim _ _ ReadByteArrayOp_Float [r] [a,i] =
  PrimInline [j| `r` = `a`.f3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Double [r] [a,i] =
  PrimInline [j| `r` = `a`.f6[`i`]; |]
genPrim _ _ ReadByteArrayOp_StablePtr [r1,r2] [a,i] =
   PrimInline [j| `r1` = h$stablePtrBuf; `r2` = `a`.i3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Int8 [r] [a,i] =
  PrimInline [j| `r` = `a`.dv.getInt8(`i`,true); |]
genPrim _ _ ReadByteArrayOp_Int16 [r] [a,i] =
  PrimInline [j| `r` = `a`.dv.getInt16(`i`<<1,true); |]
genPrim _ _ ReadByteArrayOp_Int32 [r] [a,i] =
  PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Int64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.i3[(`i`<<1)+1];
                 `r2` = `a`.i3[`i`<<1];
               |]
genPrim _ _ ReadByteArrayOp_Word8 [r] [a,i] = PrimInline [j| `r` = `a`.u8[`i`]; |]
genPrim _ _ ReadByteArrayOp_Word16 [r] [a,i] = PrimInline [j| `r` = `a`.u1[`i`]; |]
genPrim _ _ ReadByteArrayOp_Word32 [r] [a,i] = PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ ReadByteArrayOp_Word64 [r1,r2] [a,i] =
  PrimInline [j| `r1` = `a`.i3[(`i`<<1)+1];
                 `r2` = `a`.i3[`i`<<1];
               |]
{- new ops in 8.6
   , ReadByteArrayOp_Word8AsChar
   , ReadByteArrayOp_Word8AsWideChar
   , ReadByteArrayOp_Word8AsAddr
   , ReadByteArrayOp_Word8AsFloat
   , ReadByteArrayOp_Word8AsDouble
   , ReadByteArrayOp_Word8AsStablePtr
   , ReadByteArrayOp_Word8AsInt16
   , ReadByteArrayOp_Word8AsInt32
   , ReadByteArrayOp_Word8AsInt64
   , ReadByteArrayOp_Word8AsInt
   , ReadByteArrayOp_Word8AsWord16
   , ReadByteArrayOp_Word8AsWord32
   , ReadByteArrayOp_Word8AsWord64
   , ReadByteArrayOp_Word8AsWord
 -}
genPrim _ _ WriteByteArrayOp_Char [] [a,i,e] = PrimInline [j| `a`.u8[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_WideChar [] [a,i,e] = PrimInline [j| `a`.i3[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Int [] [a,i,e] = PrimInline [j| `a`.i3[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Word [] [a,i,e] = PrimInline [j| `a`.i3[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Addr [] [a,i,e1,e2] = PrimInline [j| if(!`a`.arr) { `a`.arr = []; }
                                                                  `a`.arr[`i`<<2] = [`e1`,`e2`];
                                                                |]
genPrim _ _ WriteByteArrayOp_Float [] [a,i,e] = PrimInline [j| `a`.f3[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Double [] [a,i,e] = PrimInline [j| `a`.f6[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_StablePtr [] [a,i,_e1,e2]     = PrimInline [j| `a`.i3[`i`] = `e2`; |]

genPrim _ _ WriteByteArrayOp_Int8 [] [a,i,e] = PrimInline [j| `a`.dv.setInt8(`i`, `e`, true); |]
genPrim _ _ WriteByteArrayOp_Int16 [] [a,i,e]     = PrimInline [j| `a`.dv.setInt16(`i`<<1, `e`, true); |]
genPrim _ _ WriteByteArrayOp_Int32 [] [a,i,e]     = PrimInline [j| `a`.i3[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Int64 [] [a,i,e1,e2] =
  PrimInline [j| `a`.i3[(`i`<<1)+1] = `e1`;
                 `a`.i3[`i`<<1] = `e2`;
               |]
genPrim _ _ WriteByteArrayOp_Word8 [] [a,i,e]     = PrimInline [j| `a`.u8[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Word16 [] [a,i,e]     = PrimInline [j| `a`.u1[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Word32 [] [a,i,e]     = PrimInline [j| `a`.i3[`i`] = `e`; |]
genPrim _ _ WriteByteArrayOp_Word64 [] [a,i,e1,e2] =
  PrimInline [j| `a`.i3[(`i`<<1)+1] = `e1`;
                 `a`.i3[`i`<<1] = `e2`;
               |]
{- implement new ops in 8.6
                  , WriteByteArrayOp_Word8AsChar
                  , WriteByteArrayOp_Word8AsWideChar
                  , WriteByteArrayOp_Word8AsAddr
                  , WriteByteArrayOp_Word8AsFloat
                  , WriteByteArrayOp_Word8AsDouble
                  , WriteByteArrayOp_Word8AsStablePtr
                  , WriteByteArrayOp_Word8AsInt16
                  , WriteByteArrayOp_Word8AsInt32
                  , WriteByteArrayOp_Word8AsInt64
                  , WriteByteArrayOp_Word8AsInt
                  , WriteByteArrayOp_Word8AsWord16
                  , WriteByteArrayOp_Word8AsWord32
                  , WriteByteArrayOp_Word8AsWord64
                  , WriteByteArrayOp_Word8AsWord
 -}

-- fixme we can do faster by copying 32 bit ints or doubles
genPrim _ _ CopyByteArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline [j| for(var i=`n` - 1; i >= 0; i--) {
                   `a2`.u8[i+`o2`] = `a1`.u8[i+`o1`];
                 }
               |]
genPrim d t CopyMutableByteArrayOp [] xs@[_a1,_o1,_a2,_o2,_n] = genPrim d t CopyByteArrayOp [] xs
genPrim d t CopyByteArrayToAddrOp [] xs@[_a1,_o1,_a2,_o2,_n] = genPrim d t CopyByteArrayOp [] xs
genPrim d t CopyMutableByteArrayToAddrOp [] xs@[_a1,_o1,_a2,_o2,_n] = genPrim d t CopyByteArrayOp [] xs
genPrim d t CopyAddrToByteArrayOp [] xs@[_ba,_bo,_aa,_ao,_n] = genPrim d t CopyByteArrayOp [] xs

genPrim _ _ SetByteArrayOp [] [a,o,n,v] =
  PrimInline [j| for(var i=0;i<`n`;i++) {
                   `a`.u8[`o`+i] = `v`;
                 }
               |]

genPrim _ _ NewArrayArrayOp [r] [n] = PrimInline (newArray r n jnull)
genPrim _ _ SameMutableArrayArrayOp [r] [a1,a2] = PrimInline [j| `r` = (`a1` === `a2`) ? 1 : 0 |]
genPrim _ _ UnsafeFreezeArrayArrayOp [r] [a] = PrimInline [j| `r` = `a` |]
genPrim _ _ SizeofArrayArrayOp [r] [a] = PrimInline [j| `r` = `a`.length; |]
genPrim _ _ SizeofMutableArrayArrayOp [r] [a] = PrimInline [j| `r` = `a`.length; |]
genPrim _ _ IndexArrayArrayOp_ByteArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim _ _ IndexArrayArrayOp_ArrayArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim _ _ ReadArrayArrayOp_ByteArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim _ _ ReadArrayArrayOp_MutableByteArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim _ _ ReadArrayArrayOp_ArrayArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim _ _ ReadArrayArrayOp_MutableArrayArray [r] [a,n] = PrimInline [j| `r` = `a`[`n`] |]
genPrim _ _ WriteArrayArrayOp_ByteArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim _ _ WriteArrayArrayOp_MutableByteArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim _ _ WriteArrayArrayOp_ArrayArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim _ _ WriteArrayArrayOp_MutableArrayArray [] [a,n,v] = PrimInline [j| `a`[`n`] = `v` |]
genPrim _ _ CopyArrayArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline [j| for(var i=0;i<`n`;i++) { `a2`[i+`o2`]=`a1`[i+`o1`]; } |]
genPrim _ _ CopyMutableArrayArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline [j| for(var i=0;i<`n`;i++) { `a2`[i+`o2`]=`a1`[i+`o1`]; } |]

genPrim _ _ AddrAddOp  [a',o'] [a,o,i]     = PrimInline [j| `a'` = `a`; `o'` = `o` + `i`;|]
genPrim _ _ AddrSubOp  [i] [_a1,o1,_a2,o2] = PrimInline [j| `i` = `o1` - `o2` |]
genPrim _ _ AddrRemOp  [r] [_a,o,i]        = PrimInline [j| `r` = `o` % `i` |]
genPrim _ _ Addr2IntOp [i]     [_a,o]      = PrimInline [j| `i` = `o`; |] -- only usable for comparisons within one range
genPrim _ _ Int2AddrOp [a,o]   [i]         = PrimInline [j| `a` = null; `o` = `i`; |] -- unsupported
genPrim _ _ AddrGtOp   [r] [a1,o1,a2,o2] =
  PrimInline [j| `r` = h$comparePointer(`a1`,`o1`,`a2`,`o2`) > 0 ? 1 : 0; |]
genPrim _ _ AddrGeOp   [r] [a1,o1,a2,o2] =
  PrimInline [j| `r` = h$comparePointer(`a1`,`o1`,`a2`,`o2`) >= 0 ? 1 : 0; |]
genPrim _ _ AddrEqOp   [r] [a1,o1,a2,o2]   =
  PrimInline [j| `r` = h$comparePointer(`a1`,`o1`,`a2`,`o2`) === 0 ? 1 : 0; |]
genPrim _ _ AddrNeOp   [r] [a1,o1,a2,o2]   =
  PrimInline [j| `r` = h$comparePointer(`a1`,`o1`,`a2`,`o2`) !== 0 ? 1 : 0; |]
genPrim _ _ AddrLtOp   [r] [a1,o1,a2,o2] =
  PrimInline [j| `r` = h$comparePointer(`a1`,`o1`,`a2`,`o2`) < 0 ? 1 : 0; |]
genPrim _ _ AddrLeOp   [r] [a1,o1,a2,o2] =
  PrimInline [j| `r` = h$comparePointer(`a1`,`o1`,`a2`,`o2`) <= 0 ? 1 : 0; |]

-- addr indexing: unboxed arrays
genPrim _ _ IndexOffAddrOp_Char [c] [a,o,i] = PrimInline [j| `c` = `a`.u8[`o`+`i`]; |]
genPrim _ _ IndexOffAddrOp_WideChar [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getUint32(`o`+(`i`<<2),true); |]

genPrim _ _ IndexOffAddrOp_Int [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ IndexOffAddrOp_Word [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ IndexOffAddrOp_Addr [ca,co] [a,o,i] =
  PrimInline [j| if(`a`.arr && `a`.arr[`o`+(`i`<<2)]) {
                   `ca` = `a`.arr[`o`+(`i`<<2)][0];
                   `co` = `a`.arr[`o`+(`i`<<2)][1];
                 } else {
                   `ca` = null;
                   `co` = 0;
                 }
               |]
genPrim _ _ IndexOffAddrOp_Float [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getFloat32(`o`+(`i`<<2),true); |]
genPrim _ _ IndexOffAddrOp_Double [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getFloat64(`o`+(`i`<<3),true); |]
genPrim _ _ IndexOffAddrOp_StablePtr [c1,c2] [a,o,i] = PrimInline [j| `c1` = h$stablePtrBuf; `c2` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ IndexOffAddrOp_Int8 [c] [a,o,i] = PrimInline [j| `c` = `a`.u8[`o`+`i`]; |]
genPrim _ _ IndexOffAddrOp_Int16 [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt16(`o`+(`i`<<1),true); |]
genPrim _ _ IndexOffAddrOp_Int32 [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ IndexOffAddrOp_Int64 [c1,c2] [a,o,i] =
   PrimInline [j| `c1` = `a`.dv.getInt32(`o`+(`i`<<3)+4,true);
                  `c2` = `a`.dv.getInt32(`o`+(`i`<<3),true);
                |]
genPrim _ _ IndexOffAddrOp_Word8 [c] [a,o,i] = PrimInline [j| `c` = `a`.u8[`o`+`i`]; |]
genPrim _ _ IndexOffAddrOp_Word16 [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getUint16(`o`+(`i`<<1),true); |]
genPrim _ _ IndexOffAddrOp_Word32 [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ IndexOffAddrOp_Word64 [c1,c2] [a,o,i] =
   PrimInline [j| `c1` = `a`.dv.getInt32(`o`+(`i`<<3)+4,true);
                  `c2` = `a`.dv.getInt32(`o`+(`i`<<3),true);
                |]
genPrim _ _ ReadOffAddrOp_Char [c] [a,o,i] =
   PrimInline [j| `c` = `a`.u8[`o`+`i`]; |]
genPrim _ _ ReadOffAddrOp_WideChar [c] [a,o,i] =
   PrimInline [j| `c` = `a`.dv.getUint32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Int [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Word [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Addr [c1,c2] [a,o,i] =
  PrimInline [j| var x = `i`<<2;
                 if(`a`.arr && `a`.arr[`o`+ x]) {
                   `c1` = `a`.arr[`o`+ x][0];
                   `c2` = `a`.arr[`o`+ x][1];
                 } else {
                   `c1` = null;
                   `c2` = 0;
                 }
               |]
genPrim _ _ ReadOffAddrOp_Float [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getFloat32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Double [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getFloat64(`o`+(`i`<<3),true); |]
genPrim _ _ ReadOffAddrOp_StablePtr [c1,c2] [a,o,i] = PrimInline [j| `c1` = h$stablePtrBuf; `c2` = `a`.dv.getUint32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Int8   [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt8(`o`+`i`); |]
genPrim _ _ ReadOffAddrOp_Int16  [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt16(`o`+(`i`<<1),true); |]
genPrim _ _ ReadOffAddrOp_Int32  [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Int64  [c1,c2] [a,o,i] =
  PrimInline [j| `c1` = `a`.dv.getInt32(`o`+(`i`<<3)+4,true);
                 `c2` = `a`.dv.getInt32(`o`+(`i`<<3),true);
               |]
genPrim _ _ ReadOffAddrOp_Word8  [c] [a,o,i] = PrimInline [j| `c` = `a`.u8[`o`+`i`]; |]
genPrim _ _ ReadOffAddrOp_Word16 [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getUint16(`o`+(`i`<<1),true); |]
genPrim _ _ ReadOffAddrOp_Word32 [c] [a,o,i] = PrimInline [j| `c` = `a`.dv.getInt32(`o`+(`i`<<2),true); |]
genPrim _ _ ReadOffAddrOp_Word64 [c1,c2] [a,o,i] =
   PrimInline [j| `c1` = `a`.dv.getInt32(`o`+(`i`<<3)+4,true);
                  `c2` = `a`.dv.getInt32(`o`+(`i`<<3),true);
                |]
genPrim _ _ WriteOffAddrOp_Char [] [a,o,i,v]     = PrimInline [j| `a`.u8[`o`+`i`] = `v`; |]
genPrim _ _ WriteOffAddrOp_WideChar [] [a,o,i,v] = PrimInline [j| `a`.dv.setUint32(`o`+(`i`<<2), `v`,true); |]
genPrim _ _ WriteOffAddrOp_Int [] [a,o,i,v]     = PrimInline [j| `a`.dv.setInt32(`o`+(`i`<<2), `v`,true); |]
genPrim _ _ WriteOffAddrOp_Word [] [a,o,i,v]    = PrimInline [j| `a`.dv.setInt32(`o`+(`i`<<2), `v`,true); |]
genPrim _ _ WriteOffAddrOp_Addr [] [a,o,i,va,vo] =
  PrimInline [j| if(!`a`.arr) { `a`.arr = []; }
                 `a`.arr[`o`+(`i`<<2)] = [`va`,`vo`];
               |]
genPrim _ _ WriteOffAddrOp_Float [] [a,o,i,v]   = PrimInline [j| `a`.dv.setFloat32(`o`+(`i`<<2), `v`,true); |]
genPrim _ _ WriteOffAddrOp_Double [] [a,o,i,v]  = PrimInline [j| `a`.dv.setFloat64(`o`+(`i`<<3),`v`,true); |]
genPrim _ _ WriteOffAddrOp_StablePtr [] [a,o,i,_v1,v2]  = PrimInline [j| `a`.dv.setUint32(`o`+(`i`<<2), `v2`, true); |]
genPrim _ _ WriteOffAddrOp_Int8 [] [a,o,i,v]    = PrimInline [j| `a`.dv.setInt8(`o`+`i`, `v`); |]
genPrim _ _ WriteOffAddrOp_Int16 [] [a,o,i,v]   = PrimInline [j| `a`.dv.setInt16(`o`+(`i`<<1), `v`, true); |]
genPrim _ _ WriteOffAddrOp_Int32 [] [a,o,i,v]   = PrimInline [j| `a`.dv.setInt32(`o`+(`i`<<2), `v`, true); |]
genPrim _ _ WriteOffAddrOp_Int64 [] [a,o,i,v1,v2] = PrimInline [j| `a`.dv.setInt32(`o`+(`i`<<3)+4, `v1`, true);
                                                                   `a`.dv.setInt32(`o`+(`i`<<3), `v2`, true);
                                                                 |]
genPrim _ _ WriteOffAddrOp_Word8 [] [a,o,i,v]   = PrimInline [j| `a`.u8[`o`+`i`] = `v`; |]
genPrim _ _ WriteOffAddrOp_Word16 [] [a,o,i,v]  = PrimInline [j| `a`.dv.setUint16(`o`+(`i`<<1), `v`, true); |]
genPrim _ _ WriteOffAddrOp_Word32 [] [a,o,i,v]  = PrimInline [j| `a`.dv.setInt32(`o`+(`i`<<2), `v`, true); |]
genPrim _ _ WriteOffAddrOp_Word64 [] [a,o,i,v1,v2] = PrimInline [j| `a`.dv.setInt32(`o`+(`i`<<3)+4, `v1`, true);
                                                                    `a`.dv.setInt32(`o`+(`i`<<3), `v2`, true);
                                                                  |]
genPrim _ _ NewMutVarOp       [r] [x]   = PrimInline [j| `r` = new h$MutVar(`x`);  |]
genPrim _ _ ReadMutVarOp      [r] [m]   = PrimInline [j| `r` = `m`.val; |]
genPrim _ _ WriteMutVarOp     [] [m,x]  = PrimInline [j| `m`.val = `x`; |]
genPrim _ _ SameMutVarOp      [r] [x,y] =
  PrimInline [j| `r` = (`x` === `y`) ? 1 : 0; |]
genPrim _ _ AtomicModifyMutVarOp [r] [m,f] =
  PrimInline [j| `r` = h$atomicModifyMutVar(`m`,`f`); |]
genPrim _ _ CasMutVarOp [status,r] [mv,o,n] =
  PrimInline [j| if(`mv`.val === `o`) {
                    `status` = 0;
                    `r` = `n`; `mv`.val = `n`;
                 } else {
                    `status` = 1;
                    `r` = `mv`.val;
                 }
               |]
genPrim _ _ CatchOp [_r] [a,handler] = PRPrimCall
  [j| return h$catch(`a`, `handler`); |]
genPrim _ _ RaiseOp         [_r] [a] = PRPrimCall [j| return h$throw(`a`,false); |]
genPrim _ _ RaiseIOOp       [_r] [a] = PRPrimCall [j| return h$throw(`a`,false); |]

genPrim _ _ MaskAsyncExceptionsOp [_r] [a] =
  PRPrimCall [j| return h$maskAsync(`a`); |]
genPrim _ _ MaskUninterruptibleOp [_r] [a] =
  PRPrimCall [j| return h$maskUnintAsync(`a`); |]
genPrim _ _ UnmaskAsyncExceptionsOp [_r] [a] =
  PRPrimCall [j| return h$unmaskAsync(`a`); |]

genPrim _ _ MaskStatus [r] [] = PrimInline [j| `r` = h$maskStatus(); |]

genPrim _ _ AtomicallyOp [_r] [a] = PRPrimCall [j| return h$atomically(`a`); |]
genPrim _ _ RetryOp [_r] [] = PRPrimCall [j| return h$stmRetry(); |]
genPrim _ _ CatchRetryOp [_r] [a,b] = PRPrimCall [j| return h$stmCatchRetry(`a`,`b`); |]
genPrim _ _ CatchSTMOp [_r] [a,h] = PRPrimCall [j| return h$catchStm(`a`,`h`); |]
genPrim _ _ NewTVarOp [tv] [v] = PrimInline [j| `tv` = h$newTVar(`v`); |]
genPrim _ _ ReadTVarOp [r] [tv] = PrimInline [j| `r` = h$readTVar(`tv`); |]
genPrim _ _ ReadTVarIOOp [r] [tv] = PrimInline [j| `r` = h$readTVarIO(`tv`); |]
genPrim _ _ WriteTVarOp [] [tv,v] = PrimInline [j| h$writeTVar(`tv`,`v`); |]
genPrim _ _ SameTVarOp [r] [tv1,tv2] = PrimInline [j| `r` = h$sameTVar(`tv1`,`tv2`) ? 1 : 0; |]

genPrim _ _ NewMVarOp [r] []   = PrimInline [j| `r` = new h$MVar(); |]
genPrim _ _ TakeMVarOp [_r] [m] = PRPrimCall [j| return h$takeMVar(`m`); |]
genPrim _ _ TryTakeMVarOp [r,v] [m] = PrimInline [j| `r` = h$tryTakeMVar(`m`);
                                                     `v` = `Ret1`;
                                                   |]
genPrim _ _ PutMVarOp [] [m,v] = PRPrimCall [j| return h$putMVar(`m`,`v`); |]
genPrim _ _ TryPutMVarOp [r] [m,v] = PrimInline [j| `r` = h$tryPutMVar(`m`,`v`) |]
genPrim _ _ ReadMVarOp [_r] [m] = PRPrimCall [j| return h$readMVar(`m`); |]
genPrim _ _ TryReadMVarOp [r,v] [m] = PrimInline [j| `v` = `m`.val;
                                                     `r` = (`v`===null) ? 0 : 1;
                                                   |]
genPrim _ _ SameMVarOp [r] [m1,m2] =
   PrimInline [j| `r` = (`m1` === `m2`) ? 1 : 0; |]
genPrim _ _ IsEmptyMVarOp [r] [m]  =
  PrimInline [j| `r` = (`m`.val === null) ? 1 : 0; |]

genPrim _ _ DelayOp [] [t] = PRPrimCall [j| return h$delayThread(`t`); |]
genPrim _ _ WaitReadOp [] [fd] = PRPrimCall [j| return h$waitRead(`fd`); |]
genPrim _ _ WaitWriteOp [] [fd] = PRPrimCall [j| return h$waitWrite(`fd`); |]
genPrim _ _ ForkOp [_tid] [x] = PRPrimCall [j| return h$fork(`x`, true); |]
genPrim _ _ ForkOnOp [_tid] [_p,x] = PRPrimCall [j| return h$fork(`x`, true); |] -- ignore processor argument
genPrim _ _ KillThreadOp [] [tid,ex] =
  PRPrimCall [j| return h$killThread(`tid`,`ex`); |]
genPrim _ _ YieldOp [] [] = PRPrimCall [j| return h$yield(); |]
genPrim _ _ MyThreadIdOp [r] [] = PrimInline [j| `r` = h$currentThread; |]
genPrim _ _ LabelThreadOp [] [t,la,lo] = PrimInline [j| `t`.label = [`la`,`lo`]; |]
genPrim _ _ IsCurrentThreadBoundOp [r] [] = PrimInline [j| `r` = 1; |]
genPrim _ _ NoDuplicateOp [] [] = PrimInline mempty -- don't need to do anything as long as we have eager blackholing
genPrim _ _ ThreadStatusOp [stat,cap,locked] [tid] = PrimInline
  [j| `stat` = h$threadStatus(`tid`);
      `cap` = `Ret1`;
      `locked` = `Ret2`;
    |]
genPrim _ _ MkWeakOp [r] [o,b,c] = PrimInline [j| `r` = h$makeWeak(`o`,`b`,`c`); |]
genPrim _ _ MkWeakNoFinalizerOp [r] [o,b] = PrimInline [j| `r` = h$makeWeakNoFinalizer(`o`,`b`); |]
genPrim _ _ AddCFinalizerToWeakOp [r] [_a1,_a1o,_a2,_a2o,_i,_a3,_a3o,_w] =
  PrimInline [j| `r` = 1; |]
genPrim _ _ DeRefWeakOp        [f,v] [w] = PrimInline [j| `v` = `w`.val;
                                                          `f` = (`v`===null) ? 0 : 1;
                                                        |]
genPrim _ _ FinalizeWeakOp     [fl,fin] [w] =
  PrimInline [j| `fin` = h$finalizeWeak(`w`);
                 `fl`  = `Ret1`;
               |]
genPrim _ _ TouchOp [] [_e] = PrimInline mempty -- fixme what to do?

genPrim _ _ MakeStablePtrOp [s1,s2] [a] = PrimInline [j| `s1` = h$stablePtrBuf; `s2` = h$makeStablePtr(`a`); |]
genPrim _ _ DeRefStablePtrOp [r] [_s1,s2] = PrimInline [j| `r` = h$deRefStablePtr(`s2`); |]
genPrim _ _ EqStablePtrOp [r] [_sa1,sa2,_sb1,sb2] = PrimInline [j| `r` = (`sa2` === `sb2`) ? 1 : 0; |]

genPrim _ _ MakeStableNameOp [r] [a] = PrimInline [j| `r` = h$makeStableName(`a`); |]
genPrim _ _ EqStableNameOp [r] [s1,s2] = PrimInline [j| `r` = h$eqStableName(`s1`, `s2`); |]
genPrim _ _ StableNameToIntOp [r] [s] = PrimInline [j| `r` = h$stableNameInt(`s`); |]

genPrim _ _ CompactNewOp [c] [s] =
  PrimInline [j| `c` = h$compactNew(`s`); |]
genPrim _ _ CompactResizeOp [] [s] =
  PrimInline [j| h$compactResize(`s`); |]
genPrim _ _ CompactContainsOp [r] [c,v] =
  PrimInline [j| `r` = h$compactContains(`c`, `v`); |]
genPrim _ _ CompactContainsAnyOp [r] [v] =
  PrimInline [j| `r` = h$compactContainsAny(`v`); |]
genPrim _ _ CompactGetFirstBlockOp [ra,ro,s] [c] =
  PrimInline [j| `ra` = h$compactGetFirstBlock(`c`);
                 `ro` = `Ret1`;
                 `s`  = `Ret2`;
               |]
genPrim _ _ CompactGetNextBlockOp [ra,ro,s] [c,a,o] =
  PrimInline [j| `ra` = h$compactGetNextBlock(`c`,`a`,`o`);
                 `ro` = `Ret1`;
                 `s`  = `Ret2`;
               |]
genPrim _ _ CompactAllocateBlockOp [ra,ro] [size,sa,so] =
  PrimInline [j| `ra` = h$compactAllocateBlock(`size`,`sa`,`so`);
                 `ro` = `Ret1`;
               |]
genPrim _ _ CompactFixupPointersOp [newroota, newrooto] [blocka,blocko,roota,rooto] =
  PrimInline [j| `newroota` = h$compactFixupPointers(`blocka`,`blocko`,`roota`,`rooto`);
                 `newrooto` = `Ret1`;
               |]
genPrim _ _ CompactAdd [_r] [c,o] =
  PRPrimCall [j| return h$compactAdd(`c`,`o`); |]
genPrim _ _ CompactAddWithSharing [_r] [c,o] =
  PRPrimCall [j| return h$compactAddWithSharing(`c`,`o`); |]
genPrim _ _ CompactSize [s] [c] =
  PrimInline [j| `s` = h$compactSize(`c`); |]

genPrim _ _ ReallyUnsafePtrEqualityOp [r] [p1,p2] = PrimInline [j| `r` = `p1`===`p2`?1:0; |]
genPrim _ _ ParOp [r] [_a] = PrimInline [j| `r` = 0; |]
genPrim _ _ SparkOp [r] [a] = PrimInline [j| `r` = `a`; |]
genPrim _ _ SeqOp [_r] [e] = PRPrimCall [j| return h$e(`e`); |]
{-
GetSparkOp
-}
genPrim _ _ NumSparks [r] [] = PrimInline [j| `r` = 0 |]
{-
ParGlobalOp
ParLocalOp
ParAtOp
ParAtAbsOp
ParAtRelOp
ParAtForNowOp
CopyableOp
NoFollowOp
-}
{-
PrefetchByteArrayOp3
PrefetchMutableByteArrayOp3
PrefetchAddrOp3
PrefetchValueOp3
PrefetchByteArrayOp2
PrefetchMutableByteArrayOp2
PrefetchAddrOp2
PrefetchValueOp2
PrefetchByteArrayOp1
PrefetchMutableByteArrayOp1
PrefetchAddrOp1
PrefetchValueOp1

-}
genPrim _ t DataToTagOp [_r] [d] = PRPrimCall
  [j| `Stack`[++`Sp`] = h$dataToTag_e;
      return h$e(`d`);
    |]
genPrim _ t TagToEnumOp [r] [tag]
  | isBoolTy t = PrimInline [j| `r` = `tag`?true:false;  |]
  | otherwise  = PrimInline [j| `r` = h$tagToEnum(`tag`) |]
genPrim _ _ AddrToAnyOp [r] [d,_o] = PrimInline [j| `r` = `d`; |]
{-
MkApUpd0_Op
NewBCOOp
UnpackClosureOp
GetApStackValOp
-}

-- stack trace/cost-centre operations
genPrim d _ GetCurrentCCSOp [a, o] [_dummy_arg] =
  let ptr = if buildingProf d then [je| h$buildCCSPtr(`jCurrentCCS`) |]
                              else jnull
  in PrimInline [j| `a` = `ptr`; `o` = 0; |]
genPrim d _ GetCCSOfOp [a, o] [obj]
  | buildingProf d =
      PrimInline [j| if (typeof(`obj`) === "object") {
                       `a` = h$buildCCSPtr(`obj`.cc); `o` = 0;
                     } else {
                       `a` = null; `o` = 0;
                     }
                   |]
  | otherwise = PrimInline [j| `a` = null; `o` = 0; |]

genPrim _ _ TraceEventOp [] [ed,eo] = PrimInline [j| h$traceEvent(`ed`,`eo`); |]
genPrim _ _ TraceMarkerOp [] [ed,eo] = PrimInline [j| h$traceMarker(`ed`, `eo`); |]
{- new ops in 8.6
  , GetThreadAllocationCounter
  , SetThreadAllocationCounter
 -}
genPrim _ _ CasArrayOp                 [s,o] [a,i,old,new] = PrimInline [j| var x = `a`[`i`]; if(x === `old`) { `o` = `new`; `a`[`i`] = `new`; `s` = 0; } else { `s` = 1; `o` = x; } |]
genPrim _ _ NewSmallArrayOp            [a]   [n,e]         = PrimInline [j| `a` = new Array(`n`); for(var i=0;i<`n`;i++) { `a`[i] = `e`; }; `a`.m = 0; `a`.__ghcjsArray = true; |]
genPrim _ _ SameSmallMutableArrayOp    [r]   [x,y]         = PrimInline [j| `r` = (`x` === `y`) ? 1 : 0; |]
genPrim _ _ ReadSmallArrayOp           [r]   [a,i]         = PrimInline [j| `r` = `a`[`i`]; |]
genPrim _ _ WriteSmallArrayOp          []    [a,i,e]       = PrimInline [j| `a`[`i`] = `e`; |]
genPrim _ _ SizeofSmallArrayOp         [r]   [a]           = PrimInline [j| `r` = `a`.length; |]
genPrim _ _ SizeofSmallMutableArrayOp  [r]   [a]           = PrimInline [j| `r` = `a`.length; |]
genPrim _ _ IndexSmallArrayOp          [r]   [a,i]         = PrimInline [j| `r` = `a`[`i`]; |]
genPrim _ _ UnsafeFreezeSmallArrayOp   [r]   [a]           = PrimInline [j| `r` = `a`; |]
genPrim _ _ UnsafeThawSmallArrayOp     [r]   [a]           = PrimInline [j| `r` = `a`; |]
genPrim _ _ CopySmallArrayOp           []    [s,si,d,di,n] = PrimInline [j| for(var i=`n`-1;i>=0;i--) { `d`[`di`+i] = `s`[`si`+i]; } |]
genPrim _ _ CopySmallMutableArrayOp    []    [s,si,d,di,n] = PrimInline [j| for(var i=`n`-1;i>=0;i--) { `d`[`di`+i] = `s`[`si`+i]; } |]
genPrim _ _ CloneSmallArrayOp          [r]   [a,o,n]       = PrimInline [j| `r` = `a`.slice(`o`,`o`+`n`); `r`.m = 0; `r`.__ghcjsArray = true; |]
genPrim _ _ CloneSmallMutableArrayOp   [r]   [a,o,n]       = PrimInline [j| `r` = `a`.slice(`o`,`o`+`n`); `r`.m = 0; `r`.__ghcjsArray = true; |]
genPrim _ _ FreezeSmallArrayOp         [r]   [a,o,n]       = PrimInline [j| `r` = `a`.slice(`o`,`o`+`n`); `r`.m = 0; `r`.__ghcjsArray = true; |]
genPrim _ _ ThawSmallArrayOp           [r]   [a,o,n]       = PrimInline [j| `r` = `a`.slice(`o`,`o`+`n`); `r`.m = 0; `r`.__ghcjsArray = true; |]
genPrim _ _ CasSmallArrayOp            [s,o] [a,i,old,new] = PrimInline [j| var x = `a`[`i`]; if(x === `old`) { `o` = `new`; `a`[`i`] = `new`; `s` = 0; } else { `s` = 1; `o` = x; } |]

genPrim _ _ AtomicReadByteArrayOp_Int  [r]   [a,i]         = PrimInline [j| `r` = `a`.i3[`i`]; |]
genPrim _ _ AtomicWriteByteArrayOp_Int []    [a,i,v]       = PrimInline [j| `a`.i3[`i`] = `v`; |]
genPrim _ _ CasByteArrayOp_Int         [r]   [a,i,old,new] = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; if(t === `old`) { `a`.i3[`i`] = `new`; } |]
genPrim _ _ FetchAddByteArrayOp_Int    [r]   [a,i,v]       = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; `a`.i3[`i`] = t+`v`; |]
genPrim _ _ FetchSubByteArrayOp_Int    [r]   [a,i,v]       = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; `a`.i3[`i`] = t-`v`; |]
genPrim _ _ FetchAndByteArrayOp_Int    [r]   [a,i,v]       = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; `a`.i3[`i`] = t&`v`; |]
genPrim _ _ FetchOrByteArrayOp_Int     [r]   [a,i,v]       = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; `a`.i3[`i`] = (t|`v`); |]
genPrim _ _ FetchNandByteArrayOp_Int   [r]   [a,i,v]       = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; `a`.i3[`i`] = ~(t&`v`); |]
genPrim _ _ FetchXorByteArrayOp_Int    [r]   [a,i,v]       = PrimInline [j| var t = `a`.i3[`i`]; `r` = t; `a`.i3[`i`] = t^`v`; |]

genPrim _ _ ClzOp                      [r]   [x]           = PrimInline [j| `r` = h$clz32(`x`);       |]
genPrim _ _ Clz8Op                     [r]   [x]           = PrimInline [j| `r` = h$clz8(`x`);        |]
genPrim _ _ Clz16Op                    [r]   [x]           = PrimInline [j| `r` = h$clz16(`x`);       |]
genPrim _ _ Clz32Op                    [r]   [x]           = PrimInline [j| `r` = h$clz32(`x`);       |]
genPrim _ _ Clz64Op                    [r]   [x1,x2]       = PrimInline [j| `r` = h$clz64(`x1`,`x2`); |]

genPrim _ _ CtzOp                      [r]   [x]           = PrimInline [j| `r` = h$ctz32(`x`);       |]
genPrim _ _ Ctz8Op                     [r]   [x]           = PrimInline [j| `r` = h$ctz8(`x`);        |]
genPrim _ _ Ctz16Op                    [r]   [x]           = PrimInline [j| `r` = h$ctz16(`x`);       |]
genPrim _ _ Ctz32Op                    [r]   [x]           = PrimInline [j| `r` = h$ctz32(`x`);       |]
genPrim _ _ Ctz64Op                    [r]   [x1,x2]       = PrimInline [j| `r` = h$ctz64(`x1`,`x2`); |]

-- genPrim _ _ op rs as = PrimInline [j| throw `"unhandled primop: " ++ show op ++ " " ++ show (length rs, length as)`; |]

genPrim _ _ op rs as = PrimInline [j| h$log(`"warning, unhandled primop: "++show op++" "++show (length rs, length as)`);
  `f`;
  `copyRes`;
|]
  where
    f = ApplStat (iex . TxtI . T.pack $ "h$primop_" ++ show op) as
    copyRes = mconcat $ zipWith (\r reg -> [j| `r` = `reg`; |]) rs (enumFrom Ret1)

newByteArray :: JExpr -> JExpr -> JStat
newByteArray tgt len = [j| `tgt` = h$newByteArray(`len`); |]

newArray :: JExpr -> JExpr -> JExpr -> JStat
newArray tgt len elem = [j| `tgt` = h$newArray(`len`,`elem`); |]

two_24 :: Int
two_24 = 2^(24::Int)
