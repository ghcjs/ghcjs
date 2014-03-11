{-# LANGUAGE QuasiQuotes,
             OverloadedStrings #-}

{-
  generate various apply functions for the RTS, for speeding up
  function application in the most common cases. The code is generated
  because it contains lots of repeating patterns, and to make it more
  flexible when changing the RTS (for example how arguments are passed)

  The code in here can be a bit hard to read due to all the generated
  low-level access things. Reading rts.js for a compiled program can be
  easier (the file is always the same unless you change low-level RTS
  options)

  - fixme: add selector thunks and let the gc follow them
-}

module Gen2.RtsApply where

import           Compiler.JMacro

import           Gen2.RtsAlloc
import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils
import           Gen2.ClosureInfo

import           Data.Bits
import           Data.List                        (find, foldl', sort)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

t :: T.Text -> T.Text
t = id

rtsApply :: JStat
rtsApply = mconcat $  map (uncurry stackApply) applySpec
                   ++ map (uncurry fastApply) applySpec
                   ++ map pap specPap
                   ++ [ mkApplyArr
                      , genericStackApply
                      , genericFastApply
                      , zeroApply
                      , updates
                      , papGen
                      , moveRegs2
                      ]

-- specialized apply for these
-- make sure that once you are in spec, you stay there
applySpec :: [(Int,Int)] -- regs,arity
applySpec = [ (regs,arity)  | arity <- [1..4], regs <- [max 0 (arity-1)..(arity*2)]]

specApply :: Bool -> Int -> Int -> Maybe JExpr
specApply fast n r
  | (r,n) == (0,0)         = Just (toJExpr . TxtI . T.pack $ ("h$ap_0_0" ++ fastSuff))
  | (r,n) == (0,1)         = Just (toJExpr . TxtI . T.pack $ ("h$ap_1_0" ++ fastSuff))
  | (r,n) `elem` applySpec =
        Just (toJExpr . TxtI . T.pack $ ("h$ap_" ++ show n ++ "_" ++ show r ++ fastSuff))
  | otherwise = Nothing
   where
      fastSuff | fast      = "_fast"
               | otherwise = ""
{-
  Build arrays to quickly lookup apply functions, getting the fast variant when possible
   - h$apply[r << 8 | n] = function application for r regs, n args
   - h$paps[r]           = partial application for r registers (number of args is in the object)
 -}
mkApplyArr =
  [j| var !h$apply = [];
      var !h$paps = [];
      h$initStatic.push(function() {
        var i;
        for(i=0;i<65536;i++) {
          h$apply[i] = h$ap_gen;
        }
        for(i=0;i<128;i++) {
          h$paps[i] = h$pap_gen;
        }
        h$apply[0] = h$ap_0_0;
        h$apply[1] = h$ap_1_0;
        `map assignSpec applySpec`;
        `map assignPap specPap`;
      });
    |]
  where
    assignSpec (r,n) =
      [j| h$apply[`shiftL r 8 .|. n`] = `TxtI . T.pack $ ("h$ap_" ++ show n ++ "_" ++ show r)`; |]
    assignPap p =
      [j| h$paps[`p`] = `TxtI . T.pack $ ("h$pap_" ++ show p)`; |]

-- generic stack apply that can do everything, but less efficiently
-- on stack: tag: (regs << 8 | arity)
-- fixme: set closure info of stack frame
genericStackApply :: JStat
genericStackApply =
  [j| fun h$ap_gen {
        `traceRts $ t"h$ap_gen"`;
        var c = `R1`.f;
        switch(c.t) {
          case `Thunk`:
            return c;
          case `Fun`:
            var farity = `funArity' c`;
            `funCase c farity`;
          case `Pap`:
            var parity;
            `papArity parity (toJExpr R1)`;
            `funCase c parity`;
          case `Blackhole`:
            `push' [toJExpr R1, jsv "h$return"]`;
            return h$blockOnBlackhole(`R1`);
          default:
            throw "h$ap_gen: unexpected closure type";
        }
      }
      `ClosureInfo "h$ap_gen" (CIRegs 0 [PtrV]) "h$ap_gen" CILayoutVariable CIStackFrame noStatic`;
    |]
  where
    funCase c arity = withIdent $ \pap ->
      [j| var myArity = `Stack`[`Sp`-1];
          var ar = `arity` & 0xFF;
          var myAr = myArity & 0xFF;
          var myRegs = myArity >> 8;
          `traceRts $ t"h$ap_gen: args: " |+ myAr |+ t" regs: " |+ myRegs`;
          if(myAr == ar) {
            `traceRts $ t"h$ap_gen: exact"`;
            for(var i=0;i<myRegs;i++) {
              h$setReg(i+2,`Stack`[`Sp`-2-i]);
            }
            `Sp` = `Sp` - myRegs - 2;
            return `c`;
          } else if(myAr > ar) {
            var regs = `arity` >> 8;
            `traceRts $ t"h$ap_gen: oversat: arity: " |+ ar |+ t" regs: " |+ regs`;
            for(var i=0;i<regs;i++) {
              `traceRts $ t"h$ap_gen: loading register: " |+ i`;
              h$setReg(i+2,`Stack`[`Sp`-2-i]);
            }
            var newTag = ((myRegs-regs)<<8)|(myAr-ar);
            var newAp = h$apply[newTag];
            `traceRts $ t"h$ap_gen: next: " |+ (newAp|."n")`;
            if(newAp === h$ap_gen) {
              `Sp` = `Sp` - regs;
              `Stack`[`Sp`-1] = newTag;
            } else {
              `Sp` = `Sp` - regs - 1;
            }
            `Stack`[`Sp`] = newAp;
            return `c`;
          } else {
            `traceRts $ t"h$ap_gen: undersat"`;
            var p = h$paps[myRegs];
            var dat = [`R1`,myAr];
            for(var i=0;i<myRegs;i++) {
               dat.push(`Stack`[`Sp`-i-1]);
            }
            `Sp` = `Sp` - myRegs - 2;
            `R1` = h$init_closure(p, dat);
            return `Stack`[`Sp`];
          }
        |]
{-
  generic fast apply: can handle anything (slowly)
  signature tag in argument
-}
genericFastApply :: JStat
genericFastApply =
  [j| fun h$ap_gen_fast tag {
        `traceRts $ t"h$ap_gen_fast: " |+ tag`;
        var c = `R1`.f;
        switch(c.t) {
          case `Thunk`:
            `traceRts $ t"h$ap_gen_fast: thunk"`;
            `pushStackApply c tag`;
            return c;
          case `Fun`:
            var farity = `funArity' c`;
            `traceRts $ t"h$ap_gen_fast: fun " |+ farity`;
            `funCase c tag farity`;
          case `Pap`:
            var parity;
            `papArity parity (toJExpr R1)`;
            `traceRts $ t"h$ap_gen_fast: pap " |+ parity`;
            `funCase c tag parity`;
          case `Con`:
            `traceRts $ t"h$ap_gen_fast: con"`;
            if(tag != 0) { throw "h$ap_gen_fast: invalid apply"; }
            return c;
          case `Blackhole`:
            `traceRts $ t"h$ap_gen_fast: blackhole"`;
            `pushStackApply c tag`;
            `push' [toJExpr R1, jsv "h$return"]`;
            return h$blockOnBlackhole(`R1`);
          default:
            throw ("h$ap_gen_fast: unexpected closure type: " + c.t);
        }
      }
    |]
  where
     -- thunk: push everything to stack frame, enter thunk first
    pushStackApply :: JExpr -> JExpr -> JStat
    pushStackApply c tag =
      [j| `pushAllRegs tag`;
          var ap = h$apply[`tag`];
          if(ap === h$ap_gen) {
            `Sp` = `Sp` + 2;
            `Stack`[`Sp`-1] = `tag`;
          } else {
            `Sp` = `Sp` + 1;
          }
          `Stack`[`Sp`] = ap;

        |]
    funCase :: JExpr -> JExpr -> JExpr -> JStat
    funCase c tag arity =
      [j| var ar = `arity` & 0xFF;
          var myAr = `tag` & 0xFF;
          var myRegs = `tag` >> 8;
          `traceRts $ t"h$ap_gen_fast: args: " |+ myAr |+ t" regs: " |+ myRegs`;
          if(myAr === ar) {
            // call the function directly
            `traceRts $ t"h$ap_gen_fast: exact"`;
            return `c`;
          } else if(myAr > ar) {
            // push stack frame with remaining args, then call fun
            `traceRts $ t"h$ap_gen_fast: oversat " |+ Sp`;
            var regsStart = (`arity` >> 8)+1;
            `Sp` = `Sp` + myRegs - regsStart + 1;
            `traceRts $ t"h$ap_gen_fast: oversat " |+ Sp`;
            `pushArgs regsStart myRegs`;
            var newTag = (((myRegs-(`arity`>>8))<<8))|(myAr-ar);
            var newAp = h$apply[newTag];
            if(newAp === h$ap_gen) {
              `Sp` = `Sp` + 2;
              `Stack`[`Sp`-1] = newTag;
            } else {
              `Sp` = `Sp` + 1;
            }
            `Stack`[`Sp`] = newAp;
            return `c`;
          } else {
            `traceRts $ t"h$ap_gen_fast: undersat: " |+ myRegs |+ t" " |+ tag`; // build PAP and return stack top
            if(`tag` != 0) {
              var p = h$paps[myRegs];
              `traceRts $ t"h$ap_gen_fast: got pap: " |+ (p|."n")`;
              var dat = [`R1`,myAr];
              for(var i=0;i<myRegs;i++) {
                dat.push(h$getReg(i+2));
              }
              `R1` = { f: p, d1: null, d2: null, m: 0 };
              h$init_closure(`R1`, dat);
            }
            return `Stack`[`Sp`];
          }
        |]
    pushAllRegs :: JExpr -> JStat
    pushAllRegs tag =
      [j| var regs = `tag` >> 8;
          `Sp` = `Sp` + regs;
          `SwitchStat regs (map pushReg [65,64..2]) mempty`;
        |]
      where
        pushReg r = (toJExpr (r-1), [j| `Stack`[`Sp`-`r-2`] = `numReg r`; |])

    pushArgs :: JExpr -> JExpr -> JStat
    pushArgs start end =
      [j| for(var i=`end`;i>=`start`;i--) {
             `traceRts $ ((t"pushing register: " |+ i)::JExpr)`;
             `Stack`[`Sp`+`start`-i] = h$getReg(i+1);
          }
        |]


stackApply :: Int -> -- ^ number of registers in stack frame
              Int -> -- ^ number of arguments
              JStat
stackApply r n = [j| `decl func`;
                     `JVar func` = `JFunc funArgs (preamble <> body)`;
                     `ClosureInfo funcName (CIRegs 0 [PtrV]) funcName layout CIStackFrame noStatic`;
                   |]
  where
    layout    = CILayoutUnknown r

    frameSize = r+1

    funcName = T.pack ("h$ap_" ++ show n ++ "_" ++ show r)

    popFrame = adjSpN frameSize

    func = TxtI funcName
    body = [j| var c = `R1`.f;
               `traceRts $ funcName |+ t" " |+ (c|."n") |+ t" sp: " |+ Sp |+ t" a: " |+ (c|."a")`;
               switch(c.t) {
                 case `Thunk`:
                   `traceRts $ funcName <> ": thunk"`;
                   return c;
                 case `Fun`:
                   `traceRts $ funcName <> ": fun"`;
                   `funCase c`;
                 case `Pap`:
                   `traceRts $ funcName <> ": pap"`;
                   `papCase c`;
                 case `Blackhole`:
                   `push' [toJExpr R1, jsv "h$return"]`;
                   return h$blockOnBlackhole(`R1`);
                 default:
                   throw (`"panic: " <> funcName <> ", unexpected closure type: "` + c.t);
               }
           |]

    funExact c = popSkip' 1 (reverse $ take r (map toJExpr $ enumFrom R2))
                 <> [j| return `c`; |]
    stackArgs = map (\x -> [je| `Stack`[`Sp`-`x`] |]) [1..r]

    papCase :: JExpr -> JStat
    papCase c = withIdent $ \pap ->
                [j| var arity0;
                    `papArity arity0 (toJExpr R1)`;
                    var arity = arity0 & 0xFF;
                    `traceRts $ (funcName <> ": found pap, arity: ") |+ arity0`;
                    if(`n` === arity) {
                      `traceRts $ funcName <> ": exact"`;
                      `funExact c`;
                    } else if(`n` > arity) {
                      `traceRts $ funcName <> ": oversat"`;
                      `oversatCase c arity0 arity`;
                    } else {
                      `traceRts $ funcName <> ": undersat"`;
                      // fixme do we want double pap?
                      `mkPap pap (toJExpr R1) (toJExpr n) stackArgs`;
                      `Sp` = `Sp` - `r+1`;
                      `R1` = `pap`;
                      return `Stack`[`Sp`];
                    }
                  |]
    funCase :: JExpr -> JStat
    funCase c = withIdent $ \pap ->
                [j| var ar0 = `funArity' c`;
                    var ar  = ar0 & 0xFF;
                    if(`n` === ar) {
                      `traceRts $ funcName <> ": exact"`;
                      `funExact c`;
                    } else if(`n` > ar) {
                      `traceRts $ funcName <> ": oversat"`;
                      `oversatCase c ar0 ar`;
                    } else {
                      `traceRts $ funcName <> ": undersat"`;
                      `mkPap pap (toJExpr R1) (toJExpr n) stackArgs`;
                      `Sp` = `Sp` - `r+1`;
                      `R1` = `pap`;
                      return `Stack`[`Sp`];
                    }
                  |]

    -- oversat: call the function but keep enough on the stack for the next
    oversatCase :: JExpr -- function
                -> JExpr -- the arity value
                -> JExpr -- real arity (arity & 0xff)
                -> JStat
    oversatCase c arity arity0 =
      [j| var rs = `arity` >> 8;
          `loadRegs rs`;
          `Sp` = `Sp` - rs;
          var newAp = h$apply[(`n`-`arity0`)|((`r`-rs)<<8)];
          `Stack`[`Sp`] = newAp;
          `traceRts $ (funcName <> ": new stack frame: ") |+ (newAp |. "n")`;
          return `c`;
        |]
      where
        loadRegs rs = SwitchStat rs switchAlts mempty
          where
            switchAlts = map (\x -> ([je|`x`|], [j|`numReg (x+1)` = `Stack`[`Sp`-`x`]; |])) [r,r-1..1]

{-
  stg_ap_r_n_fast is entered if a function of unknown arity
  is called, n arguments are already in r registers
-}
fastApply :: Int -> Int -> JStat
fastApply r n =
  [j| `decl func`;
      `JVar func` = `JFunc myFunArgs (preamble <> body)`;
    |]
    where
      funName = T.pack ("h$ap_" ++ show n ++ "_" ++ show r ++ "_fast")
      func    = TxtI funName

      myFunArgs = []

      loadArgs = zipWith (\r n -> [j| `r` = `Stack`[`Sp`-`n-1`] |]) (enumFrom R2) [1..r]

      regArgs = take r (enumFrom R2)

      mkAp :: Int -> Int -> [JExpr]
      mkAp n' r' = [ jsv . T.pack $ "h$ap_" ++ show n' ++ "_" ++ show r' ]

      regsTo :: Int -> [JExpr]
      regsTo m = map (toJExpr . numReg) (reverse [m..r+1])

      body = [j| var c = `R1`.f;
                 `traceRts $ (funName <> ": sp ") |+ Sp`;
                 switch(c.t) {
                   case `Fun`:
                     `traceRts $ (funName <> ": ") |+ clName c |+ t" (arity: " |+ (c |. "a") |+ t")"`;
                     var farity = `funArity' c`;
                     `funCase c farity`;
                   case `Pap`:
                     `traceRts $ (funName <> ": pap")`;
                     var arity;
                     `papArity arity (toJExpr R1)`;
                     `funCase c arity`;
                   case `Thunk`:
                     `traceRts $ (funName <> ": thunk")`;
                     `push' $ reverse (map toJExpr $ take r (enumFrom R2)) ++ mkAp n r`;
                     return c;
                   case `Blackhole`:
                     `traceRts $ (funName <> ": blackhole")`;
                     `push' $ reverse (map toJExpr $ take r (enumFrom R2)) ++ mkAp n r`;
                     `push' [toJExpr R1, jsv "h$return"]`;
                     return h$blockOnBlackhole(`R1`);
                   default:
                     throw (`funName <> ": unexpected closure type: "` + c.t);
                 }
               |]
      funCase :: JExpr -> JExpr -> JStat
      funCase c arity = withIdent $ \pap ->
        [j| var ar = `arity` & 0xFF;
            if(`n` === ar) {
              `traceRts (funName <> ": exact")`;
              return `c`;
            } else if (`n` > ar) {
              `traceRts (funName <> ": oversat")`;
              `oversatCase c arity`;
            } else {
              `traceRts (funName <> ": undersat")`;
              `mkPap pap (toJExpr R1) (toJExpr n) (map toJExpr regArgs)`;
              `R1` = `pap`;
              return `Stack`[`Sp`];
            }
          |]
      oversatCase :: JExpr -> JExpr -> JStat
      oversatCase c arity =
        [j| var rs = `arity` >> 8;
            var rsRemain = `r` - rs;
            `traceRts (funName |+ t" regs oversat " |+ rs |+ t" remain: " |+ rsRemain)`;
            `saveRegs rs`;
            `Sp` = `Sp` + rsRemain  + 1;
            `Stack`[`Sp`] = h$apply[(rsRemain<<8)|(`n`-(`arity`&0xFF))];
            return `c`;
          |]
          where
            saveRegs n = SwitchStat n switchAlts mempty
              where
                switchAlts = map (\x -> ([je|`x`|],[j|`Stack`[`Sp`+`r-x`] = `numReg (x+2)`|])) [0..r-1]

zeroApply :: JStat
zeroApply = [j| fun h$ap_0_0_fast { `preamble`; `enter (toJExpr R1)`; }
                fun h$ap_0_0 { `preamble`; `adjSpN 1`; `enter (toJExpr R1)`; }
                `ClosureInfo "h$ap_0_0" (CIRegs 0 [PtrV]) "h$ap_0_0" (CILayoutFixed 0 []) CIStackFrame noStatic`;

                fun h$ap_1_0 x {
                  `preamble`;
                  var c = `R1`.f;
                  `traceRts $ t"h$ap_1_0: " |+ (c|."n") |+ t" :a " |+ (c|."a") |+ t" (" |+ (clTypeName c) |+ t")"`;
                  if(c.t === `Thunk`) {
                    return c;
                  } else if(c.t === `Blackhole`) {
                    `push' [toJExpr R1, jsv "h$return"]`;
                    return h$blockOnBlackhole(`R1`);
                  } else {
                    `adjSpN 1`;
                    return c;
                  }
                }
                `ClosureInfo "h$ap_1_0" (CIRegs 0 [PtrV]) "h$ap_1_0" (CILayoutFixed 0 []) CIStackFrame noStatic`;

                fun h$e c { `preamble`; `R1` = c; `enter c`; }

              |]

-- carefully enter a closure that might be a thunk or a function

-- e may be a local var, but must've been copied to R1 before calling this
enter :: JExpr -> JStat
enter e = [j| if(typeof `e` !== 'object') {
                return `Stack`[`Sp`];
              }
              var c = `e`.f;
              if(c === h$unbox_e) {
                `R1` = `e`.d1;
                return `Stack`[`Sp`];
              }
              switch(c.t) {
                case `Con`:
                  `(mempty :: JStat)`;
                case `Fun`:
                  `(mempty :: JStat)`;
                case `Pap`:
                  return `Stack`[`Sp`];
                case `Blackhole`:
                  `push' [jsv "h$ap_0_0", e, jsv "h$return"]`;
                  return h$blockOnBlackhole(`e`);
                default:
                  return c;
              }
            |]

enterv :: JStat
enterv = push' [jsv "h$ap_1_0"] <> enter (toJExpr R1)

updates =
  [j|
      fun h$upd_frame {
        `preamble`;
        var updatee = `Stack`[`Sp` - 1];
        // wake up threads blocked on blackhole
        var waiters = updatee.d2;
        if(waiters !== null) {
          for(var i=0;i<waiters.length;i++) {
            h$wakeupThread(waiters[i]);
          }
        }
        // overwrite the object
        if(typeof `R1` === 'object') {
          `traceRts $ t"$upd_frame: boxed: " |+ ((R1|."f")|."n")`;
          updatee.f = `R1`.f;
          updatee.d1 = `R1`.d1;
          updatee.d2 = `R1`.d2;
        } else {
          updatee.f = h$unbox_e;
          updatee.d1 = `R1`;
          updatee.d2 = null;
        }
        `adjSpN 2`;
        `traceRts $ t"h$upd_frame: updating: " |+ updatee |+ t" -> " |+ R1`;
        return `Stack`[`Sp`];
      };
      `ClosureInfo "h$upd_frame" (CIRegs 0 [PtrV]) "h$upd_frame" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic`;
  |]

mkFunc :: Ident -> JStat -> JStat
mkFunc func body = [j| `decl func`; `JVar func` = `JFunc funArgs body`; |]

{-
  Partial applications. There are two different kinds of partial application:
    pap_r contains r registers, pap_gen can contain any number

    layout:
     - d1      = function
     - d2.d1 & 0xff = number of args
       d2.d1 >> 8   = number of registers (r for h$pap_r)
     - d2.d2.. = args (r)
-}
-- arity is the remaining arity after our supplied arguments are applied
mkPap :: Ident   -- ^ id of the pap object
      -> JExpr   -- ^ the function that's called (can be a second pap)
      -> JExpr   -- ^ number of arguments in pap
      -> [JExpr] -- ^ values for the supplied arguments
      -> JStat
mkPap tgt fun n values =
      traceRts ("making pap with: " ++ show (length values) ++ " items") <>
      allocDynamic True tgt (iex entry) (fun:[je| `length values * 256`+`n`|]:map toJExpr values')
  where
    values' | null values = [jnull]
            | otherwise   = values
    entry | length values > numSpecPap = TxtI "h$pap_gen"
          | otherwise                  = TxtI . T.pack $ "h$pap_" ++ show (length values)

-- specialized (faster) pap generated for [0..numSpecPap]
-- others use h$pap_gen
specPap :: [Int]
specPap = [0..numSpecPap]

numSpecPap :: Int
numSpecPap = 6

pap :: Int -> JStat
pap r = [j| `decl func`;
            `iex func` = `JFunc [] (preamble <> body)`;
            `ClosureInfo funcName CIRegsUnknown funcName (CILayoutUnknown (r+2)) CIPap noStatic`;
          |]
  where
    funcName = T.pack ("h$pap_" ++ show r)

    func     = TxtI funcName

    body = [j| var c = `R1`.d1; // the closure
               var d = `R1`.d2;
               var f = c.f;
               `assertRts (isFun' f ||| isPap' f) (funcName <> ": expected function or pap")`;
               var extra;
               if(`isFun' f`) {
                 extra = (f.a>>8) - `r`;
               } else {
                 `papArity extra c`;
                 extra = (extra>>8) - `r`;
               }
               `traceRts $ (funcName <> ": pap extra args moving: ") |+ extra`;
               `moveBy extra`;
               `loadOwnArgs d`;
               `R1` = c;
               return f;
             |]
    moveBy extra = SwitchStat extra
                   (reverse $ map moveCase [1..maxReg-r-1]) mempty
    moveCase m = (toJExpr m, [j| `numReg (m+r+1)` = `numReg (m+1)`; |])
    loadOwnArgs d = mconcat $ map (\r -> [j| `numReg (r+1)` = `dField d (r+2)`; |]) [1..r]
    dField d n = SelExpr d (TxtI . T.pack $ ('d':show (n-1)))

-- generic pap
papGen :: JStat
papGen = [j| fun h$pap_gen {
               var c = `R1`.d1;
               var d = `R1`.d2;
               var r = d.d1;
               var f = c.f;
               `assertRts (isFun' f ||| isPap' f) $ t "h$pap_gen: expected function or pap"`;
               var extra;
               if(`isFun f`) {
                 extra = (f.a>>8) - r;
               } else {
                 extra = (extra>>8) - r;
               }
               `traceRts $ (t "h$pap_gen: generic pap extra args moving: " |+ extra)`;
               h$moveRegs2(extra, r);
               `loadOwnArgs d r`;
               `R1` = c;
               return f;
             }
             `ClosureInfo funcName CIRegsUnknown funcName CILayoutVariable CIPap noStatic`;
           |]
  where
    funcName = "h$pap_gen"
    loadOwnArgs d r =
      let prop n = d |. ("d" <> T.pack (show $ n+1))
          loadOwnArg n = (toJExpr n, [j| `numReg (n+1)` = `prop n`; |])
      in  SwitchStat r (map loadOwnArg [127,126..1]) mempty

-- general utilities
-- move the first n registers, starting at R2, m places up (do not use with negative m)
moveRegs2 :: JStat
moveRegs2 = [j| fun h$moveRegs2 n m {
                 `moveSwitch n m`;
               }
             |]
  where
    moveSwitch n m = SwitchStat [je| (n << 8 | m) |] switchCases (defaultCase n m)
    -- fast cases
    switchCases = [switchCase n m | n <- [1..5], m <- [1..4]] -- tune the parameteters for performance and size
    switchCase :: Int -> Int -> (JExpr, JStat)
    switchCase n m = (toJExpr $ (n `shiftL` 8) .|. m, mconcat (map (\n -> moveRegFast n m) [n+1,n..2]) <> [j| break; |])
    moveRegFast n m = [j| `numReg n` = `numReg (n+m)`; |]
    -- fallback
    defaultCase n m = [j| for(var i=n;i>0;i--) {
                            h$setReg(i+1, h$getReg(i+1+m));
                          }
                        |]
    moveReg n m = [j| h$setReg(`n`, h$getReg(`n+m`)); |]

