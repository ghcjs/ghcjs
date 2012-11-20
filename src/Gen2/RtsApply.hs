{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  generate various apply functions for the rts
  note: the fast apply functions expect that the same specialized forms
        exist for stack apply

  - fixme: add selector thunks and let the gc follow them
-}

{-
  for: stg_ap_n_fast
   - tag is a bitmap of pointers

  the `a` tag in a function:
   - f.a & 0xff           = number of arguments (registers) expected
   - (f.a & 0xff00) >> 8  = number of trailing void args

  pap heap object p:  pap_n has n registers stored, layout is unknown because of pointers
   (gc recovers ptr layout from function at p+1)
  fixme? no way to indicate that pap has received a trailing void arg, effectively limiting us to max 1 trailing v
   - p.gtag == -n-1        = only object size here, pointers require special care
   - p.gai = [-1]         = special care in the gc required!
   - heap[p+1]            = original function (can be a pap?)
   - heap[p+2] ..         = arguments

  to get number of remaining registers: use function info - object size: (heap[p+2].a >> 8) -

  -- generic applications on stack, when called, r1 is always pointer to closure
  stg_ap_X.gai = [1]      = r1 active, ptr, no other arguments
  stg_ap_X.gtag & 0xff    = object size
  stg_ap_X.gtag >> 8      = pointer locations
  stg_ap_X.a & 0xff       = number of funcall arguments it has in hand, can be different from size!

  -- specialized versions
  stg_ap_1_n              = 1 nonpointer
  stg_ap_1_p              = 1 pointer
  stg_ap_1_nv             = 1 nonpointer, void argument extra
  stg_ap_1_pv             = 1 pointer, void argument extra
  stg_ap_2_nn             = 2x non, 2 args
  stg_ap_2_pp             = 2x ptr, 2 args
  stg_ap_3_ppp

  layout specialized: [stg_ap_X, arg1, arg2]

  -- generic
  stg_ap_1
  stg_ap_2
  stg_ap_3 ...
  stg_ap_4
  stg_ap_5
  stg_ap_6 ..

  layout generic s = [stg_ap_X, layout, tag, arg1, arg2, ...]
  stg_ap_1.gai = [1];      = r1 active, pointer to closure, rest is filled by apply func
  stg_ap_1.gtag = -1;      = layout info inside frame
  layout && 0xff           = size of frame
  layout >> 8              = offsets of pointers (starting at 2)



-}

module Gen2.RtsApply where

import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Types

import           Gen2.RtsAlloc
import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils

import           Data.Bits
import           Data.List                        (find, foldl', sort)
import           Data.Maybe
import           Data.Monoid

rtsApply :: JStat
rtsApply = mconcat $  map (\n -> stackApply n 0 Nothing) [1..8]
                   ++ map (\n -> stackApply n 1 Nothing) [1..8]
                   ++ map (\(name, n, v, ptr) -> stackApply n v (Just (name, ptr))) fixedApply
                   ++ map (\n -> fastApply n 0 Nothing) [1..8]
                   ++ map (\n -> fastApply n 1 Nothing) [1..8]
                   ++ map (\(name, n, v, ptr) -> fastApply n v (Just (name, ptr))) fixedApply
                   ++ map pap          [1..8]
                   ++ [zeroApply, vApply, updates]

-- list of specialized apply paths:
-- (name, number of args, number of trailing void, pointer offsets)
fixedApply = [ ( "p",      1, 0, [0]         )
             , ( "n",      1, 0, []          )
             , ( "pn",     2, 0, [0]         )
             , ( "nn",     2, 0, []          )
             , ( "np",     2, 0, [0]         )
             , ( "pp",     2, 0, [0,1]       )
             , ( "pv",     1, 1, [0]         )
             , ( "nv",     1, 1, []          )
             , ( "nnv",    2, 1, []          )
             , ( "pnv",    2, 1, [0]         )
             , ( "npv",    2, 1, [1]         )
             , ( "ppv",    2, 1, [0,1]       )
             , ( "ppp",    3, 0, [0,1,2]     )
             , ( "pppv",   3, 1, [0,1,2]     )
             , ( "pppp",   4, 0, [0,1,2,3]   )
             , ( "ppppv",  4, 1, [0,1,2,3]   )
             , ( "ppppp",  5, 0, [0,1,2,3,4] )
             , ( "pppppv", 5, 1, [0,1,2,3,4] )
             ]

stackApply :: Int ->                   -- ^ number of registers in stack frame
              Int ->                   -- ^ number of extra trailing void args
              Maybe (String, [Int]) -> -- ^ optional pair for fixed layout thing
              JStat
stackApply n v mfixed = [j| `decl func`;
                            `JVar func` = `JFunc funArgs (preamble <> body)`;
                            `ClosureInfo (iex func) [R1] funcName layout (CIFun 0 0) CINoStatic`;
                          |]
  where
    layout = case mfixed of
       Nothing           -> CILayoutVariable
       Just (_, offsets) -> CILayoutPtrs frameSize offsets

    funcName = case mfixed of
                 Nothing     -> "stg_ap_" ++ show n ++ vsuff
                 Just (n, _) -> "stg_ap_" ++ n

    vsuff | v == 0    = ""
          | otherwise = "_" ++ replicate v 'v'

    (frameSize, offset)
        | isJust mfixed = (n+1, 0) -- fixed layout
        | otherwise     = (n+2, 1) -- layout stored in frame

    popFrame = adjSpN frameSize

    func = StrI funcName
    body = [j| do {
                 var c = `Heap`[`R1`];
                 `traceRts $ funcName |+ " " |+ (c|."n") |+ " sp: " |+ Sp`;
                 switch(c.t) {
                   case `Thunk`:
                     `traceRts $ funcName |+ ": thunk"`;
                     return c;
                   case `Fun`:
                     `traceRts $ funcName |+ ": fun"`;
                     `funCase c`;
                   case `Pap`:
                     `traceRts $ funcName |+ ": pap"`;
                     `papCase c`;
                   case `Ind`:
                     `traceRts $ funcName |+ ": ind"`;
                     `R1` = `Heap`[`R1`+1];
                     continue;
                   default:
                     throw (`"panic: " ++ funcName ++ ", unexpected closure type: "` + c.t);
                 }
               } while(true);
           |]

    funExact c = popSkip (1+offset) (take n (map toJExpr $ enumFrom R2))
    stackArgs = map (\x -> [je| `Stack`[`Sp`-`x+offset`] |]) [1..n]

    overSat :: JExpr -> JExpr -> JStat
    overSat c m = SwitchStat [je| `n`-`m` |] (map (oversatCase c) [0..n-1])
                      (traceRts $ funcName ++ ": oversat: unexpected arity")

    oversatCase :: JExpr -> Int -> (JExpr, JStat)
    oversatCase c m = oversatCase' m
       where
          oversatFun 0 | v == 0    = ("stg_ap_zero", True)
                       | otherwise = ("stg_ap_v", True)
          oversatFun m = fromMaybe ("stg_ap_" ++ show m ++ vsuff, False) $ do
            (_, ptrs) <- mfixed
            let ptrs' = shiftedPtrTag (n-m) ptrs
            (name, _, _, _) <-
                find (\(_, m', v', p) -> (m', v', ptrTag p) == (m,v,ptrs'))
                fixedApply
            return ("stg_ap_" ++ name, True)

          newTag :: Int -> JExpr
          newTag m = case mfixed of
                       Nothing        -> [je| (`Heap`[`R1`+1] >> `8+n-m` << 8) | `m+2` |]
                       -- fixme: proper support for big tags/objs?
                       Just (_, ptrs) -> let nt = (fromJust (shiftedPtrTag (n-m) ptrs) `shiftL` 8) .|. (m+2)
                                         in  toJExpr nt

          oversatCase' :: Int -> (JExpr, JStat)
          oversatCase' m | oFixed    = (toJExpr m, [j| `loadArgs`;
                                                      `adjSpN $ frameSize + m + 1`;
                                                      `Stack`[`Sp`] = `jsv oFun`;
                                                      return `c`;
                                                    |])
                        | otherwise = (toJExpr m, [j| `loadArgs`;
                                                      `adjSpN $ frameSize + m + 2`;
                                                      `Stack`[`Sp` - 1] = `newTag m`;
                                                      `Stack`[`Sp`] = `jsv oFun`;
                                                      return `c`;
                                                    |])
               where
                 loadArgs = loadSkip 1 (take (n-m) (map toJExpr $ enumFrom R2))
                 (oFun, oFixed) = oversatFun m

    papCase :: JExpr -> JStat
    papCase c = withIdent $ \pap ->
                [j| var arity;
                    `papArity arity (toJExpr R1)`;
                    var arity2 = arity & 0xff;
                    `traceRts $ (funcName ++ ": found pap, arity: ") |+ arity`;
                    if(`n` === arity2) {
                      `traceRts $ funcName ++ ": exact"`;
                      `funExact c`;
                      return `c`;
                    } else if(`n` < arity2 || (`n` === arity2 && (arity >> 8) < `v`)) {
                      `traceRts $ funcName ++ ": undersat"`;
                      `mkPap pap (toJExpr R1) stackArgs`; // fixme do we want double pap?
                      `R1` = `iex pap`;
                      `adjSpN (n+1)`;
                      return `Stack`[`Sp`];
                    } else {
                      `traceRts $ funcName ++ ": oversat"`;
                      `overSat c arity2`;
                    }
                  |]
    funCase :: JExpr -> JStat
    funCase c = let alts = zip (map toJExpr [(1::Int)..]) (map funOver [1..n-1] ++ [funExact0])
                in  SwitchStat [je|`funArity c` & 0xff |] alts funUnder
                   where
                     funExact0 = withIdent $ \pap ->
                                 [j| `traceRts "exact0"`;
                                     var voidArgs = `funArity c` >> 8;
                                     if(voidArgs === `v`) {  // exact
                                       `traceRts "exact1"`;
                                       `funExact c`;
                                       return `c`;
                                     } else if(`v` < voidArgs) { // oversat
                                       `traceRts "oversat"`;
                                       `funExact c`;
                                       for(var i=voidArgs;i<`v`;i++) {
                                         `adjSp 1`;
                                         `Stack`[`Sp`] = stg_ap_v;
                                       }
                                       return `c`;
                                     } else {
                                       // missing void args, fixme we can't store how many in pap!
                                       `mkPap pap (toJExpr R1) stackArgs`;
                                       `popFrame`;
                                       `R1` = `iex pap`;
                                       return `Stack`[`Sp`];
                                     }
                                   |]
                     funOver m = snd $ oversatCase c m
                     funUnder  = withIdent $ \pap -> [j|
                                     `traceRts $ "undersat (" |+ funArity c |+ ")"`;
                                     var arity = `funArity c`;
                                     var tag = `funTag (toJExpr R1)`;
                                     `mkPap pap (toJExpr R1) stackArgs`;
                                     `R1` = `iex pap`;
                                     `popFrame`;
                                     return `Stack`[`Sp`];
                                   |]

vApply :: JStat
vApply = [j| fun stg_ap_v_fast {
               `preamble`;
               var h = `Heap`[`R1`];
               switch(h.t) {
                 case `Fun`:
                   if(h.a === 256) {  // 0 args, 1 trailing v
                     return h;
                   } else {
                     log("stg_ap_v_fast: PAP");
                   }
                 default:
//                   `adjSp 1`;
                   `push [iex $ StrI "stg_ap_v"]`; // fixme not necessary
                   return stg_ap_v;
               }
             }
         |]

{-
  stg_ap_n_fast is entered if a function of unknown arity
  is called, arguments are already in registers
-}
{-
  fast apply is never a heap object, pushed to the stack, or returned
  so no gc info is needed

  the non-specialized version is called with a tag argument
-}
fastApply :: Int -> Int -> Maybe (String, [Int]) -> JStat
fastApply 0 0 _ = let func = StrI "stg_ap_0_fast" in decl func <> [j| `JVar func` = `JFunc [] (preamble <> enter)` |]
fastApply 0 1 _ = let func = StrI "stg_ap_v_fast" in decl func <> [j| `JVar func` = `JFunc [] (preamble <> enter)` |]
-- [j| fun stg_ap_v_fast !o { `enter`; } |]
fastApply n v mspec = [j| `decl func`;
                          `JVar func` = `JFunc myFunArgs (preamble <> body)`;
                        |]
    where
      funName = case mspec of
                  Nothing       -> "stg_ap_" ++ show n ++ vsuff ++ "_fast"
                  Just (name,_) -> "stg_ap_" ++ name            ++ "_fast"
      func    = StrI funName

      myFunArgs | isJust mspec = funArgs
                | otherwise    = funArgs ++ [StrI "tag"]

      vsuff | v == 0 || isJust mspec = ""
            | otherwise              = '_' : replicate v 'v'

      loadArgs = zipWith (\r n -> [j| `r` = `Stack`[`Sp`-`n-1`] |]) (enumFrom R2) [1..n]

      fastSwitch :: JExpr -> JStat
      fastSwitch c = SwitchStat [je| `c`.a & 0xff |] (map (oversat c) [0..n-1] ++ [exact c]) (undersat c)

      exact :: JExpr -> (JExpr, JStat)
      exact c      = (toJExpr n, withIdent $ \pap ->
                                 [j| var vexp = `c`.a >> 8;
                                     if(`v` == vexp) {        // exactly right
                                       return `c`;
                                     } else if(`v` < vexp) {  // oversat, push v apply
                                       `adjSp 1`;
                                       `Stack`[`Sp`] = stg_ap_v; // fixme only one v supported
                                       return `c`;
                                     } else {                 // undersat, make pap
                                       `makePap pap`;
                                       `R1` = `iex pap`;
                                       return `Stack`[`Sp`];
                                     }
                                   |])

      undersat :: JExpr -> JStat
      undersat c = withIdent $ \pap ->
                   [j| `makePap pap`;
                       `R1` = `iex pap`;
                       return `Stack`[`Sp`];
                     |]

      -- stack things we need to push when we have m arguments left after applying our function
      mkAp :: Int -> [JExpr]
      mkAp 0 = [ jsv $ "stg_ap_0" ++ vsuff ]
      mkAp m = fromMaybe tagged $ do
                 (_, ptrs) <- mspec
                 let ptrs' = shiftedPtrTag (n-m) ptrs
                 (name, _, _, _) <- find (\(_, m', v', p) -> (m',v',ptrTag p) == (m,v,ptrs')) fixedApply
                 return [ jsv $ "stg_ap_" ++ name ]
                   where
                     newTag = case mspec of
                                Nothing        -> [je| tag >> `n-m` |]
                                -- fixme proper support for big objects
                                Just (_, ptrs) -> toJExpr (fromJust $ shiftedPtrTag (n-m) ptrs)
                     tagged = [ newTag
                              , jsv $ "stg_ap_" ++ show m ++ vsuff
                              ]

      makePap :: Ident -> JStat
      makePap pap = mkPap pap (toJExpr R1) (map toJExpr $ take n $ enumFrom R2)

      regsTo :: Int -> [JExpr]
      regsTo m = map (toJExpr . numReg) (reverse [m..n+1])

      oversat :: JExpr -> Int -> (JExpr, JStat)
      oversat c m =
          (toJExpr m, [j| `push $ map toJExpr (reverse $ enumFromTo (numReg (m+2)) (numReg (n+1))) ++ mkAp (n-m)`
                           return `c`;
                        |])

      body = [j|
                 var c = `Heap`[`R1`];
                 do {
                   if(c.t === `Fun`) {
                       `traceRts $ (funName ++ ": ") |+ clName c |+ " (arity: " |+ (c |. "a") |+ ")"`;
                       `fastSwitch c`;
                   } else if(c.t === `Ind`) {
                       `traceRts $ funName ++ ": following ind"`;
                       `R1` = `Heap`[`R1`+1];
                       continue;
                   } else {
                       `traceRts $ (funName ++ ": ") |+ (c|."n") |+ " (not a fun but: " |+ clTypeName c |+ ") to " |+ n |+ " args"`;
                     `push $ reverse (map toJExpr $ take n (enumFrom R2)) ++ mkAp n`;
                     return `c`;
                   }
                 } while(true);
               |]



zeroApply :: JStat
zeroApply = [j| fun stg_ap_0_fast { `preamble`; `enter`; }

                fun stg_ap_0 { `preamble`; `adjSpN 1`; `enter`; }
                `ClosureInfo (iex (StrI "stg_ap_0")) [R1] "stg_ap_0" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

                fun stg_ap_v x { `preamble`; `adjSpN 1`; return `Heap`[`R1`]; }
                `ClosureInfo (iex (StrI "stg_ap_v")) [R1] "stg_ap_v" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;
                var !stg_ap_0_v_fast = stg_ap_v_fast;
                var !stg_ap_0_v = stg_ap_v;

              |]
{-
                `setObjInfo (jsv "stg_ap_v") $
                   "t"   .= Fun <>
                   "a"   .= ji 0 <>
                   "i"   .= [ji 1, jstr "stg_ap_v"] <>
                   gcInfo 1 [] <>
                   "gai" .= [ji 1]
                `;
              |]
-}
-- carefully enter a closure that might be a thunk or a function
enter :: JStat
enter = [j| var c = `Heap`[`R1`]; `enter' c`; |]

enter' :: JExpr -> JStat
enter' c = [j| do {
                 switch(`c`.t) {
                   case `Ind`:
                     `R1` = `Heap`[`R1`+1];
                     `c` = `Heap`[`R1`];
                     continue;
                   case `Con`:
                     `(mempty :: JStat)`;
                   case `Fun`:
                     `(mempty :: JStat)`;
                   case `Pap`:
                     return `Stack`[`Sp`];
                   default:
                     return `c`;
                 }
              } while(true);
             |]



updates =
  [j|
      var !ind_entry;
      ind_entry = \x {
        `preamble`;
        `R1` = `Heap`[`R1`+1];
        return `Heap`[`R1`];
      };
      `ClosureInfo (iex $ StrI "ind_entry") [] "updated frame" (CILayoutFixed 2 [PtrV]) CIInd CINoStatic`;

      fun stg_upd_frame {
        `preamble`;
        var updatee = `Stack`[`Sp` - 1];
        `adjSpN 2`;
        `traceRts $ "updating: " |+ updatee |+ " -> " |+ R1`;
        `Heap`[updatee] = ind_entry;
        `Heap`[updatee+1] = `R1`;
        if(updatee < hpOld) {
          hpForward.push(updatee);
        }
        return `Stack`[`Sp`];
      };
      `ClosureInfo (iex $ StrI "stg_upd_frame") [R1] "stg_upd_frame" (CILayoutFixed 2 [PtrV]) (CIFun 0 0) CINoStatic`;
  |]
{-
updateApply :: Int -> JStat
updateApply n = mkFunc func body <>
                setGcInfo (iex func) (n+1) [] <>
               [j| `iex func`.i  = [`n+1`, `funcName`];
                   `iex func`.gai = [1];
                |]
    where
      funcName = "stg_ap_" ++ show n ++ "_upd"
      func    = StrI funcName
      args    = map (\m -> [je| heap[r1+`m`] |]) [2..n]
      retfun | n == 1    = "stg_ap_0_fast"
             | otherwise = "stg_ap_" ++ show (n-1)
      body = [j| r1 = heap[r1+1];
                 `push args`;
                 `push [jsv retfun]`;
                 return `jsv retfun`();
               |]
-}

mkFunc :: Ident -> JStat -> JStat
mkFunc func body = [j| `decl func`; `JVar func` = `JFunc funArgs body`; |]

-- arity is the remaining arity after our supplied arguments are applied
mkPap :: Ident   -- ^ id of the pap object
      -> JExpr   -- ^ the function that's called (can be a second pap)
      -> [JExpr] -- ^ values for the supplied arguments
      -> JStat
mkPap tgt fun values =
    traceRts ("making pap with: " ++ show (length values) ++ " items") <>
    allocDynamic True tgt (iex entry) (fun:map toJExpr values)
        where
          entry = StrI $ "stg_pap_" ++ show (length values)

-- entry function for a pap with n stored registers
pap :: Int -> JStat
pap n = [j| `decl func`;
            `iex func` = `JFunc [] (preamble <> body)`;
            `ClosureInfo (iex func) [] funcName CILayoutVariable (CIPap n) CINoStatic`;
          |]
  where
    funcName = "stg_pap_" ++ show n
    func     = StrI funcName

    body = [j| var c = `Heap`[`R1`+1];
               var f = `Heap`[c];
               `assertRts (isFun f ||| isPap f) (funcName ++ ": expected function or pap")`;
               var extra = (f.a & 0xff) - `n`;
               `moveBy extra`;
               `loadOwnArgs`;
               `R1` = c;
               return f;
             |]
    moveBy extra = SwitchStat extra
                   (reverse $ map moveCase [1..maxReg-n-1]) mempty
    moveCase m = (toJExpr m, [j| `numReg (m+n+1)` = `numReg (m+1)`; |])
    loadOwnArgs = mconcat $ map (\r -> [j| `numReg (r+1)` = `Heap`[`R1`+`r+1`]; |]) [1..n]

