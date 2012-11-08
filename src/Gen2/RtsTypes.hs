{-# LANGUAGE QuasiQuotes #-}
module Gen2.RtsTypes where

import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Types

import           Gen2.StgAst
import           Gen2.Utils

import           Data.Char                        (toLower)

import           Data.Bits
import qualified Data.List                        as L
import           Data.Monoid

import           Encoding
import           Id
import           StgSyn
import           TyCon
import           Type
import           Unique

import           Gen2.RtsSettings

-- closure types
data CType = Thunk | Fun | Pap | Con | Ind | Blackhole
  deriving (Show, Eq, Ord, Enum, Bounded)

--
ctNum :: CType -> Int
ctNum Fun       = 1
ctNum Con       = 2
ctNum Thunk     = 0 -- 4
ctNum Pap       = 3 -- 8
ctNum Ind       = 4 -- 16
ctNum Blackhole = 5 -- 32

instance ToJExpr CType where
  toJExpr e = toJExpr (ctNum e)

-- function argument and free variable types
data VarType = PtrV     -- pointer = heap index, one field (gc follows this)
             | VoidV    -- no fields
--             | FloatV   -- one field -- no single precision supported
             | DoubleV  -- one field
             | IntV     -- one field
             | LongV    -- two fields
             | ArrV     -- a pointer not to the heap: two fields, array + index
               deriving (Eq, Ord, Show, Enum, Bounded)

varSize :: VarType -> Int
varSize VoidV = 0
varSize LongV = 2
varSize ArrV  = 2
varSize _     = 1

isVoid :: VarType -> Bool
isVoid VoidV = True
isVoid _     = False

isPtr :: VarType -> Bool
isPtr PtrV = True
isPtr _    = False

isSingleVar :: VarType -> Bool
isSingleVar v = varSize v == 1

isMultiVar :: VarType -> Bool
isMultiVar v = varSize v > 1

-- can we pattern match on these values in a case?
isMatchable :: VarType -> Bool
isMatchable DoubleV = True
isMatchable IntV    = True
isMatchable _       = False

{- fixme don't use this, this loses information
vtFromCgr :: CgRep -> VarType
vtFromCgr VoidArg   = VoidV
vtFromCgr PtrArg    = PtrV
vtFromCgr NonPtrArg = ArrV
vtFromCgr LongArg   = LongV
vtFromCgr FloatArg  = DoubleV
vtFromCgr DoubleArg = DoubleV
-}

-- go through PrimRep, not CgRep to make Int -> IntV instead of LongV
tyConVt :: TyCon -> VarType
tyConVt = primRepVt . tyConPrimRep

typeVt :: Type -> VarType
typeVt = primRepVt . typePrimRep

argVt :: StgArg -> VarType
argVt = typeVt . stgArgType

primRepVt :: PrimRep -> VarType
primRepVt VoidRep   = VoidV
primRepVt PtrRep    = PtrV
primRepVt IntRep    = IntV
primRepVt WordRep   = IntV
primRepVt Int64Rep  = LongV
primRepVt Word64Rep = LongV
primRepVt AddrRep   = ArrV
primRepVt FloatRep  = DoubleV
primRepVt DoubleRep = DoubleV


instance ToJExpr VarType where
  toJExpr = toJExpr . fromEnum

data StgReg = R1  | R2  | R3  | R4  | R5  | R6  | R7  | R8
            | R9  | R10 | R11 | R12 | R13 | R14 | R15 | R16
            | R17 | R18 | R19 | R20 | R21 | R22 | R23 | R24
            | R25 | R26 | R27 | R28 | R29 | R30 | R31 | R32
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ToJExpr StgReg where
  toJExpr = ve . {- ("r."++) . -} map toLower . show

regName :: StgReg -> String
regName = map toLower . show

regNum :: StgReg -> Int
regNum r = fromEnum r + 1

numReg :: Int -> StgReg
numReg r = toEnum (r - 1)

minReg :: Int
minReg = regNum minBound

maxReg :: Int
maxReg = regNum maxBound

-- arguments that the trampoline calls our funcs with
funArgs :: [Ident]
funArgs = [] -- [StrI "o"] -- [StrI "_heap", StrI "_stack"] -- [] -- [StrI "r", StrI "heap", StrI "stack"]

data Special = Stack | Sp | Heap | Hp deriving (Show, Eq)

instance ToJExpr Special where
  toJExpr Stack = [je| _stack |]
  toJExpr Sp    = [je| _sp    |]
  toJExpr Heap  = [je| _heap  |]
  toJExpr Hp    = [je| hp     |]


adjHp :: Int -> JStat
adjHp e = [j| hp+=`e`; |] -- [j| `jsv "hp = _hp"` = _hp + `e` |]

adjHpN :: Int -> JStat
adjHpN e = [j| hp=hp-`e`; |] -- [j| `jsv "hp = _hp"` = _hp - `e` |]

adjSp :: Int -> JStat
adjSp e = [j| _sp += `e`; sp = _sp; |]

adjSpN :: Int -> JStat
adjSpN e = [j| _sp = _sp - `e`; sp = _sp; |]

-- stuff that functions are supposed to execute at the start of the body
-- (except very simple functions)
preamble :: JStat
preamble = [j| var !_stack = stack; var !_heap = heap; var !_sp = sp; |] -- var !_heap = heap; |] -- var !_sp = sp; |]
-- [j| var th = `jsv "('x',eval)"`('this'); var !_stack = th.stack; var !_heap = th.heap; var !_sp = th.sp; |]
ve :: String -> JExpr
ve = ValExpr . JVar . StrI

-- setClosureInfo -- fixme

-- set fields used by the gc to follow pointers
-- gc info fields:
-- .gtag -> gtag & 0xFF = size, other bits indicate  offsets of pointers, gtag === 0 means tag invalid, use list
-- .gi   -> info list, [size, offset1, offset2, offset3, ...]
gcInfo :: Int   -> -- ^ size of closure in array indices, including entry
         [Int]  -> -- ^ offsets from entry where pointers are found
          JObj
gcInfo size offsets =
  "gi"   .= infolist <>
  "gtag" .= tag
      where
        tag | maximum (0:offsets) < 25 && minimum (1:offsets) >= 0 && size <= 255 =
                size .|. (L.foldl' (.|.) 0 $ map (\x -> 1 `shiftL` (8+x)) offsets)
            | otherwise = 0
        infolist = size : L.sort offsets

-- set fields to indicate that object layout is stored  inside the object in the second index
-- objects with embedded layout info have gtag == -1
gcEmbedded :: JObj
gcEmbedded =
    "gi"   .= [ji (-1)] <>
    "gtag" .= ji (-1)

-- a pap has a special gtag, pap object size n (n-2 arguments), we hve gtag = -n-1
gcPap :: Int -> JObj
gcPap n =
    "gi"   .= [n] <>
    "gtag" .= ji (-n-1)

push :: [JExpr] -> JStat
push [] = mempty
push xs = [j| `adjSp l`; `items`; |]
  where
    items = zipWith (\i e -> [j| `Stack`[`offset i`] = `e`; |]) [(1::Int)..] xs
    offset i | i == l    = [je| `Sp` |]
             | otherwise = [je| `Sp` - `l-i` |]
    l = length xs

pop :: [JExpr] -> JStat
pop = popSkip 0

-- pop the expressions, but ignore the top n elements of the stack
popSkip :: Int -> [JExpr] -> JStat
popSkip 0 [] = mempty
popSkip n [] = adjSpN n
popSkip n xs = [j| `loadSkip n xs`; `adjSpN $ length xs+n`; |]

-- like popSkip, but without modifying sp
loadSkip :: Int -> [JExpr] -> JStat
loadSkip n xs = mconcat items
    where
      items = reverse $ zipWith (\i e -> [j| `e` = `Stack`[`offset (i+n)`]; |]) [(0::Int)..] (reverse xs)
      offset 0 = [je| `Sp` |]
      offset n = [je| `Sp` - `n` |]

debugPop e@(ValExpr (JVar (StrI i))) offset = [j| log("popped: " + `i`  + " -> " + `e`) |]
debugPop _ _ = mempty

-- declare and pop
popSkipI :: Int -> [Ident] -> JStat
popSkipI 0 [] = mempty
popSkipI n [] = adjSpN n
popSkipI n xs = [j| `loadSkipI n xs`; `adjSpN $ length xs+n`; |]

-- like popSkip, but without modifying sp
loadSkipI :: Int -> [Ident] -> JStat
loadSkipI n xs = mconcat items
    where
      items = reverse $ zipWith (\i e -> [j| `decl e`; `iex e` = `Stack`[`offset (i+n)`]; |]) [(0::Int)..] (reverse xs)
      offset 0 = [je| `Sp` |]
      offset n = [je| `Sp` - `n` |]

popn :: Int -> JStat
popn n = adjSpN n

-- below: c argument is closure entry, p argument is (heap) pointer to entry

closureType :: JExpr -> JExpr
closureType c = [je| `c`.t |]

isThunk :: JExpr -> JExpr
isThunk c = [je| `closureType c` === `Thunk` |]

isFun :: JExpr -> JExpr
isFun c = [je| `closureType c` === `Fun` |]

isPap :: JExpr -> JExpr
isPap c = [je| `closureType c` === `Pap` |]

isCon :: JExpr -> JExpr
isCon c = [je| `closureType c` === `Con` |]

isInd :: JExpr -> JExpr
isInd c = [je| `closureType c` === `Ind` |]

conTag :: JExpr -> JExpr
conTag c = [je| `c`.a |]

entry :: JExpr -> JExpr
entry p = [je| `Heap`[`p`] |]

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of trailing void args)
funArity :: JExpr -> JExpr
funArity c = [je| `c`.a |]

-- expects heap pointer to entry (fixme document this better or make typesafe)
-- arity & 0xff = real number of arguments
-- arity >> 8   = number of trailing void
papArity :: JExpr -> JExpr -> JStat
papArity tgt p = [j| `tgt` = 0;
                     var cur = `p`;
                     do {
                       `tgt` = `tgt`-`papArgs cur`;
                       cur = `Heap`[cur+1];
                     } while(`Heap`[cur].t === `Pap`);
                     `tgt` += `Heap`[cur].a;
                   |]

-- number of stored args in pap
papArgs :: JExpr -> JExpr
papArgs p = [je| (-3) - `Heap`[`p`].gtag |]

funTag :: JExpr -> JExpr
funTag c = [je| `c`.gtag |]

-- some utilities do do something with a range of regs
-- start or end possibly supplied as javascript expr
withRegs :: StgReg -> StgReg -> (StgReg -> JStat) -> JStat
withRegs start end f = mconcat $ map f [start..end]

withRegs' :: Int -> Int -> (StgReg -> JStat) -> JStat
withRegs' start end f = withRegs (numReg start) (numReg end) f

-- start from js expr, start is guaranteed to be at least min
-- from low to high (fallthrough!)
withRegsS :: JExpr -> StgReg -> Int -> Bool -> (StgReg -> JStat) -> JStat
withRegsS start min end fallthrough f =
  SwitchStat start (map mkCase [regNum min..end]) mempty
    where
      brk | fallthrough = mempty
          | otherwise   = [j| break; |]
      mkCase n = (toJExpr n, [j| `f (numReg n)`; `brk`; |])

-- end from js expr, from high to low
withRegsRE :: Int -> JExpr -> StgReg -> Bool -> (StgReg -> JStat) -> JStat
withRegsRE start end max fallthrough f =
  SwitchStat end (reverse $ map mkCase [numReg start..max]) mempty
    where
      brk | fallthrough = mempty
          | otherwise   = [j| break; |]
      mkCase n = (toJExpr (regNum n), [j| `f n`; `brk` |])


jsIdIdent :: Id -> Maybe Int -> String -> Ident
jsIdIdent i mn suffix = StrI . (\x -> "$hs_"++x++suffix++u) . zEncodeString . showPpr' . idName $ i
    where
      u  | isLocalId i = '_' : (show . getKey . getUnique $ i)
         | otherwise   = ""

jsVar :: String -> JExpr
jsVar v = ValExpr . JVar . StrI $ v

-- regular id, shortcut for bools!
jsId :: Id -> JExpr
jsId i | n == "GHC.Types.True"  = toJExpr (1::Int)
       | n == "GHC.Types.False" = toJExpr (0::Int)
       | otherwise = ValExpr (JVar $ jsIdIdent i Nothing "")
    where
      n = showPpr' . idName $ i

-- entry id
jsEnId :: Id -> JExpr
jsEnId i = ValExpr (JVar $ jsEnIdI i)

jsEnIdI :: Id -> Ident
jsEnIdI i = jsIdIdent i Nothing "_e"

jsEntryId :: Id -> JExpr
jsEntryId i = ValExpr (JVar $ jsEntryIdI i)

jsEntryIdI :: Id -> Ident
jsEntryIdI i = jsIdIdent i Nothing "_e"

-- datacon entry, different name than the wrapper
jsDcEntryId :: Id -> JExpr
jsDcEntryId i = ValExpr (JVar $ jsDcEntryIdI i)

jsDcEntryIdI :: Id -> Ident
jsDcEntryIdI i = jsIdIdent i Nothing "_con_e"

jsIdV :: Id -> JVal
jsIdV i = JVar $ jsIdIdent i Nothing ""

jsIdI :: Id -> Ident
jsIdI i = jsIdIdent i Nothing ""

-- some types, Word64, Addr#, unboxed tuple have more than one javascript var
jsIdN :: Id -> Int -> JExpr
jsIdN i n = ValExpr (JVar $ jsIdIdent i (Just n) "")



data ClosureInfo = ClosureInfo
     { ciVar    :: JExpr    -- ^ object being infod
     , ciRegs   :: [StgReg] -- ^ registers with pointers
     , ciName   :: String   -- ^ friendly name for printing
     , ciLayout :: CILayout -- ^ heap/stack layout of the object
     , ciType   :: CIType   -- ^ type of the object, with extra info where required
     , ciStatic :: CIStatic -- ^ static references of this object
     }

data CIType = CIFun { citArity :: Int  -- | number of arguments (double arguments are counted double)
                    , citNVoid :: Int  -- | number of trailing void args
                    }
            | CIThunk
            | CICon { citConstructor :: Int }
            | CIPap { citSize :: Int } -- fixme is this ok?
            | CIBlackhole

data CIStatic = CIStaticParent { staticParent :: Id } -- ^ static refs are stored in parent in fungroup
              | CIStaticRefs   { staticRefs :: [Id] } -- ^ list of refs that need to be kept alive
              | CINoStatic

-- | static refs: array = references, single var = follow parent link, null = nothing to report
instance ToJExpr CIStatic where
  toJExpr CINoStatic         = [je| null |]
  toJExpr (CIStaticParent p) = jsId p
  toJExpr (CIStaticRefs [])  = [je| null |]
  toJExpr (CIStaticRefs rs)  = toJExpr (map idStr rs)
          where
            idStr i = let (StrI xs) = jsIdI i in [je| `xs` |]

data CILayout = CILayoutVariable -- layout stored in object itself, first position from the start
              | CILayoutPtrs     -- size and pointer offsets known
                  { layoutSize :: !Int
                  , layoutPtrs :: [Int] -- offsets of pointer fields
                  }
              | CILayoutFixed    -- whole layout known
                  { layoutSize :: !Int      -- closure size in array positions, including entry
                  , layout     :: [VarType]
                  }

-- standard fixed layout: entry fun + payload
fixedLayout :: [VarType] -> CILayout
fixedLayout vts = CILayoutFixed (1 + sum (map varSize vts)) vts

-- a gi gai i
instance ToStat ClosureInfo where
  toStat (ClosureInfo obj rs name layout CIThunk srefs)       = setObjInfoL obj rs layout Thunk name 0 srefs
  toStat (ClosureInfo obj rs name layout (CIFun arity nvoid) srefs) = setObjInfoL obj rs layout Fun name (mkArityTag arity nvoid) srefs
  toStat (ClosureInfo obj rs name layout (CICon con) srefs)   = setObjInfoL obj rs layout Con name con srefs
  toStat (ClosureInfo obj rs name layout (CIPap size) srefs)  = setObjInfoL obj rs layout Pap name size srefs
  toStat (ClosureInfo obj rs name layout CIBlackhole srefs)   = setObjInfoL obj rs layout Blackhole name 0 srefs -- fixme do we need to keep track of register arguments of underlying thing?

mkArityTag :: Int -> Int -> Int
mkArityTag arity trailingVoid = arity .|. (trailingVoid `shiftL` 8)

setObjInfoL :: JExpr     -- ^ the object
            -> [StgReg]  -- ^ registers with pointers
            -> CILayout  -- ^ layout of the object
            -> CType     -- ^ closure type
            -> String    -- ^ object name, for printing
            -> Int       -- ^ `a' argument, depends on type (arity, conid, size)
            -> CIStatic  -- ^ static refs
            -> JStat
setObjInfoL obj rs CILayoutVariable t n a            = setObjInfo obj t n [] Nothing     a (-1) rs
setObjInfoL obj rs (CILayoutPtrs size ptrs) t n a    = setObjInfo obj t n [] (Just size) a (mkGcTag size ptrs) rs
setObjInfoL obj rs (CILayoutFixed size layout) t n a = setObjInfo obj t n l' (Just size) a (mkGcTag size ptrs) rs
  where
    ptrs = ptrOffsets 0 layout
    l'   = map fromEnum layout
-- fixme: do we need to treat pap gtags differently like we used to? why?

-- the tag thingie, will be 0 if info cannot be read from the tag
mkGcTag :: Int -> [Int] -> Int
mkGcTag objSize ptrs = ptrTag (map (+8) ptrs) .|. objSize

setObjInfo :: JExpr      -- ^ the thing to modify
           -> CType      -- ^ closure type
           -> String     -- ^ object name, for printing
           -> [Int]      -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Maybe Int  -- ^ object size, if static
           -> Int        -- ^ extra 'a' parameter, for constructor tag or arity
           -> Int        -- ^ tag for the garbage collector
           -> [StgReg]      -- ^ pointers in registers
           -> CIStatic   -- ^ static refs
           -> JStat
setObjInfo obj t name fields size a gctag argptrs static =
  [j| _setObjInfo(`obj`, `t`, `name`, `fields`, `a`, `gctag`, `map regNum argptrs`, `static`);  |]

-- generates a list of pointer locations for a list of variable locations starting at start
ptrOffsets :: Int -> [VarType] -> [Int]
ptrOffsets start [] = []
ptrOffsets start (t:ts)
    | isPtr t = start : ptrOffsets (start + varSize t) ts
    | otherwise = ptrOffsets (start + varSize t) ts

ptrTag :: [Int] -> Int
ptrTag ptrs
    | any (>30) ptrs = error "tag bits greater than 30 unsupported"
    | otherwise      = L.foldl' (.|.) 0 (map (1 `shiftL`) $ filter (>=0) ptrs)

shiftedPtrTag :: Int -> [Int] -> Int
shiftedPtrTag shift = ptrTag . map (subtract shift)

