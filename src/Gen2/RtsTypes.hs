{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, TupleSections, CPP #-}
module Gen2.RtsTypes where

import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Types

import           Control.Applicative

import           Gen2.StgAst
import           Gen2.Utils

import           Data.Char                        (toLower)
import           Control.Lens
import           Data.Bits
import qualified Data.List                        as L
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid

import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)
import           Control.Monad.State.Strict

import           DynFlags
import           Encoding
import           Gen2.RtsSettings
import           Id
import           Module
import           Name
import           Outputable hiding ((<>))
import           Panic
import           StgSyn
import           TyCon
import           Type
import           Unique

import           System.IO.Unsafe

showPpr' :: Outputable a => a -> G String
showSDoc' :: SDoc -> G String
#if __GLASGOW_HASKELL__ >= 706
showPpr' a = do
  df <- _gsDynFlags <$> get
  return (showPpr df a)
showSDoc' a = do
  df <- _gsDynFlags <$> get
  return (showSDoc df a)
#else
showPpr' a = return (showPpr a)
showSDoc' a = return (showSDoc a)
#endif

-- closure types
data CType = Thunk | Fun | Pap | Con | Blackhole
  deriving (Show, Eq, Ord, Enum, Bounded)

--
ctNum :: CType -> Int
ctNum Fun       = 1
ctNum Con       = 2
ctNum Thunk     = 0 -- 4
ctNum Pap       = 3 -- 8
-- ctNum Ind       = 4 -- 16
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
             | AddrV    -- a pointer not to the heap: two fields, array + index
             | ObjV     -- some js object, does not contain heap pointers
             | ArrV     -- array of followable values
             | MVarV    -- [value], [tsos waiting]
--             | TVarV
--             | MutVarV -- single element array, ArrV
             | WeakV
               deriving (Eq, Ord, Show, Enum, Bounded)

-- instance ToJExpr VarType where toJExpr = toJExpr . fromEnum

varSize :: VarType -> Int
varSize VoidV = 0
varSize LongV = 2 -- hi, low
varSize AddrV = 2 -- obj/array, offset
varSize _     = 1

typeSize :: Type -> Int
typeSize = sum . map varSize . typeVt

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

-- can this thing contain references to heap objects?
isScannable :: VarType -> [Bool]
isScannable PtrV = [True]
-- isScannable MVarV = [True]
-- isScannable TVarV = [True]
isScannable ArrV = [True]
isScannable x = replicate (varSize x) False

-- go through PrimRep, not CgRep to make Int -> IntV instead of LongV
tyConVt :: TyCon -> VarType
tyConVt = primRepVt . tyConPrimRep

typeVt :: Type -> [VarType]
typeVt t = case repType t of
             UbxTupleRep uts   -> concatMap typeVt uts
             UnaryRep ut       -> [uTypeVt ut]

-- only use if you know it's not an unboxed tuple
uTypeVt :: UnaryType -> VarType
uTypeVt ut
  | isPrimitiveType ut = primTypeVt ut
  | otherwise          = primRepVt . typePrimRep $ ut

primTypeVt :: Type -> VarType
primTypeVt t = case repType t of
                 UnaryRep ut -> case tyConAppTyCon_maybe ut of
                                   Nothing -> panic "primTypeVt: not a TyCon"
                                   Just tc -> go (show tc)
                 _ -> panic "primTypeVt: non-unary type found"
  where
   pr xs = "ghc-prim:GHC.Prim." ++ xs
   go st
    | st == pr "Addr#" = AddrV
    | st == pr "Int#"  = IntV
    | st == pr "Int64#" = LongV
    | st == pr "Char#" = IntV
    | st == pr "Word#" = IntV
    | st == pr "Word64#" = LongV
    | st == pr "Double#" = DoubleV
    | st == pr "Float#" = DoubleV
    | st == pr "Array#" = ArrV
    | st == pr "MutableArray#" = ArrV
    | st == pr "ByteArray#" = ObjV
    | st == pr "MutableByteArray#" = ObjV
    | st == pr "ArrayArray#" = ArrV
    | st == pr "MutableArrayArray#" = ArrV
    | st == pr "MutVar#" = ArrV -- one scannable thing
    | st == pr "TVar#" = ArrV -- one scannable thing, can be null
    | st == pr "MVar#" = ArrV -- one scannable thing, can be null
    | st == pr "State#" = VoidV
    | st == pr "RealWorld" = VoidV
    | st == pr "ThreadId#" = IntV
    | st == pr "Weak#" = WeakV
    | st == pr "StablePtr#" = AddrV
    | st == pr "StableName#" = ObjV
    | st == pr "MutVar#" = ArrV -- MutVarV
    | st == pr "BCO#" = ObjV -- fixme what do we need here?
    | st == pr "~#" = VoidV -- coercion token?
    | st == pr "Any" = PtrV
    | st == "Data.Dynamic.Obj" = PtrV -- ?
    | otherwise = panic ("primTypeVt: unrecognized primitive type: " ++ st)

argVt :: StgArg -> VarType
argVt = uTypeVt . stgArgType

primRepVt :: PrimRep -> VarType
primRepVt VoidRep   = VoidV
primRepVt PtrRep    = PtrV
primRepVt IntRep    = IntV
primRepVt WordRep   = IntV
primRepVt Int64Rep  = LongV
primRepVt Word64Rep = LongV
primRepVt AddrRep   = AddrV
primRepVt FloatRep  = DoubleV
primRepVt DoubleRep = DoubleV


instance ToJExpr VarType where
  toJExpr = toJExpr . fromEnum

-- fixme this is getting out of hand...
data StgReg = R1  | R2  | R3  | R4  | R5  | R6  | R7  | R8
            | R9  | R10 | R11 | R12 | R13 | R14 | R15 | R16
            | R17 | R18 | R19 | R20 | R21 | R22 | R23 | R24
            | R25 | R26 | R27 | R28 | R29 | R30 | R31 | R32
            | R33 | R34 | R35 | R36 | R37 | R38 | R39 | R40
            | R41 | R42 | R43 | R44 | R45 | R46 | R47 | R48
            | R49 | R50 | R51 | R52 | R53 | R54 | R55 | R56
            | R57 | R58 | R59 | R60 | R61 | R62 | R63 | R64
            | R65 | R66 | R67 | R68 | R69 | R70 | R71 | R72
            | R73 | R74 | R75 | R76 | R77 | R78 | R79 | R80
            | R81 | R82 | R83 | R84 | R85 | R86 | R87 | R88
            | R89 | R90 | R91 | R92 | R93 | R94 | R95 | R96
            | R97  | R98  | R99  | R100 | R101 | R102 | R103 | R104
            | R105 | R106 | R107 | R108 | R109 | R110 | R111 | R112
            | R113 | R114 | R115 | R116 | R117 | R118 | R119 | R120
            | R121 | R122 | R123 | R124 | R125 | R126 | R127 | R128
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | return registers
--   extra results from foreign calls can be stored here (first result is returned)
data StgRet = Ret1 | Ret2 | Ret3 | Ret4 | Ret5 | Ret6 | Ret7 | Ret8 | Ret9 | Ret10
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ToJExpr StgReg where
  toJExpr r
    | fromEnum r <= 32 = ve . ("h$"++) . map toLower . show $ r
    | otherwise   = [je| h$regs[`fromEnum r-32`] |]


instance ToJExpr StgRet where
  toJExpr = ve . ("h$"++) . map toLower . show

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

-- | the current code generator state
data GenState = GenState
  { _gsModule   :: Module    -- | the module we're compiling, used for generating names
  , _gsToplevel :: Maybe Id  -- | the top-level function group we're generating
  , _gsScope    :: GenScope  -- | the current lexical environment
  , _gsId       :: Int       -- | integer for the id generator
  , _gsDynFlags :: DynFlags  -- | the DynFlags, used for prettyprinting etc
--  , _gsSRTs     :: Map Id Int
  }

-- | where can we find all Ids
data GenScope = GenScope
  { _stableScope :: Map Id IdScope  -- | how to access an id 
  , _localScope  :: Set Id          -- | set of ids that have a local var binding
  } -- deriving (Eq, Monoid)

emptyScope :: GenScope
emptyScope = GenScope mempty mempty

initState :: DynFlags -> Module -> GenState
initState df m = GenState m Nothing emptyScope 1 df

runGen :: DynFlags -> Module -> G a -> a
runGen df m = flip evalState (initState df m)

{-
-- | run an action in the initial state, useful for making one-off 
runInit :: Module -> G a -> a
runInit
-}

type C   = State GenState JStat
type G a = State GenState a

instance Monoid C where
  mappend = liftM2 (<>)
  mempty  = return mempty

-- | where can we find a single Id
data IdScope = Register !StgReg
             | HeapPtr Id !Int    -- | it's in the heap object for Id, offset Int
--              | StackOffset !Int
             | StackFrame !Int    -- | current stack frame, offset
             | NoEscape Id !Int   -- | no-escape stack frame allocated by id
                                         -- fixme: why not a direct stack-offset?

makeLenses ''GenState
makeLenses ''GenScope

currentModule :: G String
currentModule = moduleNameString . moduleName <$> use gsModule

currentModulePkg :: G String
currentModulePkg = showPpr' =<< use gsModule


-- arguments that the trampoline calls our funcs with
funArgs :: [Ident]
funArgs = [] -- [StrI "o"] -- [StrI "_heap", StrI "_stack"] -- [] -- [StrI "r", StrI "heap", StrI "stack"]

data Special = Stack
             | Sp
             | HTrue
             | HFalse
     deriving (Show, Eq)

instance ToJExpr Special where
  toJExpr Stack  = [je| h$stack |]
  toJExpr Sp     = [je| h$sp    |]
  toJExpr HTrue  = [je| h$t     |]
  toJExpr HFalse = [je| h$f     |]

adjSp :: Int -> JStat
adjSp e = [j| h$sp = h$sp + `e`; |] -- [j| _sp += `e`; sp = _sp; |]

adjSpN :: Int -> JStat
adjSpN e = [j| h$sp = h$sp  - `e`; |] -- [j| _sp = _sp - `e`; sp = _sp; |]

-- stuff that functions are supposed to execute at the start of the body
-- (except very simple functions)
preamble :: JStat
preamble = mempty

push :: [JExpr] -> JStat
push [] = mempty
push xs  
   | rtsInlinePush || l > 32 || l < 2 = [j| `adjSp l`; `items`; |]
   | otherwise                        = ApplStat (toJExpr . StrI $ "h$p" ++ show l) xs
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
closureType c = [je| `c`.f.t |]

isThunk :: JExpr -> JExpr
isThunk c = [je| `c`.f.t === `Thunk` |]
isThunk' f = [je| `f`.t === `Thunk` |]

isFun :: JExpr -> JExpr
isFun c = [je| `c`.f.t === `Fun` |]

isFun' :: JExpr -> JExpr
isFun' f = [je| `f`.t === `Fun` |]

isPap :: JExpr -> JExpr
isPap c = [je| `c`.f.t === `Pap` |]

isPap' :: JExpr -> JExpr
isPap' f = [je| `f`.t === `Pap` |]

isCon :: JExpr -> JExpr
isCon c = [je| `c`.f.t === `Con` |]

isCon' :: JExpr -> JExpr
isCon' f = [je| `f`.t === `Con` |]

-- no ind anymore
-- isInd :: JExpr -> JExpr
-- isInd c = [je| `closureType c` === `Ind` |]

conTag :: JExpr -> JExpr
conTag c = [je| `c`.f.a |]
conTag' f = [je| `f`.a |]

entry :: JExpr -> JExpr
entry p = [je| `p`.f |]

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JExpr -> JExpr
funArity c = [je| `c`.f.a |]

funArity' :: JExpr -> JExpr
funArity' f = [je| `f`.a |]

-- expects heap pointer to entry (fixme document this better or make typesafe)
-- arity & 0xff = real number of arguments
-- arity >> 8   = number of trailing void
papArity :: JExpr -> JExpr -> JStat
papArity tgt p = [j| `tgt` = 0;
                     var cur = `p`;
                     var args = 0;
                     var regs = 0;
                     do {
                       regs += cur.f.a;
                       args += cur.d2.d1;
                       `traceRts $ "pap: " |+ regs |+ " " |+ args`;
                       cur = cur.d1;
                     } while(cur.f.t === `Pap`);
                     var fa = cur.f.a;
                     `traceRts $ "pap base: " |+ fa`;
                     `tgt` = (((fa>>8)-regs)<<8)|((fa&0xFF)-args);
                     `traceRts $ "pap arity: " |+ tgt`;
                   |]
{-
papArgs :: JExpr -> JExpr
papArgs p = [je| `p`.f.a |]
-}
--papArity tgt p = [j| `tgt` = `p`.f.a |]

-- number of stored args in pap (fixme?)
{-

papArgs p = [je| `Heap`[`p`].a |]
-}

-- funTag :: JExpr -> JExpr
-- funTag c = [je| `c`.gtag |]

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

jsIdIdent :: Id -> Maybe Int -> String -> G Ident
jsIdIdent i mn suffix = do
  (prefix, u) <- mkPrefixU
  StrI . (\x -> "h$"++prefix++x++mns++suffix++u) . zEncodeString <$> name
    where
      mns = maybe "" (('_':).show) mn
--      (prefix,u)
      name = fmap ('.':) . showPpr' . localiseName . getName $ i
      mkPrefixU
        | isExportedId i, Just x <- (nameModule_maybe . getName) i = do
           xstr <- showPpr' x
           return (zEncodeString xstr, "")
        | otherwise = (,('_':) . show . getKey . getUnique $ i) . ('_':) . zEncodeString
                        <$> currentModulePkg

jsVar :: String -> JExpr
jsVar v = ValExpr . JVar . StrI $ v

isBoolId :: Id -> Bool
isBoolId i = isTrueCon i || isFalseCon i

-- fixme remove dependency on the Show instances
isTrueCon :: Id -> Bool
isTrueCon i = show (idName i) == "ghc-prim:GHC.Types.True"

isFalseCon :: Id -> Bool
isFalseCon i = show (idName i) == "ghc-prim:GHC.Types.False"

-- regular id, shortcut for bools!
jsId :: Id -> G JExpr
jsId i
  | isTrueCon i  = return $ toJExpr HTrue
  | isFalseCon i = return $ toJExpr HFalse
  | otherwise = ValExpr . JVar <$> jsIdIdent i Nothing ""

-- entry id
jsEnId :: Id -> G JExpr
jsEnId i = ValExpr . JVar <$> jsEnIdI i

jsEnIdI :: Id -> G Ident
jsEnIdI i = jsIdIdent i Nothing "_e"

jsEntryId :: Id -> G JExpr
jsEntryId i = ValExpr . JVar <$> jsEntryIdI i

jsEntryIdI :: Id -> G Ident
jsEntryIdI i = jsIdIdent i Nothing "_e"

-- datacon entry, different name than the wrapper
jsDcEntryId :: Id -> G JExpr
jsDcEntryId i = ValExpr . JVar <$> jsDcEntryIdI i

jsDcEntryIdI :: Id -> G Ident
jsDcEntryIdI i = jsIdIdent i Nothing "_con_e"

jsIdV :: Id -> G JVal
jsIdV i = JVar <$> jsIdIdent i Nothing ""

jsIdI :: Id -> G Ident
jsIdI i = jsIdIdent i Nothing ""

-- some types, Word64, Addr#, unboxed tuple have more than one javascript var
jsIdIN :: Id -> Int -> G Ident
jsIdIN i n = jsIdIdent i (Just n) ""

jsIdN :: Id -> Int -> G JExpr
jsIdN i n = ValExpr . JVar <$> jsIdIdent i (Just n) ""



data ClosureInfo = ClosureInfo
     { ciVar    :: JExpr     -- ^ object being infod
     , ciRegs   :: [VarType] -- ^ things in registers
     , ciName   :: String    -- ^ friendly name for printing
     , ciLayout :: CILayout  -- ^ heap/stack layout of the object
     , ciType   :: CIType    -- ^ type of the object, with extra info where required
     , ciStatic :: CIStatic  -- ^ static references of this object
     }

data CIType = CIFun { citArity :: Int  -- | function arity
                    , citRegs  :: Int  -- | number of registers for the args
                    }
            | CIThunk
            | CICon { citConstructor :: Int }
            | CIPap { citSize :: Int } -- fixme is this ok?
            | CIBlackhole
  --          | CIInd

data CIStatic = -- CIStaticParent { staticParent :: Ident } -- ^ static refs are stored in parent in fungroup
                CIStaticRefs   { staticRefs :: [Ident] } -- ^ list of refs that need to be kept alive
              | CINoStatic

-- | static refs: array = references, single var = follow parent link, null = nothing to report
instance ToJExpr CIStatic where
  toJExpr CINoStatic         = [je| null |]
--  toJExpr (CIStaticParent p) = iex p -- jsId p
  toJExpr (CIStaticRefs [])  = [je| null |]
  toJExpr (CIStaticRefs rs)  = [je| \ -> `toJExpr rs` |] --- (map istr rs) -- (map idStr rs)


data CILayout = CILayoutVariable -- layout stored in object itself, first position from the start
              | CILayoutPtrs     -- size and pointer offsets known, no other primitive things (like IORef, MVar, Array) inside
                  { layoutSize :: !Int
                  , layoutPtrs :: [Int] -- offsets of pointer fields
                  }
              | CILayoutFixed    -- whole layout known
                  { layoutSize :: !Int      -- closure size in array positions, including entry
                  , layout     :: [VarType]
                  }

-- standard fixed layout: payload size
fixedLayout :: [VarType] -> CILayout
fixedLayout vts = CILayoutFixed (sum (map varSize vts)) vts

-- a gi gai i
instance ToStat ClosureInfo where
  toStat (ClosureInfo obj rs name layout CIThunk srefs)       =
    setObjInfoL obj rs layout Thunk name 0 srefs
  toStat (ClosureInfo obj rs name layout (CIFun arity nregs) srefs) =
    setObjInfoL obj rs layout Fun name (mkArityTag arity nregs) srefs
  toStat (ClosureInfo obj rs name layout (CICon con) srefs)   =
    setObjInfoL obj rs layout Con name con srefs
--  toStat (ClosureInfo obj rs name layout CIInd srefs)         =
--    setObjInfoL obj rs layout Ind name 0 srefs -- fixme do we need to keep track of register arguments of underlying thing?
  toStat (ClosureInfo obj rs name layout CIBlackhole srefs)   =
    setObjInfoL obj rs layout Blackhole name 0 srefs

  -- pap have a special gtag for faster gc ident (only need to access gtag field)
  toStat (ClosureInfo obj rs name layout (CIPap size) srefs)  =
    setObjInfo obj Pap name [] size (toJExpr $ -3-size) rs srefs
-- setObjInfo obj rs layout Pap name size srefs


mkArityTag :: Int -> Int -> Int
mkArityTag arity trailingVoid = arity .|. (trailingVoid `shiftL` 8)

-- tag repurposed as size
setObjInfoL :: JExpr     -- ^ the object
            -> [VarType] -- ^ things in registers
            -> CILayout  -- ^ layout of the object
            -> CType     -- ^ closure type
            -> String    -- ^ object name, for printing
            -> Int       -- ^ `a' argument, depends on type (arity, conid, size)
            -> CIStatic  -- ^ static refs
            -> JStat
setObjInfoL obj rs CILayoutVariable t n a            =
  setObjInfo obj t n [] a (toJExpr (-1 :: Int)) rs
setObjInfoL obj rs (CILayoutPtrs size ptrs) t n a    =
  setObjInfo obj t n xs a tag rs
  where
    tag = toJExpr size  -- mkGcTagPtrs t size ptrs
    xs -- | tag /= 0  = []
       = map (\i -> fromEnum $ if i `elem` ptrs then PtrV else ObjV) [1..size-1]
setObjInfoL obj rs (CILayoutFixed size layout) t n a = setObjInfo obj t n xs a tag rs
  where
    tag  = toJExpr size -- mkGcTag t size layout
    xs   = toTypeList layout
       {-
       | tag /= 0 = []
       | otherwise = toTypeList layout
       -}
mkGcTag :: CType -> Int -> [VarType] -> JExpr
mkGcTag t size vs = mkGcTagPtrs t size (scannableOffsets 0 vs)
{-
   | any (\v -> isScannable v && not (v==PtrV)) vs = 0
  | otherwise = 
-}

toTypeList :: [VarType] -> [Int]
toTypeList = concatMap (\x -> replicate (varSize x) (fromEnum x))

-- the tag thingie, will be 0 if info cannot be read from the tag
mkGcTagPtrs :: CType -> Int -> [Int] -> JExpr
mkGcTagPtrs t objSize ptrs = fromMaybe fallback $
                           toJExpr . (.|. objSize') <$> ptrTag (map (+8) ptrs)
  where
    -- if tag bits don't fit in an integer, set tag to 0, gc will use the info list
    fallback = toJExpr (0::Int) -- (objSize' : ptrs)
    objSize' = if t == Thunk then max 2 objSize else objSize

setObjInfo :: JExpr      -- ^ the thing to modify
           -> CType      -- ^ closure type
           -> String     -- ^ object name, for printing
           -> [Int]      -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Int        -- ^ extra 'a' parameter, for constructor tag or arity
           -> JExpr      -- ^ tag for the garbage collector
           -> [VarType]  -- ^ things in registers
           -> CIStatic   -- ^ static refs
           -> JStat
setObjInfo obj t name fields a gctag argptrs static
  | rtsDebug  = [j| h$setObjInfo(`obj`, `t`, `name`, `fields`, `a`, `gctag`, `nregs`, `static`);  |]
  | otherwise = [j| h$o(`obj`,`t`,`a`,`gctag`,`nregs`,`static`); |]
  where
    nregs = sum $ map varSize argptrs

scannableOffsets :: Int -> [VarType] -> [Int]
scannableOffsets start ts =
  [o | (o,True) <- zip [start..] (concatMap isScannable ts) ]

-- generates a list of pointer locations for a list of variable locations starting at start
{-
ptrOffsets :: Int -> [VarType] -> [Int]
ptrOffsets start [] = []
ptrOffsets start (t:ts)
    | isPtr t = start : ptrOffsets (start + varSize t) ts
    | otherwise = ptrOffsets (start + varSize t) ts
-}

ptrTag :: [Int] -> Maybe Int
ptrTag ptrs
    | any (>30) ptrs = Nothing -- error "tag bits greater than 30 unsupported"
    | otherwise      = Just $ L.foldl' (.|.) 0 (map (1 `shiftL`) $ filter (>=0) ptrs)

shiftedPtrTag :: Int -> [Int] -> Maybe Int
shiftedPtrTag shift = ptrTag . map (subtract shift)

-- | generate all js vars for the ids (can be multiple per var)
genIds :: Id -> G [JExpr]
genIds i
  | s == 0    = return mempty
  | s == 1    = (:[]) <$> jsId i
  | otherwise = mapM (jsIdN i) [1..s]
  where
    s  = varSize vt
    vt = uTypeVt . idType $ i

-- | get all idents for an id
genIdsI :: Id -> G [Ident]
genIdsI i
  | s == 1    = (:[]) <$> jsIdI i
  | otherwise = mapM (jsIdIN i) [1..s]
        where
          s = varSize . uTypeVt . idType $ i

-- | declare all js vars for the id, cannot be an unboxed tuple
declIds :: Id -> C
declIds  i
  | s == 0    = return mempty
  | s == 1    = decl <$> jsIdI i
  | otherwise = mconcat <$> mapM (\n -> decl <$> jsIdIN i n) [1..s]
  where
    s  = varSize vt
    vt = uTypeVt . idType $ i
