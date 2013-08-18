{-# LANGUAGE QuasiQuotes,
             TemplateHaskell,
             TypeSynonymInstances,
             FlexibleInstances,
             TupleSections,
             CPP #-}

module Gen2.RtsTypes where

import           Language.Javascript.JMacro

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict

import           Data.Array   (Array, (!), listArray)
import           Data.Char    (toLower)
import           Data.Bits
import           Data.IntMap  (IntMap)
import qualified Data.IntMap  as IM
import           Data.Ix
import qualified Data.List    as L
import qualified Data.Map     as M
import           Data.Maybe   (fromMaybe)
import           Data.Monoid
import           Data.Text    (Text)
import qualified Data.Text    as T

import           DataCon
import           DynFlags
import           Encoding
import           Id
import           Module
import           Name
import           Outputable hiding ((<>))
import           Panic
import           StgSyn
import           TyCon
import           Type
import           Unique

import           Gen2.ClosureInfo
import qualified Gen2.Object as Object
import           Gen2.RtsSettings
import           Gen2.StgAst
import           Gen2.Utils



-- showPpr' :: Outputable a => a -> G String
-- showSDoc' :: SDoc -> G String
showPpr' a = do
  df <- _gsDynFlags <$> get
  return (showPpr df a)
showSDoc' a = do
  df <- _gsDynFlags <$> get
  return (showSDoc df a)


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
  deriving (Eq, Ord, Show, Enum, Bounded, Ix)

-- | return registers
--   extra results from foreign calls can be stored here (first result is returned)
data StgRet = Ret1 | Ret2 | Ret3 | Ret4 | Ret5 | Ret6 | Ret7 | Ret8 | Ret9 | Ret10
  deriving (Eq, Ord, Show, Enum, Bounded, Ix)

instance ToJExpr StgReg where
  toJExpr = (registers!)

-- only the registers that have a single ident
registersI :: Array StgReg Ident
registersI = listArray (minBound, R32) (map (ri.(registers!)) $ enumFromTo R1 R32)
  where
    ri (ValExpr (JVar i)) = i
    ri _                  = error "registersI: not an ident"

registers :: Array StgReg JExpr
registers = listArray (minBound, maxBound) (map regN (enumFrom R1))
  where
    regN r
      | fromEnum r <= 32 = ValExpr . JVar . TxtI . T.pack . ("h$"++) . map toLower . show $ r
      | otherwise        = [je| h$regs[`fromEnum r-32`] |]

instance ToJExpr StgRet where
  toJExpr r = ValExpr (JVar (rets!r))

rets :: Array StgRet Ident
rets = listArray (minBound, maxBound) (map retN (enumFrom Ret1))
  where
    retN = TxtI . T.pack . ("h$"++) . map toLower . show

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

data IdType = IdPlain | IdEntry | IdConEntry deriving (Enum, Eq, Ord, Show)
data IdKey = IdKey !Int !Int !IdType deriving (Eq, Ord)
newtype IdCache = IdCache (M.Map IdKey Ident)

emptyIdCache = IdCache M.empty

-- | the current code generator state
data GenState = GenState
  { _gsModule        :: Module        -- | the module we're compiling, used for generating names
  , _gsToplevel      :: Maybe Id      -- | the toplevel function group we're generating
  , _gsToplevelStats :: [JStat]       -- | extra toplevel statements that our current function emits
  , _gsClosureInfo   :: [ClosureInfo] -- | closure information in the current function group
  , _gsId            :: Int           -- | integer for the id generator
  , _gsDynFlags      :: DynFlags      -- | the DynFlags, used for prettyprinting etc
  , _gsStack         :: [StackSlot]   -- | what's currently on the stack, above h$sp
  , _gsIdents        :: IdCache       -- | hash consing for identifiers from a Unique
  }

type C   = State GenState JStat
type G a = State GenState a

data StackSlot = SlotId Id Int
               | SlotUnknown
  deriving (Eq, Ord, Show)

makeLenses ''GenState

emitToplevel :: JStat -> G ()
emitToplevel s = gsToplevelStats %= (s:)

resetToplevel :: G ()
resetToplevel = do
  gsToplevelStats .= []
  gsClosureInfo   .= []

emitClosureInfo :: ClosureInfo -> G ()
emitClosureInfo ci = gsClosureInfo %= (ci:)

dropSlots :: Int -> G ()
dropSlots n = gsStack %= drop n

addSlots :: [StackSlot] -> G ()
addSlots xs = gsStack %= (xs++)

resetSlots :: G a -> G a
resetSlots m = do
  s <- getSlots
  setSlots []
  a <- m
  setSlots s
  return a

isolateSlots :: G a -> G a
isolateSlots m = do
  s <- getSlots
  a <- m
  setSlots s
  return a

setSlots :: [StackSlot] -> G ()
setSlots xs = gsStack .= xs

getSlots :: G [StackSlot]
getSlots = use gsStack

addUnknownSlots :: Int -> G ()
addUnknownSlots n = addSlots (replicate n SlotUnknown)

-- run the computation with the current stack slots, restore stack slots afterwards
withStack :: G a -> G a
withStack m = do
  s <- getSlots
  r <- m
  setSlots s
  return r

encodeUnique :: Int -> String
encodeUnique = reverse . go  -- reversed is more compressible
  where
    go n | n < 0  = '_' : encodeUnique (negate n)
         | n > 61 = let (q,r) = n `quotRem` 62
                          in  chars ! r : encodeUnique q
               | otherwise = [chars ! n]

    chars = listArray (0,61) (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

{-
-- | where can we find all Ids
data GenScope = GenScope
  { _stableScope :: Map Id IdScope  -- | how to access an id 
  , _localScope  :: Set Id          -- | set of ids that have a local var binding
  } -- deriving (Eq, Monoid)
-}
-- emptyScope :: GenScope
-- emptyScope = GenScope mempty mempty

initState :: DynFlags -> Module -> GenState
initState df m = GenState m Nothing [] [] 1 df [] emptyIdCache

runGen :: DynFlags -> Module -> G a -> a
runGen df m = flip evalState (initState df m)

{-
-- | run an action in the initial state, useful for making one-off 
runInit :: Module -> G a -> a
runInit
-}

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

-- makeLenses ''GenScope

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
  toJExpr HTrue  = [je| true    |]
  toJExpr HFalse = [je| false   |]

adjSp :: Int -> JStat
adjSp e = [j| h$sp = h$sp + `e`; |]

adjSpN :: Int -> JStat
adjSpN e = [j| h$sp = h$sp  - `e`; |]

-- stuff that functions are supposed to execute at the start of the body
-- (except very simple functions)
preamble :: JStat
preamble = mempty

pushN :: Array Int Ident
pushN = listArray (1,32) $ map (TxtI . T.pack . ("h$p"++) . show) [1..32]

pushN' :: Array Int JExpr
pushN' = fmap (ValExpr . JVar) pushN

pushNN :: Array Integer Ident
pushNN = listArray (1,255) $ map (TxtI . T.pack . ("h$pp"++) . show) [1..255]

pushNN' :: Array Integer JExpr
pushNN' = fmap (ValExpr . JVar) pushNN

-- optimized push that reuses existing values on stack
-- slots are True if the right value is already there
pushOptimized :: [(JExpr,Bool)] -> C
pushOptimized [] = return mempty
pushOptimized xs = dropSlots l >> return go
  where
    go
     | rtsInlinePush               = inlinePush
     | all snd xs                  = adjSp l
     | all (not.snd) xs && l <= 32 =
        ApplStat (pushN' ! l) (map fst xs)
     | l <= 8 && not (snd $ last xs) =
        ApplStat (pushNN' ! sig) [ e | (e,False) <- xs ]
     | otherwise = inlinePush
    l   = length xs
    sig :: Integer
    sig = L.foldl1' (.|.) $ zipWith (\(e,b) i -> if not b then bit i else 0) xs [0..]
    inlinePush = adjSp l <> mconcat (zipWith pushSlot [1..] xs)
    pushSlot i (e,False) = [j| `Stack`[`offset i`] = `e` |]
    pushSlot _ _         = mempty
    offset i | i == l    = [je| `Sp` |]
             | otherwise = [je| `Sp` - `l-i` |]

push :: [JExpr] -> C
push xs = dropSlots (length xs) >> return (push' xs)

push' :: [JExpr] -> JStat
push' [] = mempty
push' xs
   | rtsInlinePush || l > 32 || l < 2 = adjSp l <> mconcat items
   | otherwise                        = ApplStat (toJExpr $ pushN ! l) xs
  where
    items = zipWith (\i e -> [j| `Stack`[`offset i`] = `e`; |]) [(1::Int)..] xs
    offset i | i == l    = [je| `Sp` |]
             | otherwise = [je| `Sp` - `l-i` |]
    l = length xs

popUnknown :: [JExpr] -> C
popUnknown xs = popSkipUnknown 0 xs

popSkipUnknown :: Int -> [JExpr] -> C
popSkipUnknown n xs = popSkip n (map (,SlotUnknown) xs)

pop :: [(JExpr,StackSlot)] -> C
pop = popSkip 0

-- pop the expressions, but ignore the top n elements of the stack
popSkip :: Int -> [(JExpr,StackSlot)] -> C
popSkip 0 [] = mempty
popSkip n [] = addUnknownSlots n >> return (adjSpN n)
popSkip n xs = do
  addUnknownSlots n
  addSlots (map snd xs)
  return (loadSkip n (map fst xs) <> adjSpN (length xs + n))

-- pop things, don't upstate stack knowledge
popSkip' :: Int -> [JExpr] -> JStat
popSkip' 0 [] = mempty
popSkip' n [] = adjSpN n
popSkip' n xs = loadSkip n xs <> adjSpN (length xs + n)

-- like popSkip, but without modifying sp
loadSkip :: Int -> [JExpr] -> JStat
loadSkip n xs = mconcat items
    where
      items = reverse $ zipWith (\i e -> [j| `e` = `Stack`[`offset (i+n)`]; |]) [(0::Int)..] (reverse xs)
      offset 0 = [je| `Sp` |]
      offset n = [je| `Sp` - `n` |]

-- debugPop e@(ValExpr (JVar (StrI i))) offset = [j| log("popped: " + `i`  + " -> " + `e`) |]
-- debugPop _ _ = mempty

-- declare and pop
popSkipI :: Int -> [(Ident,StackSlot)] -> C
popSkipI 0 [] = mempty
popSkipI n [] = return (adjSpN n)
popSkipI n xs = do
  addUnknownSlots n
  addSlots (map snd xs)
  return (loadSkipI n (map fst xs) <> adjSpN (length xs + n))

-- like popSkip, but without modifying sp
loadSkipI :: Int -> [Ident] -> JStat
loadSkipI n xs = mconcat items
    where
      items = reverse $ zipWith f [(0::Int)..] (reverse xs)
      offset 0 = [je| `Sp` |]
      offset n = [je| `Sp` - `n` |]
      f i e = [j| `decl e`;
                  `e` = `Stack`[`offset (i+n)`];
                |]

popn :: Int -> C
popn n = addUnknownSlots n >> return (adjSpN n)

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

jsIdIdent :: Id -> Maybe Int -> IdType -> G Ident
jsIdIdent i mi suffix = do
  IdCache cache <- use gsIdents
  let key = IdKey (getKey . getUnique $ i) (fromMaybe 0 mi) suffix
  case M.lookup key cache of
    Just ident -> return ident
    Nothing -> do
      ident <- jsIdIdent' i mi suffix
      gsIdents .= IdCache (M.insert key ident cache)
      return ident

jsIdIdent' :: Id -> Maybe Int -> IdType -> G Ident
jsIdIdent' i mn suffix0 = do
  (prefix, u) <- mkPrefixU
  i' <- (\x -> T.pack $ "h$"++prefix++x++mns++suffix++u) . zEncodeString <$> name
  i' `seq` return (TxtI i') 
    where
      suffix = idTypeSuffix suffix0
      mns = maybe "" (('_':).show) mn
      name = fmap ('.':) . showPpr' . localiseName . getName $ i
      mkPrefixU
        | isExportedId i, Just x <- (nameModule_maybe . getName) i = do
           xstr <- showPpr' x
           return (zEncodeString xstr, "")
        | otherwise = (,('_':) . encodeUnique . getKey . getUnique $ i) . ('$':) . zEncodeString
                        <$> currentModulePkg

idTypeSuffix :: IdType -> String
idTypeSuffix IdPlain = ""
idTypeSuffix IdEntry = "_e"
idTypeSuffix IdConEntry = "_con_e"

jsVar :: String -> JExpr
jsVar v = ValExpr . JVar . TxtI . T.pack $ v

-- regular id, shortcut for bools!
jsId :: Id -> G JExpr
jsId i
--  | isTrueCon i  = return $ toJExpr HTrue
--  | isFalseCon i = return $ toJExpr HFalse
  | otherwise = ValExpr . JVar <$> jsIdIdent i Nothing IdPlain

-- entry id
jsEnId :: Id -> G JExpr
jsEnId i = ValExpr . JVar <$> jsEnIdI i

jsEnIdI :: Id -> G Ident
jsEnIdI i = jsIdIdent i Nothing IdEntry

jsEntryId :: Id -> G JExpr
jsEntryId i = ValExpr . JVar <$> jsEntryIdI i

jsEntryIdI :: Id -> G Ident
jsEntryIdI i = jsIdIdent i Nothing IdEntry

-- datacon entry, different name than the wrapper
jsDcEntryId :: Id -> G JExpr
jsDcEntryId i = ValExpr . JVar <$> jsDcEntryIdI i

jsDcEntryIdI :: Id -> G Ident
jsDcEntryIdI i = jsIdIdent i Nothing IdConEntry

jsIdV :: Id -> G JVal
jsIdV i = JVar <$> jsIdIdent i Nothing IdPlain

jsIdI :: Id -> G Ident
jsIdI i = jsIdIdent i Nothing IdPlain

-- some types, Word64, Addr#, unboxed tuple have more than one javascript var
jsIdIN :: Id -> Int -> G Ident
jsIdIN i n = jsIdIdent i (Just n) IdPlain

jsIdN :: Id -> Int -> G JExpr
jsIdN i n = ValExpr . JVar <$> jsIdIdent i (Just n) IdPlain

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
