{-# LANGUAGE CPP,
             QuasiQuotes,
             TemplateHaskell,
             TypeSynonymInstances,
             FlexibleInstances,
             TupleSections,
             OverloadedStrings #-}

module Gen2.RtsTypes where

import           DynFlags
import           Encoding
import           Id
import           Module
import           Name
import           Outputable hiding ((<>))
import           StgSyn
import           Unique
import           UniqFM
import           SrcLoc
import           ForeignCall (Safety(..), CCallConv(..))
import           FastString

import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.State.Strict

import           Data.Array   (Array, (!), listArray)
import           Data.Bits
import           Data.Char    (toLower)
import           Data.Default
import           Data.Ix
import qualified Data.List    as L
import qualified Data.Map     as M
import           Data.Maybe   (fromMaybe, isJust)
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.Text    (Text)
import qualified Data.Text    as T

import           Compiler.Compat
import           Compiler.JMacro
import           Compiler.Utils

import           Gen2.ClosureInfo
import           Gen2.Utils

traceRts :: ToJExpr a => CgSettings -> a -> JStat
traceRts s e = jStatIf (csTraceRts s) [j| h$log(`e`); |]

assertRts :: ToJExpr a => CgSettings -> JExpr -> a -> JStat
assertRts s e m = jStatIf (csAssertRts s) [j| if(!`e`) { throw `m`; } |]

jStatIf :: Bool -> JStat -> JStat
jStatIf True s = s
jStatIf _    _ = mempty

clName :: JExpr -> JExpr
clName c = [je| `c`.n |]

clTypeName :: JExpr -> JExpr
clTypeName c = [je| h$closureTypeName(`c`.t) |]

infixr 1 |+
infixr 1 |-
infixl 3 |.
infixl 2 |!
infixl 2 |!!

-- a + b
(|+) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|+) e1 e2 = [je| `e1` + `e2` |]

-- a - b
(|-) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|-) e1 e2 = [je| `e1` - `e2` |]

-- a & b
(|&) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|&) e1 e2 = [je| `e1` & `e2` |]

-- a.b
(|.) :: ToJExpr a => a -> Text -> JExpr
(|.) e i = SelExpr (toJExpr e) (TxtI i)

-- a[b]
(|!) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|!) e i = [je| `e`[`i`] |]

-- a[b] with b int
(|!!) :: ToJExpr a => a -> Int -> JExpr
(|!!) = (|!)

-- a(b1,b2,...)
(|^) :: ToJExpr a => a -> [JExpr] -> JExpr
(|^) a bs = ApplExpr (toJExpr a) bs

(|^^) :: Text -> [JExpr] -> JExpr
(|^^) a bs = ApplExpr (jsv a) bs

(|||) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|||) a b = [je| `a` || `b` |]

(|&&) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|&&) a b = [je| `a` && `b` |]

(|===) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|===) a b = [je| `a` === `b` |]

(|!==) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|!==) a b = [je| `a` !== `b` |]

infix 7 |=
(|=) :: ToJExpr a => Ident -> a -> JStat
(|=) i b = AssignStat (toJExpr i) (toJExpr b)

infix 7 ||=
(||=) :: ToJExpr a => Ident -> a -> JStat
(||=) i b = decl i <> i |= b

showPpr' :: Outputable a => a -> G String
showPpr' a = do
  df <- _gsDynFlags <$> get
  return (showPpr df a)

showSDoc' :: SDoc -> G String
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
      | fromEnum r < 32 = ValExpr . JVar . TxtI . T.pack . ("h$"++) . map toLower . show $ r
      | otherwise       = [je| h$regs[`fromEnum r-32`] |]

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

newtype GlobalIdCache = GlobalIdCache (M.Map Ident (IdKey, Id))

emptyGlobalIdCache :: GlobalIdCache
emptyGlobalIdCache = GlobalIdCache M.empty

data OtherSymb = OtherSymb !Module !Text
  deriving (Ord, Eq, Show)

emptyIdCache :: IdCache
emptyIdCache = IdCache M.empty

data GenState = GenState
  { _gsSettings      :: CgSettings     -- ^ codegen settings, read-only
  , _gsModule        :: !Module         -- ^ current module
  , _gsDynFlags      :: DynFlags       -- ^ dynamic flags
  , _gsId            :: !Int            -- ^ unique number for the id generator
  , _gsIdents        :: !IdCache        -- ^ hash consing for identifiers from a Unique
  , _gsUnfloated     :: !(UniqFM StgExpr) -- ^ unfloated arguments
  , _gsGroup         :: GenGroupState  -- ^ state for the current binding group
  , _gsGlobal        :: [JStat]        -- ^ global (per module) statements (gets included when anything else from the module is used)
  }

-- | the state relevant for the current binding group
data GenGroupState = GenGroupState
  { _ggsToplevelStats :: [JStat]        -- ^ extra toplevel statements for the binding group
  , _ggsClosureInfo   :: [ClosureInfo]  -- ^ closure metadata (info tables) for the binding group
  , _ggsStatic        :: [StaticInfo]   -- ^ static (CAF) data in our binding group
  , _ggsStack         :: [StackSlot]    -- ^ stack info for the current expression
  , _ggsStackDepth    :: Int            -- ^ current stack depth
  , _ggsExtraDeps     :: Set OtherSymb  -- ^ extra dependencies for the linkable unit that contains this group
  , _ggsGlobalIdCache :: GlobalIdCache
  , _ggsForeignRefs   :: [ForeignRef]
--  , _ggsGlobalRefs    :: [[Id]]
  }

instance Default GenGroupState where
  def = GenGroupState [] [] [] [] 0 S.empty emptyGlobalIdCache [] -- []

type C = State GenState JStat
type G = State GenState

data StackSlot = SlotId !Id !Int
               | SlotUnknown
  deriving (Eq, Ord, Show)

makeLenses ''GenGroupState
makeLenses ''GenState

assertRtsStat :: C -> C
assertRtsStat stat = do
  s <- use gsSettings
  if csAssertRts s then stat else mempty

-- | emit a global (for the current module) toplevel statement
emitGlobal :: JStat -> G ()
emitGlobal s = gsGlobal %= (s:)

-- functions below modify the current binding group state

-- | start with a new binding group
resetGroup :: G ()
resetGroup = gsGroup .= def

-- | add a dependency on a particular symbol to the current group
addDependency :: OtherSymb -> G ()
addDependency symbol = gsGroup . ggsExtraDeps %= (S.insert symbol)

-- | emit a top-level statement for the current binding group
emitToplevel :: JStat -> G ()
emitToplevel s = gsGroup . ggsToplevelStats %= (s:)

-- | add closure info in our binding group. all heap objects must have closure info
emitClosureInfo :: ClosureInfo -> G ()
emitClosureInfo ci = gsGroup . ggsClosureInfo %= (ci:)

-- | emit static data for the binding group
emitStatic :: Text -> StaticVal -> Maybe Ident -> G ()
emitStatic ident val cc = gsGroup . ggsStatic %= (StaticInfo ident val cc :)

emitForeign :: Maybe RealSrcSpan
            -> Text
            -> Safety
            -> CCallConv
            -> [Text]
            -> Text
            -> G ()
emitForeign mbSpan pattern safety cconv arg_tys res_ty =
  gsGroup . ggsForeignRefs %= (ForeignRef spanTxt pattern safety cconv arg_tys res_ty :)
  where
    spanTxt = case mbSpan of
                Just sp -> T.pack $
                  unpackFS (srcSpanFile sp) ++
                  " " ++
                  show (srcSpanStartLine sp, srcSpanStartCol sp) ++
                  "-" ++
                  show (srcSpanEndLine sp, srcSpanEndCol sp)
                Nothing -> "<unknown>"

adjPushStack :: Int -> G ()
adjPushStack n = do
  stackDepth += n
  dropSlots n

dropSlots :: Int -> G ()
dropSlots n = gsGroup . ggsStack %= drop n

-- | add knowledge about the stack slots
addSlots :: [StackSlot] -> G ()
addSlots xs = gsGroup . ggsStack %= (xs++)

stackDepth :: Lens' GenState Int
stackDepth = gsGroup . ggsStackDepth

----------------------------------------------------------

-- | run the action with no stack info
resetSlots :: G a -> G a
resetSlots m = do
  s <- getSlots
  d <- use stackDepth
  setSlots []
  a <- m
  setSlots s
  stackDepth .= d
  return a

-- | run the action with current stack info, but don't let modifications propagate
isolateSlots :: G a -> G a
isolateSlots m = do
  s <- getSlots
  d <- use stackDepth
  a <- m
  setSlots s
  stackDepth .= d
  return a

-- | overwrite our stack knowledge
setSlots :: [StackSlot] -> G ()
setSlots xs = gsGroup . ggsStack .= xs

-- | retrieve our current stack knowledge
getSlots :: G [StackSlot]
getSlots = use (gsGroup . ggsStack)

-- | add `n` unknown slots to our stack knowledge
addUnknownSlots :: Int -> G ()
addUnknownSlots n = addSlots (replicate n SlotUnknown)

throwSimpleSrcErr :: DynFlags -> SrcSpan -> String -> G a
throwSimpleSrcErr df span msg = return $! Ex.throw (simpleSrcErr df span msg)

initState :: DynFlags -> Module -> UniqFM StgExpr -> GenState
initState df m unfloat =
  GenState (dfCgSettings df) m df 1 emptyIdCache unfloat def []

runGen :: DynFlags -> Module -> UniqFM StgExpr -> G a -> a
runGen df m unfloat = flip evalState (initState df m unfloat)

instance Semigroup C where
  (<>) = liftM2 (<>)

instance Monoid C where
  mempty  = return mempty

data Special = Stack
             | Sp
     deriving (Show, Eq)

instance ToJExpr Special where
  toJExpr Stack  = [je| h$stack |]
  toJExpr Sp     = [je| h$sp    |]

adjSp' :: Int -> JStat
adjSp' 0 = mempty
adjSp' e = [j| `Sp` = `Sp` + `e`; |]

adjSpN' :: Int -> JStat
adjSpN' 0 = mempty
adjSpN' e = [j| `Sp` = `Sp`  - `e`; |]

adjSp :: Int -> C
adjSp 0 = return mempty
adjSp e = stackDepth += e >> return [j| `Sp` = `Sp` + `e`; |]

adjSpN :: Int -> C
adjSpN 0 = return mempty
adjSpN e = stackDepth -= e >> return [j| `Sp` = `Sp`  - `e`; |]

pushN :: Array Int Ident
pushN = listArray (1,32) $ map (TxtI . T.pack . ("h$p"++) . show) [(1::Int)..32]

pushN' :: Array Int JExpr
pushN' = fmap (ValExpr . JVar) pushN

pushNN :: Array Integer Ident
pushNN = listArray (1,255) $ map (TxtI . T.pack . ("h$pp"++) . show) [(1::Int)..255]

pushNN' :: Array Integer JExpr
pushNN' = fmap (ValExpr . JVar) pushNN

pushOptimized' :: [(Id,Int)]
               -> C
pushOptimized' xs = do
  slots  <- getSlots
  pushOptimized =<< (sequence $ zipWith f xs (slots++repeat SlotUnknown))
  where
    f (i1,n1) (SlotId i2 n2) = (,i1==i2&&n1==n2) <$> genIdsN i1 n1
    f (i1,n1) _              = (,False)          <$> genIdsN i1 n1

{- |  optimized push that reuses existing values on stack
      automatically chooses an optimized partial push (h$ppN)
      function when possible.
 -}
pushOptimized :: [(JExpr,Bool)] -- ^ contents of the slots, True if same value is already there
              -> C
pushOptimized [] = return mempty
pushOptimized xs = do
  dropSlots l
  stackDepth += length xs
  go .  csInlinePush <$> use gsSettings
  where
    go True = inlinePush
    go _
     | all snd xs                  = adjSp' l
     | all (not.snd) xs && l <= 32 =
        ApplStat (pushN' ! l) (map fst xs)
     | l <= 8 && not (snd $ last xs) =
        ApplStat (pushNN' ! sig) [ e | (e,False) <- xs ]
     | otherwise = inlinePush
    l   = length xs
    sig :: Integer
    sig = L.foldl1' (.|.) $ zipWith (\(_e,b) i -> if not b then bit i else 0) xs [0..]
    inlinePush = adjSp' l <> mconcat (zipWith pushSlot [1..] xs)
    pushSlot i (e,False) = [j| `Stack`[`offset i`] = `e` |]
    pushSlot _ _         = mempty
    offset i | i == l    = [je| `Sp` |]
             | otherwise = [je| `Sp` - `l-i` |]

push :: [JExpr] -> C
push xs = do
  dropSlots (length xs)
  stackDepth += length xs
  flip push' xs <$> use gsSettings

push' :: CgSettings -> [JExpr] -> JStat
push' _ [] = mempty
push' cs xs
   | csInlinePush cs || l > 32 || l < 2 = adjSp' l <> mconcat items
   | otherwise                          = ApplStat (toJExpr $ pushN ! l) xs
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

-- | pop the expressions, but ignore the top n elements of the stack
popSkip :: Int -> [(JExpr,StackSlot)] -> C
popSkip 0 [] = mempty
popSkip n [] = addUnknownSlots n >> adjSpN n
popSkip n xs = do
  addUnknownSlots n
  addSlots (map snd xs)
  a <- adjSpN (length xs + n)
  return (loadSkip n (map fst xs) <> a)

-- | pop things, don't upstate stack knowledge
popSkip' :: Int     -- ^ number of slots to skip
         -> [JExpr] -- ^ assign stack slot values to these
         -> JStat
popSkip' 0 []  = mempty
popSkip' n []  = adjSpN' n
popSkip' n tgt = loadSkip n tgt <> adjSpN' (length tgt + n)

-- | like popSkip, but without modifying the stack pointer
loadSkip :: Int -> [JExpr] -> JStat
loadSkip = loadSkipFrom (toJExpr Sp)

loadSkipFrom :: JExpr -> Int -> [JExpr] -> JStat
loadSkipFrom fr n xs = mconcat items
    where
      items = reverse $ zipWith (\i e -> [j| `e` = `Stack`[`offset (i+n)`]; |]) [(0::Int)..] (reverse xs)
      offset 0 = [je| `fr` |]
      offset n = [je| `fr` - `n` |]


-- declare and pop
popSkipI :: Int -> [(Ident,StackSlot)] -> C
popSkipI 0 [] = mempty
popSkipI n [] = adjSpN n
popSkipI n xs = do
  addUnknownSlots n
  addSlots (map snd xs)
  a <- adjSpN (length xs + n)
  return (loadSkipI n (map fst xs) <> a)

-- like popSkip, but without modifying sp
loadSkipI :: Int -> [Ident] -> JStat
loadSkipI = loadSkipIFrom (toJExpr Sp)

loadSkipIFrom :: JExpr -> Int -> [Ident] -> JStat
loadSkipIFrom fr n xs = mconcat items
    where
      items = reverse $ zipWith f [(0::Int)..] (reverse xs)
      offset 0 = fr
      offset n = [je| `fr` - `n` |]
      f i e = [j| `decl e`;
                  `e` = `Stack`[`offset (i+n)`];
                |]

popn :: Int -> C
popn n = addUnknownSlots n >> adjSpN n

-- below: c argument is closure entry, p argument is (heap) pointer to entry

closureType :: JExpr -> JExpr
closureType c = [je| `c`.f.t |]

isThunk :: JExpr -> JExpr
isThunk c = [je| `c`.f.t === `Thunk` |]

isThunk' :: JExpr -> JExpr
isThunk' f = [je| `f`.t === `Thunk` |]

isBlackhole :: JExpr -> JExpr
isBlackhole c = [je| `c`.f.t === `Blackhole` |]

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

conTag' :: JExpr -> JExpr
conTag' f = [je| `f`.a |]

entry :: JExpr -> JExpr
entry p = [je| `p`.f |]

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JExpr -> JExpr
funArity c = [je| `c`.f.a |]

-- function arity with raw reference to the entry
funArity' :: JExpr -> JExpr
funArity' f = [je| `f`.a |]

-- arity of a partial application
papArity :: JExpr -> JExpr
papArity cp = [je| `cp`.d2.d1 |]

funOrPapArity :: JExpr       -- ^ heap object
              -> Maybe JExpr -- ^ reference to entry, if you have one already (saves a c.f lookup twice)
              -> JExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c Nothing =
  [je| `isFun c` ? `funArity c` : `papArity c` |]
funOrPapArity c (Just f) =
  [je| `isFun' f` ? `funArity' f` : `papArity c` |]

{-
  Most stack frames have a static size, stored in f.size, but there
  are two exceptions:

   - dynamically sized stack frames (f.size === -1) have the size
     stored in the stack slot below the header
   - h$ap_gen is special

 -}

stackFrameSize :: JExpr -- ^ assign frame size to this
               -> JExpr -- ^ stack frame header function
               -> JStat -- ^ size of the frame, including header
stackFrameSize tgt f =
  [j| if(`f` === h$ap_gen) { // h$ap_gen is special
        `tgt` = (`Stack`[`Sp`-1] >> 8) + 2;
      } else {
        var tag = `f`.size;
        if(tag < 0) { // dynamic size
          `tgt` = `Stack`[`Sp`-1];
        } else {
          `tgt` = (tag & 0xff) + 1;
        }
      }
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

-- | the global linkable unit of a module exports this symbol, depend on it to include that unit
--   (used for cost centres)
moduleGlobalSymbol :: DynFlags -> Module -> Text
moduleGlobalSymbol dflags m
  = "h$" <>
    T.pack (zEncodeString $ showModule dflags m) <>
    "_<global>"

jsIdIdent :: Id -> Maybe Int -> IdType -> G Ident
jsIdIdent i mi suffix = do
  IdCache cache <- use gsIdents
  case M.lookup key cache of
    Just ident -> updateGlobalIdCache ident
    Nothing -> do
      ident <- jsIdIdent' i mi suffix
      let cache' = key `seq` ident `seq` IdCache (M.insert key ident cache)
      gsIdents .= cache'
      cache' `seq` updateGlobalIdCache ident
  where
    key = IdKey (getKey . getUnique $ i) (fromMaybe 0 mi) suffix
    updateGlobalIdCache :: Ident -> G Ident
    updateGlobalIdCache ji
      -- fixme also allow cashing entries for lifting?
      | not (isGlobalId i) || isJust mi || suffix /= IdPlain = pure ji
      | otherwise = do
          GlobalIdCache gidc <- use globalIdCache
          case M.lookup ji gidc of
            Nothing -> do
              globalIdCache .= GlobalIdCache (M.insert ji (key, i) gidc)
              return ji
            Just _  -> pure ji

globalIdCache :: Lens' GenState GlobalIdCache
globalIdCache = gsGroup . ggsGlobalIdCache

-- uncached
jsIdIdent' :: Id -> Maybe Int -> IdType -> G Ident
jsIdIdent' i mn suffix0 = do
  dflags      <- use gsDynFlags
  (prefix, u) <- mkPrefixU dflags
  i' <- (\x -> T.pack $ "h$"++prefix++x++mns++suffix++u) . zEncodeString <$> name
  i' `seq` return (TxtI i')
    where
      suffix = idTypeSuffix suffix0
      mns = maybe "" (('_':).show) mn
      name = fmap ('.':) . showPpr' . localiseName . getName $ i
      mkPrefixU :: DynFlags -> G (String, String)
      mkPrefixU dflags
        | isExportedId i, Just x <- (nameModule_maybe . getName) i = do
           let xstr = showModule dflags x
           return (zEncodeString xstr, "")
        | otherwise = (,('_':) . encodeUnique . getKey . getUnique $ i) . ('$':)
                    . zEncodeString . showModule dflags <$> use gsModule

showModule :: DynFlags -> Module -> String
showModule dflags m = pkg ++ ":" ++ modName
  where
    modName     = moduleNameString (moduleName m)
    pkg         = encodeInstalledUnitId dflags (toInstalledUnitId $ moduleUnitId m)

encodeInstalledUnitId :: DynFlags -> InstalledUnitId -> String
encodeInstalledUnitId dflags k
  | isGhcjsPrimPackage dflags k = "ghcjs-prim"
  | isGhcjsThPackage dflags k   = "ghcjs-th"
  | otherwise                   = installedUnitIdString k

{-
   some packages are wired into GHCJS, but not GHC
   make sure we don't version them in the output
   since the RTS uses thins from them
-}

isGhcjsPrimPackage :: DynFlags -> InstalledUnitId -> Bool
isGhcjsPrimPackage dflags pkgKey
  =  pn == "ghcjs-prim" ||
     (null pn && pkgKey == thisInstalledUnitId dflags &&
      any (=="-DBOOTING_PACKAGE=ghcjs-prim") (opt_P dflags))
  where
    pn = getInstalledPackageName dflags pkgKey

isGhcjsThPackage :: DynFlags -> InstalledUnitId -> Bool
isGhcjsThPackage dflags pkgKey
  =  pn == "ghcjs-th" ||
     (null pn && pkgKey == thisInstalledUnitId dflags &&
      any (=="-DBOOTING_PACKAGE=ghcjs-th") (opt_P dflags))
  where
    pn = getInstalledPackageName dflags pkgKey

ghcjsPrimPackage :: DynFlags -> InstalledUnitId
ghcjsPrimPackage dflags =
  case prims of
    ((_,k):_) -> toInstalledUnitId k
    _         -> error "Package `ghcjs-prim' is required to link executables"
  where
    prims = filter ((=="ghcjs-prim").fst)
                   (searchModule dflags (mkModuleName "GHCJS.Prim"))

ghcjsThPackage :: DynFlags -> InstalledUnitId
ghcjsThPackage dflags =
  case prims of
    ((_,k):_) -> toInstalledUnitId k
    _         -> error "Package `ghcjs-th' is required to link executables"
  where
    prims = filter ((=="ghcjs-th").fst)
                   (searchModule dflags (mkModuleName "GHCJS.Prim.TH.Eval"))


idTypeSuffix :: IdType -> String
idTypeSuffix IdPlain = ""
idTypeSuffix IdEntry = "_e"
idTypeSuffix IdConEntry = "_con_e"

jsVar :: String -> JExpr
jsVar v = ValExpr . JVar . TxtI . T.pack $ v

jsId :: Id -> G JExpr
jsId i
--  | i == trueDataConId  = return $ toJExpr True
--  | i == falseDataConId = return $ toJExpr False
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
    s  = typeSize (idType i)

genIdsN :: Id -> Int -> G JExpr
genIdsN i n = do
  xs <- genIds i
  return $ xs !! (n-1)

-- | get all idents for an id
genIdsI :: Id -> G [Ident]
genIdsI i
  | s == 1    = (:[]) <$> jsIdI i
  | otherwise = mapM (jsIdIN i) [1..s]
        where
          s = typeSize (idType i)

genIdsIN :: Id -> Int -> G Ident
genIdsIN i n = do
  xs <- genIdsI i
  return $ xs !! (n-1)

-- | declare all js vars for the id
declIds :: Id -> C
declIds  i
  | s == 0    = return mempty
  | s == 1    = decl <$> jsIdI i
  | otherwise = mconcat <$> mapM (\n -> decl <$> jsIdIN i n) [1..s]
  where
    s  = typeSize (idType i)
