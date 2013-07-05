{-# LANGUAGE QuasiQuotes #-}

module Gen2.ClosureInfo where

import           Data.Bits ((.|.), shiftL)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Applicative ((<$>))

import           Language.Javascript.JMacro

import           Gen2.Utils
import           Gen2.StgAst ()

import           StgSyn
import           DataCon
import           TyCon
import           Type

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
             | MVarV    -- h$MVar object
--             | TVarV
             | MutVarV -- h$MutVar object
             | WeakV
               deriving (Eq, Ord, Show, Enum, Bounded)

-- can we unbox C x to x, only if x is represented as a Number
isUnboxableCon :: DataCon -> Bool
isUnboxableCon dc
  | [t] <- dataConRepArgTys dc, [t1] <- typeVt t =
       isUnboxable t1 &&
       dataConTag dc == 1 &&
       length (tyConDataCons $ dataConTyCon dc) == 1
  | otherwise = False

-- one-constructor types with one primitive field represented as a JS Number
-- can be unboxed
isUnboxable :: VarType -> Bool
isUnboxable DoubleV = True
isUnboxable IntV    = True -- includes Char#
isUnboxable _       = False

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
                                   Nothing -> error "primTypeVt: not a TyCon"
                                   Just tc -> go (show tc)
                 _ -> error "primTypeVt: non-unary type found"
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
    | st == pr "MutVar#" = ObjV
    | st == pr "TVar#" = ObjV
    | st == pr "MVar#" = ObjV
    | st == pr "State#" = VoidV
    | st == pr "RealWorld" = VoidV
    | st == pr "ThreadId#" = ObjV
    | st == pr "Weak#" = WeakV
    | st == pr "StablePtr#" = AddrV
    | st == pr "StableName#" = ObjV
    | st == pr "MutVar#" = ArrV -- MutVarV
    | st == pr "BCO#" = ObjV -- fixme what do we need here?
    | st == pr "~#" = VoidV -- coercion token?
    | st == pr "Any" = PtrV
    | st == "Data.Dynamic.Obj" = PtrV -- ?
    | otherwise = error ("primTypeVt: unrecognized primitive type: " ++ st)

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


data ClosureInfo = ClosureInfo
     { ciVar    :: Text      -- ^ object being infod
     , ciRegs   :: [VarType] -- ^ things in registers
     , ciName   :: Text      -- ^ friendly name for printing
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
                CIStaticRefs   { staticRefs :: [Text] } -- ^ list of refs that need to be kept alive
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
setObjInfoL :: Text      -- ^ the object name
            -> [VarType] -- ^ things in registers
            -> CILayout  -- ^ layout of the object
            -> CType     -- ^ closure type
            -> Text      -- ^ object name, for printing
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

setObjInfo :: Text       -- ^ the thing to modify
           -> CType      -- ^ closure type
           -> Text       -- ^ object name, for printing
           -> [Int]      -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Int        -- ^ extra 'a' parameter, for constructor tag or arity
           -> JExpr      -- ^ tag for the garbage collector
           -> [VarType]  -- ^ things in registers
           -> CIStatic   -- ^ static refs
           -> JStat
setObjInfo obj t name fields a gctag argptrs static
--  | True  = [j| h$setObjInfo(`StrI (T.unpack obj)`, `t`, `name`, `fields`, `a`, `gctag`, `nregs`, `static`);  |]
   | otherwise = [j| h$o(`StrI (T.unpack obj)`,`t`,`a`,`gctag`,`nregs`,`static`); |]
  where
    nregs = sum $ map varSize argptrs

scannableOffsets :: Int -> [VarType] -> [Int]
scannableOffsets start ts =
  [o | (o,True) <- zip [start..] (concatMap isScannable ts) ]

ptrTag :: [Int] -> Maybe Int
ptrTag ptrs
    | any (>30) ptrs = Nothing -- error "tag bits greater than 30 unsupported"
    | otherwise      = Just $ foldl' (.|.) 0 (map (1 `shiftL`) $ filter (>=0) ptrs)

shiftedPtrTag :: Int -> [Int] -> Maybe Int
shiftedPtrTag shift = ptrTag . map (subtract shift)

