{-# LANGUAGE QuasiQuotes,
             BangPatterns,
             ScopedTypeVariables,
             TemplateHaskell,
             OverloadedStrings #-}

{-
  The compactor does link-time optimization. It is much simpler
  than the Optimizer, no fancy dataflow analysis here.

  Optimizations:
  - rewrite all variables starting with h$$ to shorter names,
       these are internal names
  - write all function metadata compactly
 -}

module Gen2.Compactor where

import           DynFlags

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict

import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import           Data.ByteString.Builder (Builder)
import           Data.Char (chr, ord)
import           Data.Function (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.Map (Map)
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import           Compiler.JMacro
import           Compiler.Settings

import           Gen2.Base
import           Gen2.ClosureInfo
import           Gen2.Utils (buildingProf, buildingDebug)
import           Gen2.Printer             (pretty)
import qualified Panic
import           Text.PrettyPrint.Leijen.Text (renderPretty, displayT)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson (ToJSON(..), Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16

import System.IO.Unsafe (unsafePerformIO)


-- | collect global objects (data / CAFs). rename them and add them to the table
collectGlobals :: [StaticInfo]
               -> State CompactorState ()
collectGlobals = mapM_ (\(StaticInfo i _ _) -> renameObj i)

debugShowStat :: (JStat, [ClosureInfo], [StaticInfo]) -> String
debugShowStat (_s, cis, sis) = "closures:\n" ++ unlines (map show cis) ++ "\nstatics:" ++ unlines (map show sis) ++ "\n\n"

renameInternals :: GhcjsSettings
                -> DynFlags
                -> CompactorState
                -> [Text]
                -> [(JStat, [ClosureInfo], [StaticInfo])]
                -> (CompactorState, [JStat], JStat)
renameInternals settings dflags cs0 rtsDeps stats0a = (cs, stats, meta)
  where
    -- hashes = buildHashes stats0a
    stats0 :: [(JStat, [ClosureInfo], [StaticInfo])]
    stats0 = (if gsDedupe settings then dedupe rtsDeps else id) stats0a
    ((stats, meta), cs) = runState renamed cs0
    renamed :: State CompactorState ([JStat], JStat)
    renamed
      | buildingDebug dflags || buildingProf dflags = do
        cs <- get
        let renamedStats = map (\(s,_,_) -> s & identsS %~ lookupRenamed cs) stats0
            statics      = map (renameStaticInfo cs)  $ concatMap (\(_,_,x) -> x) stats0
            infos        = map (renameClosureInfo cs) $ concatMap (\(_,x,_) -> x) stats0
            -- render metadata as individual statements
            meta = mconcat (map staticDeclStat statics) <>
                   mconcat (map (staticInitStat $ buildingProf dflags) statics) <>
                   mconcat (map (closureInfoStat True) infos)
        return (renamedStats, meta)
      | otherwise = do
        -- collect all global objects and entries, add them to the renaming table
        mapM_ (\(_, cis, sis) -> do
               mapM_ (renameEntry . TxtI . ciVar) cis
               mapM_ (renameObj . siVar) sis
               mapM_ collectLabels sis) stats0

        -- sort our entries, store the results
        -- propagate all renamings throughtout the code
        cs <- get
        let renamedStats = map (\(s,_,_) -> s & identsS %~ lookupRenamed cs) stats0
            sortedInfo   = concatMap (\(_,xs,_) -> map (renameClosureInfo cs) xs) stats0
            entryArr     = map (TxtI . fst) . sortBy (compare `on` snd) . HM.toList $ cs ^. entries
            lblArr       = map (TxtI . fst) . sortBy (compare `on` snd) . HM.toList $ cs ^. labels
            ss           = concatMap (\(_,_,xs) -> map (renameStaticInfo cs) xs) stats0
            infoBlock    = encodeStr (concatMap (encodeInfo cs) sortedInfo)
            staticBlock  = encodeStr (concatMap (encodeStatic cs) ss)
            staticDecls  = mconcat (map staticDeclStat ss)
            meta = staticDecls <>
                   [j| h$scheduleInit(`entryArr`, h$staticDelayed, `lblArr`, `infoBlock`, `staticBlock`);
                       h$staticDelayed = [];
                     |]
        return (renamedStats, meta)


-- | rename a heap object, which means adding it to the
--  static init table in addition to the renamer
renameObj :: Text
          -> State CompactorState Text
renameObj xs = do
  (TxtI xs') <- renameVar (TxtI xs)
  addItem statics statics numStatics numStatics parentStatics xs'
  return xs'

renameEntry :: Ident
            -> State CompactorState Ident
renameEntry i = do
  i'@(TxtI i'') <- renameVar i
  addItem entries entries numEntries numEntries parentEntries i''
  return i'

addItem :: Getting (HashMap Text Int) CompactorState (HashMap Text Int)
        -> Setting (->) CompactorState CompactorState (HashMap Text Int) (HashMap Text Int)
        -> Getting Int CompactorState Int
        -> ASetter' CompactorState Int
        -> Getting (HashMap Text Int) CompactorState (HashMap Text Int)
        -> Text
        -> State CompactorState ()
addItem items items' numItems numItems' parentItems i = do
  s <- use items
  case HM.lookup i s of
    Just _ -> return ()
    Nothing -> do
      sp <- use parentItems
      case HM.lookup i sp of
        Just _  -> return ()
        Nothing -> do
          ni <- use numItems
          items' %= HM.insert i ni
          numItems' += 1

collectLabels :: StaticInfo -> State CompactorState ()
collectLabels si = mapM_ (addItem labels labels numLabels numLabels parentLabels) (labelsV . siVal $ si)
  where
    labelsV (StaticData _ args) = concatMap labelsA args
    labelsV (StaticList args _) = concatMap labelsA args
    labelsV _                   = []
    labelsA (StaticLitArg l) = labelsL l
    labelsA _                = []
    labelsL (LabelLit _ lbl) = [lbl]
    labelsL _                = []

lookupRenamed :: CompactorState -> Ident -> Ident
lookupRenamed cs i@(TxtI t) =
  case HM.lookup t (cs ^. nameMap) of
    Nothing -> i
    Just i' -> i'

renameVar :: Ident                      -- ^ text identifier to rename
          -> State CompactorState Ident -- ^ the updated renamer state and the new ident
renameVar i@(TxtI t)
  | "h$$" `T.isPrefixOf` t = do
      m <- use nameMap
      case HM.lookup t m of
        Just r  -> return r
        Nothing -> do
          y <- newIdent
          nameMap %= HM.insert t y
          return y
  | otherwise = return i

newIdent :: State CompactorState Ident
newIdent = do
  (y:ys) <- use identSupply
  identSupply .= ys
  return y

-- | rename a compactor info entry according to the compactor state (no new renamings are added)
renameClosureInfo :: CompactorState
                  -> ClosureInfo
                  -> ClosureInfo
renameClosureInfo cs (ClosureInfo v rs n l t s)  =
  (ClosureInfo (renameV v) rs n l t (f s))
    where
      renameV t = maybe t (\(TxtI t') -> t') (HM.lookup t m)
      m                   = cs ^. nameMap
      f (CIStaticRefs rs) = CIStaticRefs (map renameV rs)

-- | rename a static info entry according to the compactor state (no new renamings are added)
renameStaticInfo :: CompactorState
                 -> StaticInfo
                 -> StaticInfo
renameStaticInfo cs si = si & staticIdents %~ renameIdent
  where
    renameIdent t = maybe t (\(TxtI t') -> t') (HM.lookup t $ cs ^. nameMap)

staticIdents :: Traversal' StaticInfo Text
staticIdents f (StaticInfo i v cc) = StaticInfo <$> f i <*> staticIdentsV f v <*> pure cc

staticIdentsV :: Traversal' StaticVal Text
staticIdentsV f (StaticFun i args)     = StaticFun <$> f i <*> traverse (staticIdentsA f) args
staticIdentsV f (StaticThunk (Just (i, args))) = StaticThunk . Just <$> liftA2 (,) (f i) (traverse (staticIdentsA f) args)
staticIdentsV f (StaticData con args)  = StaticData <$> f con <*> traverse (staticIdentsA f) args
staticIdentsV f (StaticList xs t)      = StaticList <$> traverse (staticIdentsA f) xs <*> traverse f t
staticIdentsV _ x                      = pure x

staticIdentsA :: Traversal' StaticArg Text
staticIdentsA f (StaticObjArg t) = StaticObjArg <$> f t
staticIdentsA _ x                = pure x


{-
   simple encoding of naturals using only printable low char points,
   rely on gzip to compress repeating sequences,
   most significant bits first
      1 byte: ascii code 32-123  (0-89), \ and " unused
      2 byte: 124 a b            (90-8189)
      3 byte: 125 a b c          (8190-737189)
-}
encodeStr :: [Int] -> String
encodeStr = concatMap encodeChr
  where
    c :: Int -> Char
    c i | i > 90 || i < 0 = error ("encodeStr: c " ++ show i)
        | i >= 59   = chr (34+i)
        | i >= 2    = chr (33+i)
        | otherwise = chr (32+i)
    encodeChr i
      | i < 0       = error "encodeStr: negative"
      | i <= 89     = [c i]
      | i <= 8189   = let (c1, c2)  = (i - 90) `divMod` 90 in [chr 124, c c1, c c2]
      | i <= 737189 = let (c2a, c3) = (i - 8190) `divMod` 90
                          (c1, c2)  = c2a `divMod` 90
                      in [chr 125, c c1, c c2, c c3]
      | otherwise = error "encodeStr: overflow"

entryIdx :: String
         -> CompactorState
         -> Text
         -> Int
entryIdx msg cs i = fromMaybe lookupParent (HM.lookup i' (cs ^. entries))
  where
    (TxtI i')    = lookupRenamed cs (TxtI i)
    lookupParent = maybe err (+ cs ^. numEntries) (HM.lookup i' (cs ^. parentEntries))
    err = error (msg ++ ": invalid entry: " ++ T.unpack i')

objectIdx :: String
          -> CompactorState
          -> Text
          -> Int
objectIdx msg cs i = fromMaybe lookupParent (HM.lookup i' (cs ^. statics))
  where
    (TxtI i')    = lookupRenamed cs (TxtI i)
    lookupParent = maybe err (+ cs ^. numStatics) (HM.lookup i' (cs ^. parentStatics))
    err          = error (msg ++ ": invalid static: " ++ T.unpack i')

labelIdx :: String
         -> CompactorState
         -> Text
         -> Int
labelIdx msg cs l = fromMaybe lookupParent (HM.lookup l (cs ^. labels))
  where
    lookupParent = maybe err (+ cs ^. numLabels) (HM.lookup l (cs ^. parentLabels))
    err          = error (msg ++ ": invalid label: " ++ T.unpack l)

encodeInfo :: CompactorState
           -> ClosureInfo  -- ^ information to encode
           -> [Int]
encodeInfo cs (ClosureInfo _var regs name layout typ static)
  | CIThunk              <- typ = [0] ++ ls
  | (CIFun _arity regs0) <- typ, regs0 /= argSize regs
     = error ("encodeInfo: inconsistent register metadata for " ++ T.unpack name)
  | (CIFun arity _regs0) <- typ = [1, arity, encodeRegs regs] ++ ls
  | (CICon tag)          <- typ = [2, tag] ++ ls
  | CIStackFrame         <- typ = [3, encodeRegs regs] ++ ls
-- (CIPap ar)         <- typ = [4, ar] ++ ls  -- these should only appear during runtime
  | otherwise                  = error ("encodeInfo, unexpected closure type: " ++ show typ)
  where
    ls         = encodeLayout layout ++ encodeSrt static
    encodeLayout CILayoutVariable      = [0]
    encodeLayout (CILayoutUnknown s)   = [s+1]
    encodeLayout (CILayoutFixed s _vs) = [s+1]
    encodeSrt (CIStaticRefs rs) = length rs : map (objectIdx "encodeInfo" cs) rs
    encodeRegs CIRegsUnknown = 0
    encodeRegs (CIRegs skip regTypes) = let nregs = sum (map varSize regTypes)
                                        in  encodeRegsTag skip nregs
    encodeRegsTag skip nregs
      | skip < 0 || skip > 1 = error "encodeRegsTag: unexpected skip"
      | otherwise            = 1 + (nregs `shiftL` 1) + skip
    argSize (CIRegs skip regTypes) = sum (map varSize regTypes) - 1 + skip
    argSize _ = 0

encodeStatic :: CompactorState
             -> StaticInfo
             -> [Int]
encodeStatic cs (StaticInfo _to sv _)
    | StaticFun f args <- sv                      = [1, entry f, length args] ++ concatMap encodeArg args
    | StaticThunk (Just (t, args)) <- sv          = [2, entry t, length args] ++ concatMap encodeArg args
    | StaticThunk Nothing <- sv                   = [0]
    | StaticUnboxed (StaticUnboxedBool b) <- sv   = [3 + fromEnum b]
    | StaticUnboxed (StaticUnboxedInt i) <- sv    = [5] -- ++ encodeInt i
    | StaticUnboxed (StaticUnboxedDouble d) <- sv = [6] -- ++ encodeDouble d
--    | StaticString t <- sv         = [7, T.length t] ++ map encodeChar (T.unpack t)
--    | StaticBin bs <- sv           = [8, BS.length bs] ++ map fromIntegral (BS.unpack bs)
    | StaticList [] Nothing <- sv                 = [8]
    | StaticList args t <- sv                     = [9, length args] ++ maybe [0] (\t' -> [1, obj t']) t ++ concatMap encodeArg (reverse args)
    | StaticData con args <- sv =
      (if length args <= 6 then [11+length args] else [10,length args]) ++ [entry con] ++ concatMap encodeArg args
  where
    obj   = objectIdx "encodeStatic" cs
    entry = entryIdx  "encodeStatic" cs
    lbl   = labelIdx  "encodeStatic" cs
    -- | an argument is either a reference to a heap object or a primitive value
    encodeArg (StaticLitArg (BoolLit b))    = [0 + fromEnum b]
    encodeArg (StaticLitArg (IntLit 0))     = [2]
    encodeArg (StaticLitArg (IntLit 1))     = [3]
    encodeArg (StaticLitArg (IntLit i))     = [4] ++ encodeInt i
    encodeArg (StaticLitArg NullLit)        = [5]
    encodeArg (StaticLitArg (DoubleLit d))  = [6] ++ encodeDouble d
    encodeArg (StaticLitArg (StringLit s))  = [7] ++ encodeString s
    encodeArg (StaticLitArg (BinLit b))     = [8] ++ encodeBinary b
    encodeArg (StaticLitArg (LabelLit b l)) = [9, fromEnum b, lbl l]
    encodeArg (StaticConArg con args)       = [10, entry con, length args] ++ concatMap encodeArg args
    encodeArg (StaticObjArg t)              = [11 + obj t]
    -- encodeArg x                             = error ("encodeArg: unexpected: " ++ show x)
    encodeChar = ord -- fixme make characters more readable

encodeString :: Text -> [Int]
encodeString xs = encodeBinary (TE.encodeUtf8 xs)

-- ByteString is prefixed with length, then blocks of 4 numbers encoding 3 bytes
encodeBinary :: BS.ByteString -> [Int]
encodeBinary bs = BS.length bs : go bs
  where
    go b | BS.null b = []
         | l == 1    = let b0 = b `BS.index` 0
                       in  map fromIntegral [ b0 `shiftR` 2, (b0 .&. 3) `shiftL` 4 ]
         | l == 2    = let b0 = b `BS.index` 0
                           b1 = b `BS.index` 1
                       in  map fromIntegral [ b0 `shiftR` 2
                                            , ((b0 .&. 3) `shiftL` 4) .|. (b1 `shiftR` 4)
                                            , (b1 .&. 15) `shiftL` 2
                                            ]
         | otherwise = let b0 = b `BS.index` 0
                           b1 = b `BS.index` 1
                           b2 = b `BS.index` 2
                       in  map fromIntegral [ b0 `shiftR` 2
                                            , ((b0 .&. 3)  `shiftL` 4) .|. (b1 `shiftR` 4)
                                            , ((b1 .&. 15) `shiftL` 2) .|. (b2 `shiftR` 6)
                                            , b2 .&. 63
                                            ] ++ go (BS.drop 3 b)
      where l = BS.length b

encodeInt :: Integer -> [Int]
encodeInt i
  | i >= -10 && i < encodeMax - 11 = [fromIntegral i + 12]
  | i > 2^(31::Int)-1 || i < -2^(31::Int) = error "encodeInt: integer outside 32 bit range"
  | otherwise = let i' :: Int32 = fromIntegral i
                in [0, fromIntegral ((i' `shiftR` 16) .&. 0xffff), fromIntegral (i' .&. 0xffff)]

-- encode a possibly 53 bit int
encodeSignificand :: Integer -> [Int]
encodeSignificand i
  | i >= -10 && i < encodeMax - 11      = [fromIntegral i + 12]
  | i > 2^(53::Int) || i < -2^(53::Int) = error ("encodeInt: integer outside 53 bit range: " ++ show i)
  | otherwise = let i' = abs i
                in  [if i < 0 then 0 else 1] ++
                    map (\r -> fromIntegral ((i' `shiftR` r) .&. 0xffff)) [48,32,16,0]

encodeDouble :: SaneDouble -> [Int]
encodeDouble (SaneDouble d)
  | isNegativeZero d      = [0]
  | d == 0                = [1]
  | isInfinite d && d > 0 = [2]
  | isInfinite d          = [3]
  | isNaN d               = [4]
  | abs exponent <= 30    = [6 + fromIntegral exponent + 30] ++ encodeSignificand significand
  | otherwise             = [5] ++ encodeInt (fromIntegral exponent) ++ encodeSignificand significand
    where
      (significand, exponent) = decodeFloat d

encodeMax :: Integer
encodeMax = 737189

{- |
  The Base data structure contains the information we need
  to do incremental linking against a base bundle.

  base file format:
  GHCJSBASE
  [renamer state]
  [linkedPackages]
  [packages]
  [modules]
  [symbols]
 -}

renderBase :: Base                                   -- ^ base metadata
           -> BL.ByteString                          -- ^ rendered result
renderBase = DB.runPut . putBase

loadBase :: FilePath -> IO Base
loadBase file = DB.runGet (getBase file) <$> BL.readFile file

----------------------------

{-# INLINE identsS #-}
identsS :: Traversal' JStat Ident
identsS f (DeclStat i)         = DeclStat       <$> f i
identsS f (ReturnStat e)       = ReturnStat     <$> identsE f e
identsS f (IfStat e s1 s2)     = IfStat         <$> identsE f e <*> identsS f s1 <*> identsS f s2
identsS f (WhileStat b e s)    = WhileStat b    <$> identsE f e <*> identsS f s
identsS f (ForInStat b i e s)  = ForInStat b    <$> f i <*> identsE f e <*> identsS f s
identsS f (SwitchStat e xs s)  = SwitchStat     <$> identsE f e <*> (traverse . traverseCase) f xs <*> identsS f s
  where traverseCase g (e,s) = (,) <$> identsE g e <*> identsS g s
identsS f (TryStat s1 i s2 s3) = TryStat        <$> identsS f s1 <*> f i <*> identsS f s2 <*> identsS f s3
identsS f (BlockStat xs)       = BlockStat   <$> (traverse . identsS) f xs
identsS f (ApplStat e es)      = ApplStat    <$> identsE f e <*> (traverse . identsE) f es
identsS f (UOpStat op e)       = UOpStat op  <$> identsE f e
identsS f (AssignStat e1 e2)   = AssignStat  <$> identsE f e1 <*> identsE f e2
identsS _ (UnsatBlock{})       = error "identsS: UnsatBlock"
identsS _ (AntiStat{})         = error "identsS: AntiStat"
identsS f (LabelStat l s)      = LabelStat l <$> identsS f s
identsS _ b@(BreakStat{})      = pure b
identsS _ c@(ContinueStat{})   = pure c

{-# INLINE identsE #-}
identsE :: Traversal' JExpr Ident
identsE f (ValExpr v)         = ValExpr     <$> identsV f v
identsE f (SelExpr e i)       = SelExpr     <$> identsE f e <*> pure i -- do not rename properties
identsE f (IdxExpr e1 e2)     = IdxExpr     <$> identsE f e1 <*> identsE f e2
identsE f (InfixExpr s e1 e2) = InfixExpr s <$> identsE f e1 <*> identsE f e2
identsE f (UOpExpr o e)       = UOpExpr o   <$> identsE f e
identsE f (IfExpr e1 e2 e3)   = IfExpr      <$> identsE f e1 <*> identsE f e2 <*> identsE f e3
identsE f (ApplExpr e es)     = ApplExpr    <$> identsE f e <*> (traverse . identsE) f es
identsE _ (UnsatExpr{})       = error "identsE: UnsatExpr"
identsE _ (AntiExpr{})        = error "identsE: AntiExpr"

{-# INLINE identsV #-}
identsV :: Traversal' JVal Ident
identsV f (JVar i)       = JVar  <$> f i
identsV f (JList xs)     = JList <$> (traverse . identsE) f xs
identsV _ d@(JDouble{})  = pure d
identsV _ i@(JInt{})     = pure i
identsV _ s@(JStr{})     = pure s
identsV _ r@(JRegEx{})   = pure r
identsV f (JHash m)      = JHash <$> (traverse . identsE) f m
identsV f (JFunc args s) = JFunc <$> traverse f args <*> identsS f s
identsV _ (UnsatVal{})   = error "identsV: UnsatVal"

compact :: GhcjsSettings
        -> DynFlags
        -> CompactorState
        -> [Text]
        -> [(JStat, [ClosureInfo], [StaticInfo])]
        -> (CompactorState, [JStat], JStat) -- ^ renamer state, statements for each unit, metadata
compact settings dflags rs rtsDeps input
--  | dumpHashes' input
  =
  renameInternals settings dflags rs rtsDeps input


-- hash compactification

data HashIdx = HashIdx (Map Text Hash) (Map Hash Text)

dedupe :: [Text]
       -> [(JStat, [ClosureInfo], [StaticInfo])]
       -> [(JStat, [ClosureInfo], [StaticInfo])]
dedupe rtsDeps input
--  | dumpHashIdx idx
  =
  map (\(st,cis,sis) -> dedupeBlock idx st cis sis) input
  where
    idx    = HashIdx hashes hr
    hashes0 = buildHashes rtsDeps input
    hashes  = foldl' (flip M.delete) hashes0 rtsDeps
    hr     = fmap pickShortest
             (M.fromListWith (++) $
             map (\(i, h) -> (h, [i])) (M.toList hashes))
    pickShortest :: [Text] -> Text
    pickShortest = head . sortBy (compare `on` T.length)

dedupeBlock :: HashIdx -> JStat -> [ClosureInfo] -> [StaticInfo] -> (JStat, [ClosureInfo], [StaticInfo])
dedupeBlock hi st ci si =
  (dedupeStat hi st, mapMaybe (dedupeClosureInfo hi) ci, mapMaybe (dedupeStaticInfo hi) si)

dedupeStat :: HashIdx -> JStat -> JStat
dedupeStat hi st = go st
  where
    go (BlockStat ss) = BlockStat (map go ss)
    go s@(DeclStat (TxtI i))
      | not (isCanon hi i) = mempty
      | otherwise          = s
    go s@(AssignStat v@(ValExpr (JVar (TxtI i))) e)
      | not (isCanon hi i) = mempty
      | otherwise          = AssignStat v (e & identsE %~ toCanonI hi)
    -- rewrite identifiers in e
    go s = s & identsS %~ toCanonI hi

dedupeClosureInfo :: HashIdx -> ClosureInfo -> Maybe ClosureInfo
dedupeClosureInfo hi (ClosureInfo i rs n l ty st)
  | isCanon hi i = Just (ClosureInfo i rs n l ty (dedupeCIStatic hi st))
dedupeClosureInfo _ _ = Nothing

dedupeStaticInfo :: HashIdx -> StaticInfo -> Maybe StaticInfo
dedupeStaticInfo hi (StaticInfo i val ccs)
  | isCanon hi i = Just (StaticInfo i (dedupeStaticVal hi val) ccs)
dedupeStaticInfo _ _ = Nothing

dedupeCIStatic :: HashIdx -> CIStatic -> CIStatic
dedupeCIStatic hi (CIStaticRefs refs) = CIStaticRefs (nub $ map (toCanon hi) refs)

dedupeStaticVal :: HashIdx -> StaticVal -> StaticVal
dedupeStaticVal hi (StaticFun t args) =
  StaticFun (toCanon hi t) (map (dedupeStaticArg hi) args)
dedupeStaticVal hi (StaticThunk (Just (o, args))) =
  StaticThunk (Just (toCanon hi o, map (dedupeStaticArg hi) args))
dedupeStaticVal hi (StaticData dcon args) =
  StaticData (toCanon hi dcon) (map (dedupeStaticArg hi) args)
dedupeStaticVal hi (StaticList args lt) =
  StaticList (map (dedupeStaticArg hi) args) (fmap (toCanon hi) lt)
dedupeStaticVal _ v = v -- unboxed value or thunk with alt init, no rewrite needed

dedupeStaticArg :: HashIdx -> StaticArg -> StaticArg
dedupeStaticArg hi (StaticObjArg o)      = StaticObjArg (toCanon hi o)
dedupeStaticArg hi (StaticConArg c args) = StaticConArg (toCanon hi c) (map (dedupeStaticArg hi) args)
dedupeStaticArg hi a@(StaticLitArg{})    = a

isCanon :: HashIdx -> Text -> Bool
isCanon (HashIdx a b) t
  | Nothing <- la = True
  | Just h  <- la
  , Just t' <- M.lookup h b = t == t'
  | otherwise = False
  where la = M.lookup t a

toCanon :: HashIdx -> Text -> Text
toCanon (HashIdx a b) t
  | Just h  <- M.lookup t a
  , Just t' <- M.lookup h b = t'
  | otherwise = t

toCanonI :: HashIdx -> Ident -> Ident
toCanonI hi (TxtI x) = TxtI (toCanon hi x)
{-
mapDefinitions :: (Ident -> JExpr -> JStat) -> JStat -> JStat
mapDefinitions f s = go s
  where
    go (BlockStat ss)                    = BlockStat (map go ss)
    go (AssignStat (ValExpr (JVar i)) e) = f i e
    go s                                 = s
-}

-- still need to rename all idents in CI / SI

type Hash = (BS.ByteString, [Text])

data HashBuilder = HashBuilder !BB.Builder ![Text]

instance Monoid HashBuilder where
  mempty = HashBuilder mempty mempty
  mappend (HashBuilder b1 l1) (HashBuilder b2 l2) =
    HashBuilder (b1 <> b2) (l1 <> l2)


dumpHashIdx :: HashIdx -> Bool
dumpHashIdx hi@(HashIdx ma mb) =
  let ks = M.keys ma
      difCanon i = let i' = toCanon hi i
                   in if i == i' then Nothing else Just i'
      writeHashIdx = do
        putStrLn "writing hash idx"
        T.writeFile "hashidx.txt"
          (T.unlines . sort $ mapMaybe (\i -> fmap ((i <> " -> ") <>) (difCanon i)) ks)
  in unsafePerformIO writeHashIdx `seq` True

-- debug thing
{-
dumpHashes' :: [(JStat, [ClosureInfo], [StaticInfo])] -> Bool
dumpHashes' input =
  let hashes      = buildHashes input
      writeHashes = do
        putStrLn "writing hashes"
        BL.writeFile "hashes.json" (Aeson.encode $ dumpHashes hashes)
  in unsafePerformIO writeHashes `seq` True
-}

buildHashes :: [Text] -> [(JStat, [ClosureInfo], [StaticInfo])] -> Map Text Hash
buildHashes rtsDeps xss
--  | dumpHashes0 hashes0
  = fixHashes (fmap finalizeHash hashes0)
  where
    globals = foldl' (flip S.delete) (findAllGlobals xss) rtsDeps
    hashes0 = (M.unions (map buildHashesBlock xss))
    buildHashesBlock (st, cis, sis) =
      let hdefs = hashDefinitions globals st
          hcis  = map hashClosureInfo cis
          hsis  = map hashStaticInfo (filter (not . ignoreStatic) sis)
      in  M.fromList (combineHashes hdefs hcis ++ hsis)

findAllGlobals :: [(JStat, [ClosureInfo], [StaticInfo])] -> Set Text
findAllGlobals xss = S.fromList $ concatMap f xss
  where
    f (_, cis, sis) =
      map (\(ClosureInfo i _ _ _ _ _) -> i) cis ++
      map (\(StaticInfo i _ _) -> i) sis

fixHashes :: Map Text Hash -> Map Text Hash
fixHashes hashes = fmap (\(bs,h) -> (bs, map replaceHash h)) hashes
  where
    replaceHash h = fromMaybe h (M.lookup h finalHashes)
    hashText  bs = "h$$$" <> TE.decodeUtf8 (B16.encode bs) -- go (BS.unpack bs)
    finalHashes  = fmap hashText (fixHashesIter 500 (M.keys hashes) hashes M.empty)

fixHashesIter :: Int -> [Text] -> Map Text Hash -> Map Text BS.ByteString -> Map Text BS.ByteString
fixHashesIter n keys hashes finalHashes
--  | unsafePerformIO (putStrLn ("fixHashesIter: " ++ show n)) `seq` False = undefined
  | n < 0 || null newHashes = finalHashes
  | otherwise               = fixHashesIter (n-1) keys hashes
      (M.union finalHashes $ M.fromList newHashes)
  where
    mkNewHash k | M.notMember k finalHashes
                , Just (hb, htxt) <- M.lookup k hashes
                , Just bs <- mapM (`M.lookup` finalHashes) htxt =
                  Just (k, makeFinalHash hb bs)
                | otherwise = Nothing
    newHashes = mapMaybe mkNewHash keys

makeFinalHash :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
makeFinalHash b bs = SHA256.hash (mconcat (b:bs))

-- do not deduplicate thunks
ignoreStatic :: StaticInfo -> Bool
ignoreStatic (StaticInfo _ (StaticThunk {}) _) = True
ignoreStatic _                                 = False

-- combine hashes from x and y, leaving only those which haven an entry in both
combineHashes :: [(Text, HashBuilder)] -> [(Text, HashBuilder)] -> [(Text, HashBuilder)]
combineHashes x y = M.toList $ M.intersectionWith (<>) (M.fromList x) (M.fromList y)
{-  f hbx hby =
  mapMaybe f x
  where
    f :: (Text, HashBuilder) -> Maybe (Text, HashBuilder)
    f (tx, hbx) = fmap ((tx,) . mappend)
    m = M.fromList y -}
  -- M.toList $ M.fromListWith mappend (x ++ y)

dumpHashes0 :: Map Text HashBuilder -> Bool
dumpHashes0 hashes = unsafePerformIO writeHashes `seq` True
  where
    hashLine (n, HashBuilder bb txt) =
      n <> " ->\n    " <>
      escapeBS (BB.toLazyByteString bb) <> "\n    [" <> T.intercalate " " txt <> "]\n"
    escapeBS :: BL.ByteString -> T.Text
    escapeBS = T.pack . concatMap escapeCH . BL.unpack
    escapeCH c | c < 32 || c > 127 = '\\' : show c
               | c == 92           = "\\\\"
               | otherwise         = [chr (fromIntegral c)]

    writeHashes = do
      putStrLn "writing hashes0"
      T.writeFile "hashes0.dump" (T.unlines $ map hashLine (M.toList hashes))

dumpHashes :: Map Text Hash -> Value
dumpHashes idx = toJSON iidx
   where
       iidx :: Map Text [(Text, [Text])]
       iidx = M.fromListWith (++) $
         map (\(t, (b, deps)) -> (TE.decodeUtf8 (B16.encode b), [(t,deps)])) (M.toList idx)

ht :: Int8 -> HashBuilder
ht x = HashBuilder (BB.int8 x) []

hi :: Int -> HashBuilder
hi x = HashBuilder (BB.int64LE $ fromIntegral x) []

hi' :: (Show a, Integral a) => a -> HashBuilder
hi' x | x' > toInteger (maxBound :: Int64) || x' < toInteger (minBound :: Int64) =
        Panic.panic $ "Gen2.Compactor.hi': integer out of range: " ++ show x
      | otherwise = HashBuilder (BB.int64LE $ fromInteger x') []
  where
    x' = toInteger x

hd :: Double -> HashBuilder
hd d = HashBuilder (BB.doubleLE d) []

htxt :: Text -> HashBuilder
htxt x = HashBuilder (BB.int64LE (fromIntegral $ BS.length bs) <> BB.byteString bs) []
  where
    bs = TE.encodeUtf8 x

hobj :: Text -> HashBuilder
hobj x = HashBuilder mempty [x]

hb :: BS.ByteString -> HashBuilder
hb x = HashBuilder (BB.int64LE (fromIntegral $ BS.length x) <> BB.byteString x) []

hashDefinitions :: Set Text -> JStat -> [(Text, HashBuilder)]
hashDefinitions globals st =
  let defs = findDefinitions st
  in  map (uncurry (hashSingleDefinition globals)) defs

findDefinitions :: JStat -> [(Ident, JExpr)]
findDefinitions (BlockStat ss)                    = concatMap findDefinitions ss
findDefinitions (AssignStat (ValExpr (JVar i)) e) = [(i,e)]
findDefinitions _                                 = []

hashSingleDefinition :: Set Text -> Ident -> JExpr -> (Text, HashBuilder)
hashSingleDefinition globals (TxtI i) expr = (i, ht 0 <> render st <> mconcat (map hobj globalRefs))
  where
    globalRefs = nub $ filter (`S.member` globals) (map (\(TxtI i) -> i) (expr ^.. identsE))
    globalMap  = M.fromList $ zip globalRefs (map (T.pack . ("h$$$global_"++) . show) [(1::Int)..])
    expr'      = expr & identsE %~ (\i@(TxtI t) -> maybe i TxtI (M.lookup t globalMap))
    st         = AssignStat (ValExpr (JVar (TxtI "dummy"))) expr'
    render     = htxt . TL.toStrict . displayT . renderPretty 0.8 150 . pretty

hashClosureInfo :: ClosureInfo -> (Text, HashBuilder)
hashClosureInfo (ClosureInfo civ cir cin cil cit cis) =
  (civ, ht 1 <> hashCIRegs cir <> hashCILayout cil <> hashCIType cit <> hashCIStatic cis)

hashStaticInfo :: StaticInfo -> (Text, HashBuilder)
hashStaticInfo (StaticInfo sivr sivl sicc) =
  (sivr, ht 2 <> hashStaticVal sivl)

hashCIType :: CIType -> HashBuilder
hashCIType (CIFun a r)  = ht 1 <> hi a <> hi r
hashCIType CIThunk      = ht 2
hashCIType (CICon c)    = ht 3 <> hi c
hashCIType CIPap        = ht 4
hashCIType CIBlackhole  = ht 5
hashCIType CIStackFrame = ht 6


hashCIRegs :: CIRegs -> HashBuilder
hashCIRegs CIRegsUnknown   = ht 1
hashCIRegs (CIRegs sk tys) = ht 2 <> hi sk <> hashList hashVT tys

hashCILayout :: CILayout -> HashBuilder
hashCILayout CILayoutVariable       = ht 1
hashCILayout (CILayoutUnknown size) = ht 2 <> hi size
hashCILayout (CILayoutFixed n l)    = ht 3 <> hi n <> hashList hashVT l

hashCIStatic :: CIStatic -> HashBuilder
hashCIStatic (CIStaticRefs xs) = hashList hobj xs

hashList :: (a -> HashBuilder) -> [a] -> HashBuilder
hashList f xs = hi (length xs) <> mconcat (map f xs)

hashVT :: VarType -> HashBuilder
hashVT = hi . fromEnum

hashStaticVal :: StaticVal -> HashBuilder
hashStaticVal (StaticFun t args)      = ht 1 <> hobj t <> hashList hashStaticArg args
hashStaticVal (StaticThunk mtn)       = ht 2 <> hashMaybe htobj mtn
  where
    htobj (o, args) = hobj o <> hashList hashStaticArg args
hashStaticVal (StaticUnboxed su)      = ht 3 <> hashStaticUnboxed su
hashStaticVal (StaticData dcon args)  = ht 4 <> hobj dcon <> hashList hashStaticArg args
hashStaticVal (StaticList args lt)    = ht 5 <> hashList hashStaticArg args <> hashMaybe hobj lt

hashMaybe :: (a -> HashBuilder) -> Maybe a -> HashBuilder
hashMaybe _ Nothing  = ht 1
hashMaybe f (Just x) = ht 2 <> f x

hashStaticUnboxed :: StaticUnboxed -> HashBuilder
hashStaticUnboxed (StaticUnboxedBool b)    = ht 1 <> hi (fromEnum b)
hashStaticUnboxed (StaticUnboxedInt iv)    = ht 2 <> hi' iv
hashStaticUnboxed (StaticUnboxedDouble sd) = ht 3 <> hashSaneDouble sd


hashStaticArg :: StaticArg -> HashBuilder
hashStaticArg (StaticObjArg t)       = ht 1 <> hobj t
hashStaticArg (StaticLitArg sl)      = ht 2 <> hashStaticLit sl
hashStaticArg (StaticConArg cn args) = ht 3 <> hobj cn <> hashList hashStaticArg args

hashStaticLit :: StaticLit -> HashBuilder
hashStaticLit (BoolLit b)       = ht 1 <> hi (fromEnum b)
hashStaticLit (IntLit iv)       = ht 2 <> hi (fromIntegral iv)
hashStaticLit  NullLit          = ht 3
hashStaticLit (DoubleLit d)     = ht 4 <> hashSaneDouble d
hashStaticLit (StringLit tt)    = ht 5 <> htxt tt
hashStaticLit (BinLit bs)       = ht 6 <> hb bs
hashStaticLit (LabelLit bb ln)  = ht 7 <> hi (fromEnum bb) <> htxt ln

hashSaneDouble :: SaneDouble -> HashBuilder
hashSaneDouble (SaneDouble sd) = hd sd

finalizeHash :: HashBuilder -> Hash
finalizeHash (HashBuilder hb tt) = (SHA256.hash (BL.toStrict $ BB.toLazyByteString hb), tt)
