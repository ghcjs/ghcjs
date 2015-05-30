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
import           Data.Char (chr, ord)
import           Data.Function (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Compiler.JMacro
import           Compiler.Settings

import           Gen2.Base
import           Gen2.ClosureInfo
import           Gen2.Utils (buildingProf, buildingDebug)

-- | collect global objects (data / CAFs). rename them and add them to the table
collectGlobals :: [StaticInfo]
               -> State CompactorState ()
collectGlobals = mapM_ (\(StaticInfo i _ _) -> renameObj i)

debugShowStat :: (JStat, [ClosureInfo], [StaticInfo]) -> String
debugShowStat (_s, cis, sis) = "closures:\n" ++ unlines (map show cis) ++ "\nstatics:" ++ unlines (map show sis) ++ "\n\n"

renameInternals :: GhcjsSettings
                -> DynFlags
                -> CompactorState
                -> [(JStat, [ClosureInfo], [StaticInfo])]
                -> (CompactorState, [JStat], JStat)
renameInternals _settings dflags cs0 stats0 = (cs, stats, meta)
  where
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
staticIdentsV f (StaticFun i)          = StaticFun <$> f i
staticIdentsV f (StaticThunk (Just i)) = StaticThunk . Just <$> f i
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
    | StaticFun f <- sv                           = [1, entry f]
    | StaticThunk (Just t) <- sv                  = [2, entry t]
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
        -> [(JStat, [ClosureInfo], [StaticInfo])]
        -> (CompactorState, [JStat], JStat) -- ^ renamer state, statements for each unit, metadata
compact settings dflags rs input =
  renameInternals settings dflags rs input

