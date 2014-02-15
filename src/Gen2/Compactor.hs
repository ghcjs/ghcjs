{-# LANGUAGE QuasiQuotes,
             ScopedTypeVariables,
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

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict

import           Data.Array
import qualified Data.Binary as DB
import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import qualified Data.ByteString.Lazy as BL
import           Data.Char (chr)
import           Data.Data.Lens
import           Data.Function (on)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Compiler.JMacro

import           Compiler.Settings

import           Gen2.Utils
import           Gen2.ClosureInfo
import           Gen2.Object
import qualified Gen2.Optimizer as Optimizer

compact :: GhcjsSettings
        -> RenamerState
        -> [(JStat,[ClosureInfo])]
        -> (RenamerState, [JStat],JStat)
compact settings rs input =
  renameInternals settings rs input

data RenamerState = RenamerState [Ident] (HashMap Text Ident)

renamedVars :: [Ident]
renamedVars = map (\(TxtI xs) -> TxtI ("h$$"<>xs)) Optimizer.newLocals

emptyRenamerState :: RenamerState
emptyRenamerState = RenamerState renamedVars HM.empty

renameInternals :: GhcjsSettings
                -> RenamerState
                -> [(JStat,[ClosureInfo])]
                -> (RenamerState, [JStat], JStat)
renameInternals settings rs stats = (rs', stats', meta)
  where
    ((stats', meta), rs') = runState renamed rs
    renamed = (,) <$> mapM doRename stats <*> metadata (stats >>= snd)
    doRename (stat, ci)
      | gsDebug settings = do
         rci <- renderClosureInfo settings ci
         return (stat <> rci)
      | otherwise = identsS renameVar stat
    metadata cis
      | gsDebug settings = return mempty -- encoded for each block separately
      | otherwise        = renderClosureInfo settings cis

renameVar :: Ident -> State RenamerState Ident
renameVar i@(TxtI xs)
  | "h$$" `T.isPrefixOf` xs = do
      (RenamerState (y:ys) m) <- get
      case HM.lookup xs m of
        Just r  -> return r
        Nothing -> put (RenamerState ys (HM.insert xs y m)) >> return y
  | otherwise = return i

renderClosureInfo :: GhcjsSettings -> [ClosureInfo] -> State RenamerState JStat
renderClosureInfo settings cis =
   fmap (renderInfoBlock settings . renameClosureInfo cis) get

renameClosureInfo :: [ClosureInfo] -> RenamerState -> [ClosureInfo]
renameClosureInfo cis (RenamerState _ m) =
  let m' = HM.fromList . map (\(k,TxtI v) -> (k, v)) $ HM.toList m
  in  map (g m m') cis
   where
    g m0 m (ClosureInfo v rs n l t s) =
        (ClosureInfo (fromMaybe v $ HM.lookup v m) rs n l t (h m s))
    h _ CINoStatic = CINoStatic
    h m0 (CIStaticRefs rs) = CIStaticRefs (map (\sr -> fromMaybe sr $ HM.lookup sr m0) rs)

renderInfoBlock :: GhcjsSettings -> [ClosureInfo] -> JStat
renderInfoBlock settings infos
  | gsDebug settings = mconcat (map (closureInfoStat True) infos)
  | otherwise =
      [j| h$initStatic.push(\ {
            var !h$functions = `funArr`;
            var !h$info      = `infoTables`;
            h$initInfoTables(`nfuns`, h$functions, h$info);
          });
        |]
  where
    infos' = sortBy (compare `on` ciVar) infos
    infoTables :: String
    infoTables = encodeStr (concatMap (encodeInfo m) infos')
    funArr :: [Ident]
    funArr = map TxtI (funs ++ extras)
    s = S.fromList funs
    m = M.fromList $ zip symbols [0..]
    symbols = funs ++ extras
    funs = map ciVar infos'
    nfuns = length funs
    extras = filter (`S.notMember` s) allSrts

    allSrts = let getSrts inf = case ciStatic inf of
                                  CINoStatic      -> []
                                  CIStaticRefs xs -> xs
              in S.toList $ S.fromList (concatMap getSrts infos)
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
    c i | i > 90 || i < 0 = error "encodeStr: c"
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
      | otherwise = error "encodeStr"

encodeInfo :: Map Text Int -> ClosureInfo -> [Int]
encodeInfo m (ClosureInfo var regs name layout typ static)
  | CIThunk            <- typ = [0] ++ ls
  | (CIFun arity regs) <- typ = [1, arity, regs] ++ ls
  | (CICon tag)        <- typ = [2, tag] ++ ls
-- | (CIPap ar)         <- typ = [3, ar] ++ ls
  | otherwise                 = error "encodeInfo"
  where
    vi       = funIdx var
    funIdx t = fromMaybe (error $ "encodeInfo: funIdx: " ++ T.unpack t) (M.lookup t m)
    ls       = encodeLayout layout ++ encodeSrt static
    encodeLayout CILayoutVariable      = [0]
    encodeLayout (CILayoutPtrs s ptrs) = [s+1] -- ,length ptrs] ++ ptrs
    encodeLayout (CILayoutFixed s vs)   = [s+1] -- ,length vs] ++ map fromEnum vs
    encodeSrt CINoStatic = [0]
    encodeSrt (CIStaticRefs rs) = length rs : map funIdx rs


{-
  Base files contain a list of functions already linked from
  elsewhere. They also keep track of linked packages and the
  data required for link-time optimization

  base format:
  GHCJSBASE
  [renamer state]
  [linkedPackages]
  [packages]
  [modules]
  [symbols]
 -}

data Base = Base { baseRenamerState :: RenamerState
                 , basePkgs         :: [Text]
                 , baseUnits        :: Set (Package, Text, Int)
                 }

emptyBase :: Base
emptyBase = Base emptyRenamerState [] S.empty

renderBase :: RenamerState                           -- ^ renamer state
           -> [Text]                                 -- ^ package linked
           -> Set (Package, Text, Int)               -- ^ linkable units contained in base
           -> BL.ByteString                          -- ^ rendered result
renderBase rs packages funs = DB.runPut $ do
  DB.putByteString "GHCJSBASE"
  putRs rs
  putList DB.put packages
  putList putPkg pkgs
  putList DB.put mods
  putList putFun (S.toList funs)
  where
    pi :: Int -> DB.Put
    pi = DB.putWord32le . fromIntegral
    uniq :: Ord a => [a] -> [a]
    uniq  = S.toList . S.fromList
    pkgs  = uniq (map (\(x,_,_) -> x) $ S.toList funs)
    pkgsM = M.fromList (zip pkgs [(0::Int)..])
    mods  = uniq (map (\(_,x,_) -> x) $ S.toList funs)
    modsM = M.fromList (zip mods [(0::Int)..])
    putList f xs = pi (length xs) >> mapM_ f xs
    putRs (RenamerState (ns:_) hm) = do
      pi (HM.size hm)
      putRs' renamedVars (HM.fromList . map (\(x,y) -> (y,x)) . HM.toList $ hm)
    putRs' (n:ns) hm
      | Just v <- HM.lookup n hm = DB.put v >> putRs' ns hm
      | otherwise                = return ()
    putPkg (Package n v) = DB.put n >> DB.put v
    -- fixme group things first
    putFun (p,m,s) = pi (pkgsM M.! p) >> pi (modsM M.! m) >> DB.put s

loadBase :: Maybe FilePath -> IO Base
loadBase Nothing = return emptyBase
loadBase (Just file) = DB.runGet getBase <$> BL.readFile file
  where
    gi :: DB.Get Int
    gi = fromIntegral <$> DB.getWord32le
    getList f = DB.getWord32le >>= \n -> replicateM (fromIntegral n) f
    getFun ps ms = {- Fun -} (,,) <$> ((ps!) <$> gi) <*> ((ms!) <$> gi) <*> DB.get
    la xs = listArray (0, length xs - 1) xs
    getPkg = Package <$> DB.get <*> DB.get
    getRs = do
      n  <- gi
      let (used, unused) = splitAt n renamedVars
      renamed <- replicateM n DB.get
      return (RenamerState unused $ HM.fromList (zip renamed used))
    getBase = do
      hdr <- DB.getByteString 9
      when (hdr /= "GHCJSBASE") (error "loadBase: invalid base file")
      rs <- getRs
      linkedPackages <- getList DB.get
      pkgs <- la <$> getList getPkg
      mods <- la <$> getList DB.get
      funs <- getList (getFun pkgs mods)
      return (Base rs linkedPackages $ S.fromList funs)


----------------------------

{-# INLINE identsS #-}
identsS :: Traversal' JStat Ident
identsS f (DeclStat i mt)      = DeclStat       <$> f i <*> pure mt
identsS f (ReturnStat e)       = ReturnStat     <$> identsE f e
identsS f (IfStat e s1 s2)     = IfStat         <$> identsE f e <*> identsS f s1 <*> identsS f s2
identsS f (WhileStat b e s)    = WhileStat b    <$> identsE f e <*> identsS f s
identsS f (ForInStat b i e s)  = ForInStat b    <$> f i <*> identsE f e <*> identsS f s
identsS f (SwitchStat e xs s)  = SwitchStat     <$> identsE f e <*> (traverse . traverseCase) f xs <*> identsS f s
  where traverseCase g (e,s) = (,) <$> identsE g e <*> identsS g s
identsS f (TryStat s1 i s2 s3) = TryStat        <$> identsS f s1 <*> f i <*> identsS f s2 <*> identsS f s3
identsS f (BlockStat xs)       = BlockStat      <$> (traverse . identsS) f xs
identsS f (ApplStat e es)      = ApplStat       <$> identsE f e <*> (traverse . identsE) f es
identsS f (PPostStat b op e)   = PPostStat b op <$> identsE f e
identsS f (AssignStat e1 e2)   = AssignStat     <$> identsE f e1 <*> identsE f e2
identsS f (UnsatBlock{})       = error "identsS: UnsatBlock"
identsS f (AntiStat{})         = error "identsS: AntiStat"
identsS f (ForeignStat{})      = error "identsS: ForeignStat"
identsS f (LabelStat l s)      = LabelStat l    <$> identsS f s
identsS f b@(BreakStat{})      = pure b
identsS f c@(ContinueStat{})   = pure c

{-# INLINE identsE #-}
identsE :: Traversal' JExpr Ident
identsE f (ValExpr v)         = ValExpr       <$> identsV f v
identsE f (SelExpr e i)       = SelExpr       <$> identsE f e <*> pure i -- do not rename properties
identsE f (IdxExpr e1 e2)     = IdxExpr       <$> identsE f e1 <*> identsE f e2
identsE f (InfixExpr s e1 e2) = InfixExpr s   <$> identsE f e1 <*> identsE f e2
identsE f (PPostExpr b s e)   = PPostExpr b s <$> identsE f e
identsE f (IfExpr e1 e2 e3)   = IfExpr        <$> identsE f e1 <*> identsE f e2 <*> identsE f e3
identsE f (ApplExpr e es)     = ApplExpr      <$> identsE f e <*> (traverse . identsE) f es
identsE f (UnsatExpr{})       = error "identsE: UnsatExpr"
identsE f (AntiExpr{})        = error "identsE: AntiExpr"
identsE f (TypeExpr{})        = error "identsE: TypeExpr"

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
