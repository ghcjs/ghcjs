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
import           Control.Monad.State

import           Data.Char (chr)
import           Data.Data.Lens
import           Data.Function (on)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Javascript.JMacro

import           Gen2.Utils
import           Gen2.ClosureInfo
import qualified Gen2.Optimizer as Optimizer

compact :: Bool -> JStat -> [ClosureInfo] -> JStat
compact debug input ci = renameInternals debug input ci

data RenamerState = RenamerState [Ident] (Map Text Ident)

renameInternals :: Bool -> JStat -> [ClosureInfo] -> JStat
renameInternals debug stat ci = evalState doRename
  (RenamerState (map (\(TxtI xs) -> TxtI ("h$$"<>xs)) Optimizer.newLocals) M.empty)
  where
    doRename = do
       s'  <- if debug then return stat else identsS renameVar stat
       rci <- renderClosureInfo debug ci
       return (s' <> rci)

renameVar :: Ident -> State RenamerState Ident
renameVar i@(TxtI xs)
  | "h$$" `T.isPrefixOf` xs = do
      (RenamerState (y:ys) m) <- get
      case M.lookup xs m of
        Just r  -> return r
        Nothing -> put (RenamerState ys (M.insert xs y m)) >> return y
  | otherwise = return i

renderClosureInfo :: Bool -> [ClosureInfo] -> State RenamerState JStat
renderClosureInfo debug cis =
   fmap (renderInfoBlock debug . renameClosureInfo cis) get

renameClosureInfo :: [ClosureInfo] -> RenamerState -> [ClosureInfo]
renameClosureInfo cis (RenamerState _ m) =
  let m' = M.fromList . map (\(k,TxtI v) -> (k, v)) $ M.toList m
  in  map (g m m') cis
   where
    g m0 m (ClosureInfo v rs n l t s) =
        (ClosureInfo (fromMaybe v $ M.lookup v m) rs n l t (h m s))
    h _ CINoStatic = CINoStatic
    h m0 (CIStaticRefs rs) = CIStaticRefs (map (\sr -> fromMaybe sr $ M.lookup sr m0) rs)

renderInfoBlock :: Bool -> [ClosureInfo] -> JStat
renderInfoBlock debug infos
  | debug = mconcat (map (closureInfoStat True) infos)
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
