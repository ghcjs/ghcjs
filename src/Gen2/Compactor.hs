{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

{-
  The compactor does link-time optimization. It is much simpler
  than the Optimizer, no fancy dataflow analysis here.

  Optimizations:
  - rewrite all variables starting with h$$ to shorter names,
       these are internal names
  - write all function metadata compactly
 -}
module Gen2.Compactor where

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

compact :: JStat -> [ClosureInfo] -> JStat
compact input ci = renameInternals input ci

data RenamerState = RenamerState [Ident] (Map String Ident)

renameInternals :: JStat -> [ClosureInfo] -> JStat
renameInternals stat ci = evalState doRename
  (RenamerState (map (\(StrI xs) -> StrI ("h$$"++xs)) Optimizer.newLocals) M.empty)
  where
    doRename = do
       s'  <- template renameVar stat
       rci <- renderClosureInfo ci
       return (s' <> rci)

renameVar :: Ident -> State RenamerState Ident
renameVar i@(StrI xs)
  | "h$$" `isPrefixOf` xs = do
      (RenamerState (y:ys) m) <- get
      case M.lookup xs m of
        Just r  -> return r
        Nothing -> put (RenamerState ys (M.insert xs y m)) >> return y
  | otherwise = return i

renderClosureInfo :: [ClosureInfo] -> State RenamerState JStat
renderClosureInfo cis = fmap (renderInfoBlock . renameClosureInfo cis) get

renameClosureInfo :: [ClosureInfo] -> RenamerState -> [ClosureInfo]
renameClosureInfo cis (RenamerState _ m) =
  let m' = M.fromList . map (\(k,StrI v) -> (T.pack k, T.pack v)) $ M.toList m
  in  map (g m m') cis
   where
    g m0 m (ClosureInfo v rs n l t s) =
        (ClosureInfo (fromMaybe v $ M.lookup v m) rs n l t (h m s))
    h _ CINoStatic = CINoStatic
    h m0 (CIStaticRefs rs) = CIStaticRefs (map (\sr -> fromMaybe sr $ M.lookup sr m0) rs)

renderInfoBlock :: [ClosureInfo] -> JStat
renderInfoBlock infos = -- mconcat (map toStat infos) <>
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
    funArr = map (StrI . T.unpack) (funs ++ extras)
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

