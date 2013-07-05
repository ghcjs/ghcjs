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

import           Data.Maybe
import           Data.Monoid
import           Data.List
import           Language.Javascript.JMacro
import           Control.Monad.State.Lazy
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Control.Lens
import           Data.Data.Lens

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

-- fixme this can be stored much more compactly
renderClosureInfo :: [ClosureInfo] -> State RenamerState JStat
renderClosureInfo cis = fmap (mconcat . map toStat . renameClosureInfo cis) get

renameClosureInfo :: [ClosureInfo] -> RenamerState -> [ClosureInfo]
renameClosureInfo cis (RenamerState _ m) =
  let m' = M.fromList . map (\(k,StrI v) -> (T.pack k, T.pack v)) $ M.toList m
  in  map (g m m') cis
   where
    g m0 m (ClosureInfo v rs n l t s) =
        (ClosureInfo (fromMaybe v $ M.lookup v m) rs n l t (h m0 s))
    h _ CINoStatic = CINoStatic
    h m0 (CIStaticRefs rs) = CIStaticRefs (map (\sr@(StrI r) -> fromMaybe sr $ M.lookup r m0) rs)

