
{-# LANGUAGE QuasiQuotes,
             OverloadedStrings #-}

module Gen2.RtsAlloc where

import           DynFlags

import           Control.Lens hiding ((||=))

import           Data.Array
import           Data.Data.Lens
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

import           Compiler.JMacro

import           Gen2.ClosureInfo
import           Gen2.Profiling
import           Gen2.RtsTypes
import           Gen2.Utils

-- init a variable sized object from an array of values
initClosure :: DynFlags -> JExpr -> JExpr -> JExpr -> JExpr
initClosure dflags entry values ccs
  | buildingProf dflags =
      [je| h$init_closure({ f: `entry`, d1: null, d2: null, m: 0, cc: `ccs`}, `values`) |]
  | otherwise =
      [je| h$init_closure({ f: `entry`, d1: null, d2: null, m: 0}, `values`) |]

-- allocate multiple, possibly mutually recursive, closures

allocDynAll :: CgSettings -> Bool -> Maybe JStat -> [(Ident,JExpr,[JExpr],CostCentreStack)] -> G JStat
allocDynAll s haveDecl middle [(to,entry,free,cc)]
  | isNothing middle && to `notElem` (free ^.. template) = do
      ccs <- ccsVarJ cc
      return $ allocDynamic s haveDecl to entry free ccs
allocDynAll s haveDecl middle cls = makeObjs <> return middle' <> return fillObjs <> return checkObjs
  where
    middle' = fromMaybe mempty middle

    makeObjs :: G JStat
    makeObjs
      | csInlineAlloc s = mconcat $ flip map cls $ \(i,f,_,cc) -> do
          ccs <- costCentreStackLbl cc
          return $ dec i <> i |= (ValExpr (jhFromList $ [("f", f), ("d1", jnull), ("d2", jnull), ("m", ji 0)]
                                              ++ maybe [] (\(TxtI cid) -> [("cc", jsv cid)]) ccs))
      | otherwise       = mconcat $ flip map cls $ \(i,f,_,cc) -> do
          ccs <- costCentreStackLbl cc
          return $ dec i <> i |= ("h$c" |^^ ([f] ++ maybe [] (\(TxtI cid) -> [jsv cid]) ccs))

    fillObjs = mconcat $ map fillObj cls
    fillObj (i,_,es,_)
      | csInlineAlloc s || length es > 24 =
          case es of
            []      -> mempty
            [e]     -> [j| `i`.d1 = `e`; |]
            [e1,e2] -> [j| `i`.d1 = `e1`; `i`.d2 = `e2`; |]
            (e:es)  -> [j| `i`.d1 = `e`; `i`.d2 = `JHash (M.fromList (zip dataFields es))` |]
      | otherwise = case es of
            []      -> mempty
            [e]     -> [j| `i`.d1 = `e`; |]
            [e1,e2] -> [j| `i`.d1 = `e1`; `i`.d2 = `e2`; |]
            (e:es)  -> [j| `i`.d1 = `e`; `i`.d2 = `fillFun es`; |]

    fillFun [] = [je| null |]
    fillFun es = ApplExpr (allocData ! length es) es

    dataFields = map (T.pack . ('d':) . show) [(1::Int)..]
    dec i | haveDecl  = decl i
          | otherwise = mempty
    checkObjs | csAssertRts s  = mconcat $ map (\(i,_,_,_) -> [j| h$checkObj(`i`); |]) cls
              | otherwise = mempty

allocDynamic :: CgSettings -> Bool -> Ident -> JExpr -> [JExpr] -> Maybe JExpr -> JStat
allocDynamic s haveDecl to entry free cc =
  dec to <> [j| `to` = `allocDynamicE s entry free cc`; |]
    where
      dec i | haveDecl  = decl i
            | otherwise = mempty

-- fixme push unupdate thunks?
{-
allocStatic :: JExpr -> JExpr -> [JExpr] -> JStat
allocStatic to info args =
  [j| `to` = { f: `info`, d: `fillObj` } |]
  where
    dataFields = map (('d':).show) [(1::Int)..]
    fillObj = JHash $ M.fromList (zip dataFields args)
-}
{-
   [j| if(hpS + `n` >= hpDyn) { run_gc(); }
      `Heap`[hpS] = `info`;
      `mconcat storeArgs`;
      hpS += `n`;
  |]
    where
      n = length args
      storeArgs = zipWith (\a i -> [j| `Heap`[hpS+`i`] = `a` |]) args [(1::Int)..]
-}
