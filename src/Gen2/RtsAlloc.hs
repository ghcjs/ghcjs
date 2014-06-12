{-# LANGUAGE QuasiQuotes,
             OverloadedStrings #-}

module Gen2.RtsAlloc where

import           Control.Lens

import           Data.Array
import           Data.Data.Lens
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T

import           Compiler.JMacro

import           Gen2.ClosureInfo
import           Gen2.Profiling
import           Gen2.RtsTypes
import           Gen2.Utils

-- allocate multiple, possibly mutually recursive, closures

allocDynAll :: CgSettings -> Bool -> [(Ident,JExpr,[JExpr],CostCentreStack)] -> G JStat
allocDynAll s haveDecl [(to,entry,free,cc)]
  | to `notElem` (free ^.. template) = do
      ccs <- ccsVar cc
      return $ allocDynamic s haveDecl to entry free [je| `ccs` |] -- <> [j| `to`.cc = `ccs` |]
allocDynAll s haveDecl cls = makeObjs <> return fillObjs <> return checkObjs
  where
    makeObjs :: G JStat
    makeObjs
      | csInlineAlloc s = mconcat $ map (\(i,f,_,cc) -> do
          ccs <- ccsVar cc
          return $ dec i <> [j| `i` = { f: `f`, d1: null, d2: null, m: 0, cc: `ccs` } |]) cls
      | otherwise       = mconcat $ map (\(i,f,_,cc) -> do
          ccs <- ccsVar cc
          return $ dec i <> [j| `i` = h$c(`f`, `ccs`); |]) cls
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

allocDynamic :: CgSettings -> Bool -> Ident -> JExpr -> [JExpr] -> JExpr -> JStat
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
