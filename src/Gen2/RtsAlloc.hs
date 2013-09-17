{-# LANGUAGE QuasiQuotes,
             OverloadedStrings #-}

module Gen2.RtsAlloc where

import Control.Lens
import Data.Array
import Data.Data.Lens
import Data.Monoid
import Language.Javascript.JMacro

import qualified Data.Map as M
import qualified Data.Text as T
import Gen2.RtsSettings
import Gen2.RtsTypes
import Gen2.Utils

allocClsA :: Array Int JExpr
allocClsA = listArray (0, 1024) (toJExpr (TxtI "h$c") : map f [1..1024])
  where
    f n = toJExpr . TxtI . T.pack $ "h$c" ++ show n

allocData :: Array Int JExpr
allocData = listArray (1, 1024) (map f [1..1024])
  where
    f n = toJExpr . TxtI . T.pack $ "h$d" ++ show n

-- allocate multiple, possibly mutually recursive, closures

allocDynAll :: Bool -> [(Ident,JExpr,[JExpr])] -> JStat
allocDynAll haveDecl [(to,entry,free)]
  | to `notElem` (free ^.. template) = allocDynamic haveDecl to entry free
allocDynAll haveDecl cls = makeObjs <> fillObjs <> checkObjs
  where
    makeObjs
      | rtsInlineAlloc = mconcat $ map (\(i,f,_) -> dec i <> [j| `i` = { f: `f`, d1: null, d2: null, m: 0 } |]) cls
      | otherwise      = mconcat $ map (\(i,f,_) -> dec i <> [j| `i` = h$c(`f`); |]) cls
    fillObjs = mconcat $ map fillObj cls
    fillObj (i,_,es)
      | rtsInlineAlloc || length es > 24 =
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
    fillFun es = ApplExpr (allocData ! length es) es -- (toJExpr . TxtI $ "h$d" ++ show (length es)) es

    dataFields = map (T.pack . ('d':) . show) [(1::Int)..]
    dec i | haveDecl  = decl i
          | otherwise = mempty
    checkObjs | rtsDebug  = mconcat $ map (\(i,_,_) -> [j| h$checkObj(`i`); |]) cls
              | otherwise = mempty

allocDynamic :: Bool -> Ident -> JExpr -> [JExpr] -> JStat
allocDynamic haveDecl to entry free =
  dec to <> [j| `to` = `allocDynamicE entry free`; |]
    where
      dec i | haveDecl  = decl i
            | otherwise = mempty

allocDynamicE :: JExpr -> [JExpr] -> JExpr
allocDynamicE entry free
  | rtsInlineAlloc || length free > 24
      = [je| { f: `entry`, d1: `fillObj1`, d2: `fillObj2`, m: 0 } |]
  | otherwise = ApplExpr allocFun (toJExpr entry : free)
  where
    allocFun = allocClsA ! length free
    (fillObj1,fillObj2)
       = case free of
                []  -> (jnull, jnull)
                [x] -> (x,jnull)
                [x,y] -> (x,y)
                (x:xs) -> (x,toJExpr (JHash $ M.fromList (zip dataFields xs)))
    dataFields = map (T.pack . ('d':) . show) [(1::Int)..]

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
