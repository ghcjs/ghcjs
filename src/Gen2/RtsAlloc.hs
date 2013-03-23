{-# LANGUAGE QuasiQuotes #-}

module Gen2.RtsAlloc where

import Data.Monoid
import Language.Javascript.JMacro

import qualified Data.Map as M
import Gen2.RtsSettings
import Gen2.RtsTypes
import Gen2.Utils
-- allocate multiple, possible mutually recursive, closures
-- fixme make sure that all thunk objects have at least size 2
-- to allow overwriting by update object
allocDynAll :: Bool -> [(Ident,JExpr,[JExpr])] -> JStat
-- allocDynAll haveDecl [(to,entry,free)] = allocDynamic haveDecl to entry free
allocDynAll haveDecl cls = makeObjs <> fillObjs <> checkObjs
  where
    makeObjs = mconcat $ map (\(i,f,_) -> dec i <> [j| `i` = { f: `f`, d1: null, d2: null, m: 0 } |]) cls
    fillObjs = mconcat $ map fillObj cls
    fillObj (i,_,es) =
      case es of
        []  -> mempty
        [e] -> [j| `i`.d1 = `e`; |]
        [e1,e2] -> [j| `i`.d1 = `e1`; `i`.d2 = `e2`; |]
        (e:es)   -> [j| `i`.d1 = `e`; `i`.d2 = `JHash (M.fromList (zip dataFields es))` |]
    dataFields = map (('d':).show) [(1::Int)..]
    dec i | haveDecl  = decl i
          | otherwise = mempty
    checkObjs | rtsDebug  = mconcat $ map (\(i,_,_) -> [j| h$checkObj(`i`); |]) cls
              | otherwise = mempty

allocDynamic :: Bool -> Ident -> JExpr -> [JExpr] -> JStat
allocDynamic haveDecl to entry free =
  dec to <> [j| `to` = { f: `entry`, d1: `fillObj1`, d2: `fillObj2`, m: 0 } |] <> checkObj
  where
    (fillObj1,fillObj2)
       = case free of
                []  -> (jnull, jnull)
                [x] -> (x,jnull)
                [x,y] -> (x,y)
                (x:xs) -> (x,toJExpr (JHash $ M.fromList (zip dataFields xs)))
    dataFields = map (('d':).show) [(1::Int)..]
    dec i | haveDecl  = decl i
          | otherwise = mempty
    checkObj | rtsDebug = [j| h$checkObj(`to`); |]
             | otherwise = mempty

-- fixme push unupdate thunks?
allocStatic :: JExpr -> JExpr -> [JExpr] -> JStat
allocStatic to info args =
  [j| `to` = { f: `info`, d: `fillObj` } |]
  where
    dataFields = map (('d':).show) [(1::Int)..]
    fillObj = JHash $ M.fromList (zip dataFields args)

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
