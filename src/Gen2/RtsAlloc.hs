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
    makeObjs = mconcat $ map (\(i,f,_) -> dec i <> [j| `i` = { f: `f`, d: null } |]) cls
    fillObjs = mconcat $ map fillObj cls
    fillObj (i,_,es) = [j| `i`.d = `JHash (M.fromList (zip dataFields es))` |]
    dataFields = map (('d':).show) [(1::Int)..]
    dec i | haveDecl  = decl i
          | otherwise = mempty
    checkObjs | rtsDebug  = mconcat $ map (\(i,_,_) -> [j| h$checkObj(`i`); |]) cls
              | otherwise = mempty

allocDynamic :: Bool -> Ident -> JExpr -> [JExpr] -> JStat
allocDynamic haveDecl to entry free =
  dec to <> [j| `to` = { f: `entry`, d: `fillObj` } |] <> checkObj
  where
    fillObj = JHash $ M.fromList (zip dataFields free)
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
