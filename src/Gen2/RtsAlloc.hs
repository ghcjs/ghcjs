{-# LANGUAGE QuasiQuotes #-}

module Gen2.RtsAlloc where

import           Data.Monoid
import           Language.Javascript.JMacro

import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils

-- allocate multiple, possible mutually recursive, closures
-- fixme make sure that all thunk objects have at least size 2
-- to allow overwriting by update object
allocDynAll :: Bool -> [(Ident,JExpr,[JExpr])] -> JStat
allocDynAll haveDecl cls = assignPtrs <> loadVars <> incHp
    where
      assignPtrs        = mconcat $ zipWith (\(i,_,_) o -> dec i <> [j| `JVar i` = `Hp`+`o`; |]) cls offsets
      loadVars          = mconcat $ zipWith (\(_,e,fr) o -> loadOff o e fr) cls offsets
      loadOff o e fr    = mconcat $ zipWith (\off v -> [j| `heapOff off` = `v` |]) [o..] (e:fr)
      loadVar o v       = [j| `heapOff o` = `v` |]
      clSize (_,_,free) = max 2 (1+length free) -- all closures at least 2
      offsets           = scanl (+) 0 (map clSize cls)
      heapOff 0         = [je| `Heap`[`Hp`]     |]
      heapOff n         = [je| `Heap`[`Hp`+`n`] |]
      dec i | haveDecl  = decl i
            | otherwise = mempty
      incHp             = traceRts ("allocated " |+ last offsets |+ " items at " |+ toJExpr Hp) <>
                          adjHp (last offsets)

allocDynamic :: Bool -> Ident -> JExpr -> [JExpr] -> JStat
allocDynamic haveDecl to entry free = allocDynAll haveDecl [(to,entry,free)]

allocStatic :: JExpr -> JExpr -> [JExpr] -> JStat
allocStatic to info args =
  [j| if(hpS + `n` >= hpDyn) { run_gc(); }
      `Heap`[hpS] = `info`;
      `mconcat storeArgs`;
      hpS += `n`;
  |]
    where
      n = length args
      storeArgs = zipWith (\a i -> [j| `Heap`[hpS+`i`] = `a` |]) args [(1::Int)..]

