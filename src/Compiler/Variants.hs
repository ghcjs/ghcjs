{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module Compiler.Variants where

import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import Generator.Helpers (runGen, newGenState)

import GHC
import StgSyn (StgBinding)

data CallingConvention = Plain | Trampoline

data Variant = Variant
    { variantExtension         :: String
    , variantCallingConvention :: CallingConvention
-- fixme, render to Text or ByteString instead
    , variantRender            :: StgPgm -> Module -> String
    }

variants :: [Variant]
variants = [ plainVariant, trampolineVariant ]

plainVariant :: Variant
plainVariant = Variant ".plain.js" Plain renderPlain

trampolineVariant :: Variant
trampolineVariant = Variant ".trampoline.js" Trampoline renderTrampoline

type StgPgm = [(StgBinding,[(Id,[Id])])]

renderPlain :: StgPgm -> Module -> String
renderPlain cg mn = show abs
  where
    abs :: Js.Formatted
    abs = renderAbstract cg mn

renderTrampoline :: StgPgm -> Module -> String
renderTrampoline cg mn = show abs
  where
    abs :: Js.Trampoline Js.Formatted
    abs = renderAbstract cg mn

renderAbstract :: (Javascript js) => StgPgm -> Module-> js
renderAbstract stg m = fst $ runGen (Js.generate m stg) newGenState

