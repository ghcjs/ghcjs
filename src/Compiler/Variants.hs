{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module Compiler.Variants where

import qualified Gen2.Generator as Gen2

import StgSyn (StgBinding)
import Module (Module(..))
import Id (Id)

data CallingConvention = Gen2

data Variant = Variant
    { variantExtension         :: String
    , variantCallingConvention :: CallingConvention
-- fixme, render to Text or ByteString instead
    , variantRender            :: StgPgm -> Module -> String
    }

variantExtension' = tail . variantExtension

variants :: [Variant]
variants = [ gen2Variant ]

gen2Variant :: Variant
gen2Variant = Variant ".gen2.js" Gen2 Gen2.generate

type StgPgm = [(StgBinding,[(Id,[Id])])]


