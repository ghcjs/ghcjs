{-# LANGUAGE CPP #-}
{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module Compiler.Variants where

import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import Generator.Helpers (runGen, newGenState)
import Module (ModuleName)
import Generator.Link (link)

#ifdef GHCJS_GEN2
import qualified Gen2.Generator as Gen2
#endif

import StgSyn (StgBinding)
import Module (Module(..))
import Id (Id)

data CallingConvention = Gen2 | Plain | Trampoline

data Variant = Variant
    { variantExtension         :: String
    , variantExeExtension      :: String
    , variantCallingConvention :: CallingConvention
-- fixme, render to Text or ByteString instead
    , variantRender            :: StgPgm -> Module -> String
    , variantLink              :: String -> [FilePath] -> [FilePath] -> [ModuleName] -> IO [String]
    }

variantExtension' = tail . variantExtension

variants :: [Variant]
variants =
#ifdef GHCJS_GEN2
    gen2Variant :
#endif
#ifdef GHCJS_PLAIN
    plainVariant :
#endif
#ifdef GHCJS_TRAMPOLINE
    trampolineVariant :
#endif
    []

#ifdef GHCJS_GEN2
gen2Variant :: Variant
gen2Variant = Variant ".gen2.js" ".gen2.jsexe" Gen2 Gen2.generate
    (link ".gen2.js" . (++ ".gen2.jsexe"))
#endif

plainVariant :: Variant
plainVariant = Variant ".plain.js" ".plain.jsexe" Plain renderPlain
    (link ".plain.js" . (++ ".plain.jsexe"))

renderPlain :: StgPgm -> Module -> String
renderPlain cg mn = show abs
  where
    abs :: Js.Formatted
    abs = renderAbstract cg mn

trampolineVariant :: Variant
trampolineVariant = Variant ".trampoline.js" ".trampoline.jsexe" Trampoline renderTrampoline
    (link ".trampoline.js" . (++ ".trampoline.jsexe"))

renderTrampoline :: StgPgm -> Module -> String
renderTrampoline cg mn = show abs
  where
    abs :: Js.Trampoline Js.Formatted
    abs = renderAbstract cg mn

renderAbstract :: (Javascript js) => StgPgm -> Module-> js
renderAbstract stg m = fst $ runGen (Js.generate m stg) newGenState

type StgPgm = [(StgBinding,[(Id,[Id])])]


