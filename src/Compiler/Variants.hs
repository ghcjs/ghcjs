{-# LANGUAGE CPP #-}
{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module Compiler.Variants where

#if defined(GHCJS_PLAIN) || defined(GHCJS_TRAMPOLINE)
import           Generator.Helpers     (newGenState, runGen)
import           Generator.Link        (link)
import qualified Generator.TopLevel    as Js (generate)
import qualified Javascript.Formatted  as Js
import           Javascript.Language   (Javascript)
import qualified Javascript.Trampoline as Js
#endif

import           Module                (ModuleName)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

#ifdef GHCJS_GEN2
import qualified Gen2.Generator        as Gen2
import qualified Gen2.Linker           as Gen2
#endif

import           Id                    (Id)
import           Module                (Module (..))
import           StgSyn                (StgBinding)

data CallingConvention = Gen2 | Plain | Trampoline

data Variant = Variant
    { variantExtension         :: String
    , variantMetaExtension     :: Maybe String
    , variantExeExtension      :: String
    , variantCallingConvention :: CallingConvention
    , variantRender            :: StgPgm -> Module -> (ByteString, ByteString)
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
gen2Variant = Variant ".gen2.js" (Just ".gen2.ji") ".gen2.jsexe" Gen2 Gen2.generate
    Gen2.link
#endif

#ifdef GHCJS_PLAIN
plainVariant :: Variant
plainVariant = Variant ".plain.js" Nothing ".plain.jsexe" Plain renderPlain
    (link ".plain.js" . (++ ".plain.jsexe"))

renderPlain :: StgPgm -> Module -> (ByteString, ByteString)
renderPlain cg mn = (T.encodeUtf8 . T.pack . show $ abs, B.empty)
  where
    abs :: Js.Formatted
    abs = renderAbstract cg mn
#endif

#ifdef GHCJS_TRAMPOLINE
trampolineVariant :: Variant
trampolineVariant = Variant ".trampoline.js" Nothing ".trampoline.jsexe" Trampoline renderTrampoline
    (link ".trampoline.js" . (++ ".trampoline.jsexe"))

renderTrampoline :: StgPgm -> Module -> (ByteString, ByteString)
renderTrampoline cg mn = (T.encodeUtf8 . T.pack . show $ abs, B.empty)
  where
    abs :: Js.Trampoline Js.Formatted
    abs = renderAbstract cg mn

renderAbstract :: (Javascript js) => StgPgm -> Module-> js
renderAbstract stg m = fst $ runGen (Js.generate m stg) newGenState
#endif

type StgPgm = [(StgBinding,[(Id,[Id])])]


