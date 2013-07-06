{-# LANGUAGE CPP #-}
{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module Compiler.Variants where

import           Module                (ModuleName)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import qualified Gen2.Generator        as Gen2
import qualified Gen2.Linker           as Gen2

import           DynFlags              (DynFlags)
import           Id                    (Id)
import           Module                (Module (..))
import           StgSyn                (StgBinding)

data CallingConvention = Gen2

data Variant = Variant
    { variantExtension         :: String
    , variantMetaExtension     :: Maybe String
    , variantExeExtension      :: String
    , variantCallingConvention :: CallingConvention
    , variantRender            :: Bool -> DynFlags -> StgPgm -> Module -> (ByteString, ByteString)
    , variantLink              :: Bool -> String -> [FilePath] -> [FilePath] -> [ModuleName] -> IO [String]
    }

variantExtension' = tail . variantExtension

variants :: [Variant]
variants = [gen2Variant]

gen2Variant :: Variant
gen2Variant = Variant ".js" (Just ".ji") ".jsexe" Gen2 Gen2.generate
    Gen2.link

type StgPgm = [StgBinding]



