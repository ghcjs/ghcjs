{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-
   compatibility with older GHC
-}
module Compiler.Compat ( PackageKey
                       , packageKeyString
                       , modulePackageKey
                       , stringToPackageKey
                       , primPackageKey
                       , mainPackageKey
                       , modulePackageName
                       , getPackageName
                       , getPackageVersion
                       , getPackageLibDirs
                       , getPackageHsLibs
                       , searchModule
                       , Version(..)
                       , showVersion
                       , isEmptyVersion
                       ) where

import Module
import DynFlags
import FastString

#if !(__GLASGOW_HASKELL__ >= 709)
import Distribution.Package hiding ( PackageId )
import Packages hiding ( Version )
#else
import Packages hiding ( Version )
#endif

import           Data.Binary
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Version as DV


-- we do not support version tags since support for them has
-- been broken for a long time anyway
newtype Version = Version { unVersion :: [Integer] }
  deriving (Ord, Eq, Show, Binary)

showVersion :: Version -> Text
showVersion = T.intercalate "." . map (T.pack . show) . unVersion

isEmptyVersion :: Version -> Bool
isEmptyVersion = null . unVersion

convertVersion :: DV.Version -> Version
convertVersion v = Version (map fromIntegral $ versionBranch v)

#if !(__GLASGOW_HASKELL__ >= 709)

type PackageKey = PackageId

packageKeyString :: PackageKey -> String
packageKeyString = packageIdString

modulePackageKey :: Module -> PackageKey
modulePackageKey = modulePackageId

stringToPackageKey :: String -> PackageKey
stringToPackageKey = stringToPackageId

primPackageKey :: PackageKey
primPackageKey = primPackageId

mainPackageKey :: PackageKey
mainPackageKey = mainPackageId

modulePackageName :: DynFlags -> Module -> String
modulePackageName dflags
  = getPackageName dflags . modulePackageId

getPackageName :: DynFlags -> PackageKey -> String
getPackageName dflags
  = maybe "" ((\(PackageName n) -> n) . pkgName . sourcePackageId)
  . lookupPackage (pkgIdMap . pkgState $ dflags)

getPackageVersion :: DynFlags -> PackageKey -> Maybe Version
getPackageVersion dflags
  = fmap (convertVersion . pkgVersion . sourcePackageId)
  . lookupPackage (pkgIdMap . pkgState $ dflags)

getPackageLibDirs :: DynFlags -> PackageKey -> [FilePath]
getPackageLibDirs dflags
  = maybe [] libraryDirs . lookupPackage (pkgIdMap . pkgState $ dflags)

getPackageHsLibs :: DynFlags -> PackageKey -> [String]
getPackageHsLibs dflags
  = maybe [] hsLibraries . lookupPackage (pkgIdMap . pkgState $ dflags)

searchModule :: DynFlags -> ModuleName -> [(String, PackageKey)]
searchModule dflags
  = map ((\k -> (getPackageName dflags k, k)) . packageConfigId . fst)
  . lookupModuleInAllPackages dflags

#else

getPackageName :: DynFlags -> PackageKey -> String
getPackageName dflags
  = maybe "" ((\(PackageName n) -> unpackFS n) . packageName)
  . lookupPackage dflags

modulePackageName :: DynFlags -> Module -> String
modulePackageName dflags
  = getPackageName dflags . modulePackageKey

getPackageVersion :: DynFlags -> PackageKey -> Maybe Version
getPackageVersion dflags
  = fmap (convertVersion . packageVersion)
  . lookupPackage dflags

getPackageLibDirs :: DynFlags -> PackageKey -> [FilePath]
getPackageLibDirs dflags
  = maybe [] libraryDirs . lookupPackage dflags

getPackageHsLibs :: DynFlags -> PackageKey -> [String]
getPackageHsLibs dflags
  = maybe [] hsLibraries . lookupPackage dflags

searchModule :: DynFlags -> ModuleName -> [(String, PackageKey)]
searchModule dflags
  = map ((\k -> (getPackageName dflags k, k)) . packageKey . snd)
--  $ fromLookupResult
--  $ lookupModuleWithSuggestions dflags mn Nothing
  . lookupModuleInAllPackages dflags
{-
fromLookupResult :: LookupResult -> [(Module, PackageConfig)]
fromLookupResult (LookupFound m c)      = [(m,c)]
fromLookupResult (LookupMultiple ms)    = concatMap fromModuleOrigin ms
fromLookupResult (LookupHidden phs mhs) = concatMap fromModuleOrigin (phs ++ mhs)

fromModuleOrigin :: (Module, ModuleOrigin) -> [(Module, PackageConfig)]
fromModuleOrigin (m, mo) = case mo of
  -}
#endif
