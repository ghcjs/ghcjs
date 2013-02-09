{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-
  A cabal-install wrapper for GHCJS
  - invokes cabal-install with the correct arguments to build ghcjs packages
  - uses the GHCJS cache to install .js files for installed packages
     (temporary fix until proper GHCJS support is merged into the Cabal library)
-}

module Main where

import           Prelude                hiding (FilePath, catch)

import           System.Exit            (exitWith)
import           System.Process         (rawSystem, readProcess)

import           System.Directory       (createDirectoryIfMissing,
                                         getAppUserDataDirectory,
                                         removeDirectoryRecursive)

import           Data.Monoid
import           System.Environment     (getArgs)

import           Compiler.Cache
import           Compiler.Info
import           Compiler.Variants

extraArgs = do
  prefix <- getAppUserDataDirectory "ghcjs-cabal"
  return [ "--with-compiler=ghcjs"
         , "--with-hc-pkg=ghcjs-pkg"
         , "--with-hsc2hs=hsc2hs"
         , "--disable-documentation"
         , "--prefix=" <> prefix <> "/" <> getGhcjsCompilerVersion
         , "--bindir=" <> prefix <> "/" <> "bin"
         ]

main :: IO ()
main = do
  emptyCache
  args <- getArgs
  extra <- extraArgs
  ec <- if any (`elem` ["install", "configure"]) args
    then rawSystem "cabal" (extra ++ args)
    else rawSystem "cabal" args
  installFromCache
  exitWith ec


