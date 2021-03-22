{-# LANGUAGE CPP #-}
module Config
  ( module GHC.Version
  , cBuildPlatformString
  , cHostPlatformString
  , cProjectName
  , cBooterVersion
  , cStage
  ) where

import GhcPrelude

import GHC.Version

cBuildPlatformString :: String
cBuildPlatformString = "x86_64-unknown-linux"

cHostPlatformString :: String
cHostPlatformString = "x86_64-unknown-linux"

cProjectName          :: String
cProjectName          = "The Glorious Glasgow Haskell Compilation System"

cBooterVersion        :: String
cBooterVersion        = "8.10.1"

cStage                :: String
cStage                = show (2 :: Int)
