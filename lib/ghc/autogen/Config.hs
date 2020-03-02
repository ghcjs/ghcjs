{-# LANGUAGE CPP #-}
module Config where

import GhcPrelude

#include "ghc_boot_platform.h"

data IntegerLibrary = IntegerGMP
                    | IntegerSimple
                    deriving Eq

cBuildPlatformString :: String
cBuildPlatformString = BuildPlatform_NAME
cHostPlatformString :: String
cHostPlatformString = HostPlatform_NAME
cTargetPlatformString :: String
cTargetPlatformString = TargetPlatform_NAME

cProjectName          :: String
cProjectName          = "The Glorious Glasgow Haskell Compilation System"
cProjectGitCommitId   :: String
cProjectGitCommitId   = "dc8b78fc7ecb616ab535a03b4410f610ef8dd3d0"
cProjectVersion       :: String
cProjectVersion       = "8.8.1.20200101"
cProjectVersionInt    :: String
cProjectVersionInt    = "808"
cProjectPatchLevel    :: String
cProjectPatchLevel    = "120200101"
cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "1"
cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20200101"
cBooterVersion        :: String
cBooterVersion        = "8.6.5"
cStage                :: String
cStage                = show (STAGE :: Int)
cIntegerLibraryType   :: IntegerLibrary
cIntegerLibraryType   = IntegerGMP
cSupportsSplitObjs    :: String
cSupportsSplitObjs    = "YES"
cGhcWithInterpreter   :: String
cGhcWithInterpreter   = "YES"
cGhcWithNativeCodeGen :: String
cGhcWithNativeCodeGen = "YES"
cGhcWithSMP           :: String
cGhcWithSMP           = "YES"
cGhcRTSWays           :: String
cGhcRTSWays           = "l debug thr thr_debug thr_l thr_p dyn debug_dyn thr_dyn thr_debug_dyn l_dyn thr_l_dyn thr_debug_p debug_p"
cGhcRtsWithLibdw      :: Bool
cGhcRtsWithLibdw      = False
cGhcEnableTablesNextToCode :: String
cGhcEnableTablesNextToCode = "YES"
cLeadingUnderscore    :: String
cLeadingUnderscore    = "NO"
cGHC_UNLIT_PGM        :: String
cGHC_UNLIT_PGM        = "unlit"
cGHC_SPLIT_PGM        :: String
cGHC_SPLIT_PGM        = "ghc-split"
cLibFFI               :: Bool
cLibFFI               = False
cGhcThreaded :: Bool
cGhcThreaded = True
cGhcDebugged :: Bool
cGhcDebugged = False
