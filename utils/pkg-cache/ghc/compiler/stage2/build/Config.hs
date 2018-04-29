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
cProjectGitCommitId   = "0000000000000000000000000000000000000000"
cProjectVersion       :: String
cProjectVersion       = "8.4.2.20180420"
cProjectVersionInt    :: String
cProjectVersionInt    = "804"
cProjectPatchLevel    :: String
cProjectPatchLevel    = "220180420"
cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "2"
cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20180420"
cBooterVersion        :: String
cBooterVersion        = "8.4.1.20180414"
cStage                :: String
cStage                = show (STAGE :: Int)
cIntegerLibrary       :: String
cIntegerLibrary       = "integer-gmp"
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
cGhcRTSWays           = "l debug thr thr_debug thr_l thr_p dyn debug_dyn thr_dyn thr_debug_dyn l_dyn thr_l_dyn"
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
