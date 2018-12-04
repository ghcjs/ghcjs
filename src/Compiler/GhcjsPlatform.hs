{-# LANGUAGE CPP #-}
{-|
  ghcjs builds for a strange platform: like 32 bit
  instead of letting autoconf doing the defines, we override them here
  and try to get our own includes included instead of the library ones
-}
module Compiler.GhcjsPlatform
    (
      setGhcjsPlatform
    , setNativePlatform
    , setDfOpts
    ) where

import           DynFlags
import           Platform

import           Data.List (foldl')

import qualified Compiler.Info as Info
import           Compiler.GhcjsHooks
import           Compiler.Settings

-- | configure the GHC API for building 32 bit JavaScript code
setGhcjsPlatform :: GhcjsSettings
                 -> GhcjsEnv
                 -> [FilePath]  -- ^ JS objects for linking against
                 -> FilePath
                 -- ^ GHCJS base dir, usually "~/.ghcjs/platform-version/ghcjs"
                 -> DynFlags -> DynFlags
setGhcjsPlatform set js_env js_objs basePath df
  = addPlatformDefines basePath
      $ setDfOpts
      $ installGhcjsHooks js_env set js_objs
      $ installDriverHooks set js_env
      $ df { settings = settings' }
  where
    settings' = (settings df) { sTargetPlatform    = ghcjsPlatform
                              , sPlatformConstants = ghcjsPlatformConstants
#if __GLASGOW_HASKELL__ >= 709
                              , sProgramName       = "ghcjs"
                              , sProjectVersion    = Info.getFullCompilerVersion
#endif
                              }
    ghcjsPlatform = (sTargetPlatform (settings df))
       { platformArch     = ArchJavaScript
       , platformWordSize = 4
       }
    ghcjsPlatformConstants = (sPlatformConstants (settings df))
       { pc_WORD_SIZE       = 4
       , pc_DOUBLE_SIZE     = 8
       , pc_CINT_SIZE       = 4
       , pc_CLONG_SIZE      = 4
       , pc_CLONG_LONG_SIZE = 8
       , pc_WORDS_BIGENDIAN = False
       }

setNativePlatform :: GhcjsEnv -> GhcjsSettings -> FilePath -> DynFlags -> DynFlags
setNativePlatform env gs baseDir df
  = addPlatformDefines baseDir
  $ installNativeHooks env gs
  $ df

-- | Apply additional dynamic flags options.
-- Currently: unset 'Opt_SplitObjs'
setDfOpts :: DynFlags -> DynFlags
setDfOpts df = foldl' gopt_set (foldl' gopt_unset df unsetList) setList
  where
    setList = []
    unsetList = [Opt_SplitObjs]

addPlatformDefines :: FilePath -> DynFlags -> DynFlags
addPlatformDefines baseDir df = addCpp (("-I" ++ includeDir) : map ("-D"++) defs) $
                                df { includePaths = newIncludePaths }
  where
    ips        = includePaths df
    includeDir = baseDir ++ "/include"
    newIncludePaths = ips { includePathsGlobal = includeDir
                                               : includePathsGlobal ips
                          }
    defs = [ "__GHCJS__=" ++ Info.getShortCompilerVersion ]

addCpp :: [String] -> DynFlags -> DynFlags
addCpp cpp df = df { settings = settings1 }
  where
    settings0 = settings df
    settings1 = settings0 { sOpt_P = cpp ++ sOpt_P settings0 }
