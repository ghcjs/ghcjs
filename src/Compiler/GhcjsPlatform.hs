{-|
  ghcjs builds for a strange platform: like 32 bit
  instead of letting autoconf doing the defines, we override them here
  and try to get our own includes included instead of the library ones
-}
module Compiler.GhcjsPlatform
    (
      setGhcjsPlatform
    , setDfOpts
    ) where

import GHC
import DynFlags
import Platform
import Outputable (showSDocOneLine)

import Data.List (foldl', isPrefixOf)

import Compiler.Settings
import Compiler.GhcjsHooks

import qualified Compiler.Utils as Util

-- | configure the GHC API for building 32 bit JavaScript code
setGhcjsPlatform :: GhcjsSettings
                 -> [FilePath]  -- ^ JS objects for linking against
                 -> FilePath
                 -- ^ GHCJS base dir, usually "~/.ghcjs/platform-version"
                 -> DynFlags -> DynFlags
setGhcjsPlatform set js_objs basePath df
  = addPlatformDefines basePath
      $ setDfOpts
      $ installGhcjsHooks set js_objs
      $ installDriverHooks set
      $ df { settings = settings' }
  where
    settings' = (settings df) { sTargetPlatform    = ghcjsPlatform
                              , sPlatformConstants = ghcjsPlatformConstants
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

-- | Apply additional dynamic flags options.
-- Currently: unset 'Opt_SplitObjs'
setDfOpts :: DynFlags -> DynFlags
setDfOpts df = foldl' setOpt (foldl' unsetOpt df unsetList) setList
  where
    setList = []
    unsetList = [Opt_SplitObjs]

addPlatformDefines :: FilePath -> DynFlags -> DynFlags
addPlatformDefines baseDir df = df { settings = settings1
                                   , includePaths = includeDir : includePaths df
                                   }
  where
    includeDir = baseDir ++ "/include"
    -- ^ fixme: shouldn't be necessary if builtin_rts has this in its include-dirs?
    settings0 = settings df
    settings1 = settings0 { sOpt_P = ("-I" ++ includeDir) : map ("-D"++) defs ++ sOpt_P settings0 }
    defs = [ "__GHCJS__"
           , "__GHCAUTOCONF_H__=1"
           , "__GHCCONFIG_H__=1"
           , "SIZEOF_CHAR=1"
           , "ALIGNMENT_CHAR=1"
           , "SIZEOF_UNSIGNED_CHAR=1"
           , "ALIGNMENT_UNSIGNED_CHAR=1"
           , "SIZEOF_SHORT=2"
           , "ALIGNMENT_SHORT=2"
           , "SIZEOF_UNSIGNED_SHORT=2"
           , "ALIGNMENT_UNSIGNED_SHORT=2"
           , "SIZEOF_INT=4"
           , "ALIGNMENT_INT=4"
           , "SIZEOF_UNSIGNED_INT=4"
           , "ALIGNMENT_UNSIGNED_INT=4"
           , "SIZEOF_LONG=4"
           , "ALIGNMENT_LONG=4"
           , "SIZEOF_UNSIGNED_LONG=4"
           , "ALIGNMENT_UNSIGNED_LONG=4"
           , "HAVE_LONG_LONG=1"
           , "SIZEOF_LONG_LONG=8"
           , "ALIGNMENT_LONG_LONG=8"
           , "SIZEOF_UNSIGNED_LONG_LONG=8"
           , "ALIGNMENT_UNSIGNED_LONG_LONG=8"
           , "SIZEOF_VOID_P=4"
           , "ALIGNMENT_VOID_P=4"
           , "SIZEOF_DOUBLE=8"
           , "ALIGNMENT_DOUBLE=8"
           , "SIZEOF_FLOAT=4"
           , "ALIGNMENT_FLOAT=4"
           ]

setOpt = gopt_set
unsetOpt = gopt_unset


