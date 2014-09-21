{-# LANGUAGE TemplateHaskell #-}

module Compiler.Settings where

import           Gen2.Base

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad

import           Data.ByteString        (ByteString)
import           Data.Map               (Map)
import qualified Data.Map            as M
import           Data.Monoid

import           System.IO
import           System.Process

import           Module

{- | We can link incrementally against a base bundle, where we assume
     that the symbols from the bundle and their dependencies have already
     been loaded. We need to save the CompactorState so that we can
     do consistent renaming.

     Incremental linking greatly improves link time and can also
     be used in multi-page or repl-type applications to serve
     most of the code from a static location, reloading only the
     small parts that are actually different.
 -}
data UseBase = NoBase             -- ^ don't use incremental linking
             | BaseFile  FilePath -- ^ load base from file
             | BaseState Base     -- ^ use this base

instance Monoid UseBase where
  mempty             = NoBase
  x `mappend` NoBase = x
  _ `mappend` x      = x

data GhcjsSettings =
  GhcjsSettings { gsNativeExecutables  :: Bool
                , gsNativeToo          :: Bool
                , gsBuildingCabalSetup :: Bool
                , gsNoJSExecutables    :: Bool
                , gsStripProgram       :: Maybe FilePath
                , gsLogCommandLine     :: Maybe FilePath
                , gsGhc                :: Maybe FilePath
                , gsOnlyOut            :: Bool
                , gsNoRts              :: Bool
                , gsNoStats            :: Bool
                , gsGenBase            :: Maybe String   -- ^ module name
                , gsUseBase            :: UseBase
                }

usingBase :: GhcjsSettings -> Bool
usingBase s | NoBase <- gsUseBase s = False
            | otherwise             = True

-- | we generate a runnable all.js only if we link a complete application,
--   no incremental linking and no skipped parts
generateAllJs :: GhcjsSettings -> Bool
generateAllJs s
  | NoBase <- gsUseBase s = not (gsOnlyOut s) && not (gsNoRts s)
  | otherwise             = False

{-
  this instance is supposed to capture overriding settings, where one group
  comes from the environment (env vars, config files) and the other
  from the command line. (env `mappend` cmdLine) should give the combined
  settings, but it doesn't work very well. find something better.
 -}
instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False False Nothing Nothing Nothing False False False Nothing NoBase
  mappend (GhcjsSettings ne1 nn1 bc1 nj1 sp1 lc1 gh1 oo1 nr1 ns1 gb1 ub1)
          (GhcjsSettings ne2 nn2 bc2 nj2 sp2 lc2 gh2 oo2 nr2 ns2 gb2 ub2) =
          GhcjsSettings (ne1 || ne2)
                        (nn1 || nn2)
                        (bc1 || bc2)
                        (nj1 || nj2)
                        (sp1 `mplus` sp2)
                        (lc1 `mplus` lc2)
                        (gh1 `mplus` gh2)
                        (oo1 || oo2)
                        (nr1 || nr2)
                        (ns1 || ns2)
                        (gb1 `mplus` gb2)
                        (ub1 <> ub2)

data ThRunner =
  ThRunner { thrProcess        :: ProcessHandle
           , thrHandleIn       :: Handle
           , thrHandleErr      :: Handle
           , thrBase           :: MVar Base
           }

data GhcjsEnv = GhcjsEnv
  { compiledModules :: MVar (Map Module ByteString) -- ^ keep track of already compiled modules so we don't compile twice for dynamic-too
  , thRunners :: MVar (Map String ThRunner) -- ^ template haskell runners
  , thSplice :: MVar Int
  }

newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty <*> newMVar M.empty <*> newMVar 0

