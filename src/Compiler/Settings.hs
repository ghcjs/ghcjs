{-# LANGUAGE TemplateHaskell #-}

module Compiler.Settings where

import           Compiler.JMacro

import qualified Gen2.Object         as Object

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad

import           Data.ByteString        (ByteString)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Map               (Map)
import qualified Data.Map            as M
import           Data.Monoid
import           Data.Set               (Set)
import qualified Data.Set            as S
import           Data.Text              (Text)

import           Module
import           DynFlags

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
  GhcjsSettings { gsNativeExecutables :: Bool
                , gsNoNative          :: Bool
                , gsNoJSExecutables   :: Bool
                , gsStripProgram      :: Maybe FilePath
                , gsLogCommandLine    :: Maybe FilePath
                , gsGhc               :: Maybe FilePath
                , gsOnlyOut           :: Bool
                , gsNoRts             :: Bool
                , gsNoStats           :: Bool
                , gsGenBase           :: Maybe String   -- ^ module name
                , gsUseBase           :: UseBase
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
  mempty = GhcjsSettings False False False Nothing Nothing Nothing False False False Nothing NoBase
  mappend (GhcjsSettings ne1 nn1 nj1 sp1 lc1 gh1 oo1 nr1 ns1 gb1 ub1)
          (GhcjsSettings ne2 nn2 nj2 sp2 lc2 gh2 oo2 nr2 ns2 gb2 ub2) =
          GhcjsSettings (ne1 || ne2)
                        (nn1 || nn2)
                        (nj1 || nj2)
                        (sp1 `mplus` sp2)
                        (lc1 `mplus` lc2)
                        (gh1 `mplus` gh2)
                        (oo1 || oo2)
                        (nr1 || nr2)
                        (ns1 || ns2)
                        (gb1 `mplus` gb2)
                        (ub1 <> ub2)

data GhcjsEnv = GhcjsEnv
  { compiledModules :: MVar (Map Module ByteString) -- ^ keep track of already compiled modules so we don't compile twice for dynamic-too
  }

newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty

buildingDebug :: DynFlags -> Bool
buildingDebug dflags = WayDebug `elem` ways dflags

data CompactorState =
  CompactorState { _identSupply   :: [Ident]               -- ^ ident supply for new names
                 , _nameMap       :: !(HashMap Text Ident) -- ^ renaming mapping for internal names
                 , _entries       :: !(HashMap Text Int)   -- ^ entry functions (these get listed in the metadata init array)
                 , _numEntries    :: !Int
                 , _statics       :: !(HashMap Text Int)   -- ^ mapping of global closure -> index in current block, for static initialisation
                 , _numStatics    :: !Int                  -- ^ number of static entries
                 , _labels        :: !(HashMap Text Int)   -- ^ non-Haskell JS labels
                 , _numLabels     :: !Int                  -- ^ number of labels
                 , _parentEntries :: !(HashMap Text Int)   -- ^ entry functions we're not linking, offset where parent gets [0..n], grantparent [n+1..k] etc
                 , _parentStatics :: !(HashMap Text Int)   -- ^ objects we're not linking in base bundle
                 , _parentLabels  :: !(HashMap Text Int)   -- ^ non-Haskell JS labels in parent
                 } deriving (Show)

data Base = Base { baseCompactorState :: CompactorState
                 , basePkgs           :: [Text]
                 , baseUnits          :: Set (Object.Package, Text, Int)
                 }

makeLenses ''CompactorState



