module Compiler.Settings where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

import Module

data GhcjsSettings = GhcjsSettings { gsNativeExecutables :: Bool
                                   , gsNoNative          :: Bool
                                   , gsNoJSExecutables   :: Bool
                                   , gsLogCommandLine    :: Maybe FilePath
                                   , gsGhc               :: Maybe FilePath
                                   , gsDebug             :: Bool
                                   , gsOnlyOut           :: Bool
                                   } deriving (Eq, Show)

instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False Nothing Nothing False False
  mappend (GhcjsSettings ne1 nn1 nj1 lc1 gh1 dbg1 oo1)
          (GhcjsSettings ne2 nn2 nj2 lc2 gh2 dbg2 oo2) =
          GhcjsSettings (ne1  || ne2)
                        (nn1  || nn2)
                        (nj1  || nj2)
                        (lc1  `mplus` lc2)
                        (gh1  `mplus` gh2)
                        (dbg1 || dbg2)
                        (oo1  || oo2)

data GhcjsEnv = GhcjsEnv
  { compiledModules :: MVar (Map Module ByteString)
  }

newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty
