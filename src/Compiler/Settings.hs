module Compiler.Settings where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

import Module
import DynFlags

data GhcjsSettings = GhcjsSettings { gsNativeExecutables :: Bool
                                   , gsNoNative          :: Bool
                                   , gsNoJSExecutables   :: Bool
                                   , gsLogCommandLine    :: Maybe FilePath
                                   , gsGhc               :: Maybe FilePath
                                   , gsOnlyOut           :: Bool
                                   , gsNoRts             :: Bool
                                   , gsNoStats           :: Bool
                                   , gsGenBase           :: Maybe String   -- ^ module name
                                   , gsUseBase           :: Maybe FilePath -- ^ symbs file name
                                   } deriving (Eq, Show)

instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False Nothing Nothing False False False Nothing Nothing
  mappend (GhcjsSettings ne1 nn1 nj1 lc1 gh1 oo1 nr1 ns1 gb1 ub1)
          (GhcjsSettings ne2 nn2 nj2 lc2 gh2 oo2 nr2 ns2 gb2 ub2) =
          GhcjsSettings (ne1  || ne2)
                        (nn1  || nn2)
                        (nj1  || nj2)
                        (lc1  `mplus` lc2)
                        (gh1  `mplus` gh2)
                        (oo1  || oo2)
                        (nr1  || nr2)
                        (ns1  || ns2)
                        (gb1  `mplus` gb2)
                        (ub1  `mplus` ub2)

data GhcjsEnv = GhcjsEnv
  { compiledModules :: MVar (Map Module ByteString)
  }

newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty

buildingDebug :: DynFlags -> Bool
buildingDebug dflags = WayDebug `elem` ways dflags

