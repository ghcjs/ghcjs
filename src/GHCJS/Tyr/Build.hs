{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving #-}
{-
  build IR AST
 -}

module GHCJS.Tyr.Build where

import Control.Lens hiding (assign)

import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Ratio

import Panic

import GHCJS.Tyr.Base
import GHCJS.Tyr.Number
import GHCJS.Tyr.Operators
import GHCJS.Tyr.Types
