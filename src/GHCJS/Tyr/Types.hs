{-# LANGUAGE DeriveDataTypeable,
             DeriveGeneric,
             TypeSynonymInstances,
             FlexibleInstances,
             TemplateHaskell
  #-}

module GHCJS.Tyr.Types where

import Control.Applicative
import Control.Lens

import Data.Array
import Data.Data
import Data.Graph
import Data.Int
import Data.List (intersect)
import Data.Monoid
import Data.Text (Text)

import GHC.Generics

import Id
import Panic
import Module

import Gen2.StgAst

import GHCJS.Tyr.Base

data JId
  = JId !Name
  | JIdUndefined
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

type JExp   = Exp  HId Ty ()
type JStat  = St   HId Ty ()
type JLVal  = LVal HId Ty ()

type Name   = Text

type Ty     = ()
type HId    = ()

