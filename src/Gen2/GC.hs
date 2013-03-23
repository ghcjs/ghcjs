{-# LANGUAGE QuasiQuotes #-}

{-
  some utilities for the gc, the implementation is in shims
 -}
module Gen2.GC where

import           Language.Javascript.JMacro

import           Data.Monoid

import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils

garbageCollector :: JStat
garbageCollector =
  [j| fun h$resetRegisters {
        `mconcat $ map resetRegister [minBound..maxBound]`;
      }

      fun h$resetResultVars {
        `mconcat $ map resetResultVar [minBound..maxBound]`;
      }
    |]

