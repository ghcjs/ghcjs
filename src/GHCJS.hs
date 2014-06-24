{-# LANGUAGE ScopedTypeVariables #-}
module GHCJS
    (
      module GHC
    , module Compiler.GhcjsProgram
    , module Compiler.Program
    ) where

import GHC hiding (setSessionDynFlags)
import Compiler.GhcjsProgram
import Compiler.Program

