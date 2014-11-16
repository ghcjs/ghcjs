{-# LANGUAGE TemplateHaskell #-}

module Compiler.JMacro.Lens where

import Control.Lens
import Compiler.JMacro.Base

makeLenses ''JStat
makePrisms ''JStat
makeLenses ''JExpr
makePrisms ''JExpr
makeLenses ''JVal
makePrisms ''JVal
makeLenses ''Ident
makePrisms ''Ident

