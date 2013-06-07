
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.Integer

main = do
  print v
  print w

v :: Int
v = I# (integerToInt (smallInteger 3#))

w :: Word
w = W# (integerToWord (wordToInteger 3##))

