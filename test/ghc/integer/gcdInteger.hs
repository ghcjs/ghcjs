
{-# LANGUAGE MagicHash #-}

module Main (main) where

import GHC.Base
import GHC.Integer
import Data.Int

main :: IO ()
main = case i of
       I# i# ->
           print (gcd (smallInteger i#) (smallInteger i#))

{-# NOINLINE i #-}
i :: Int
i = fromIntegral (minBound::Int32)

