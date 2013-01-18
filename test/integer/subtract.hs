
module Main where

import Control.Applicative
{-
import Control.Monad
import Data.Int
{-
x :: [Int]
x = [minBound]

-- main = print (liftM2 subtract x x)
--main = print ([subtract] <*> x <*> x)
-}

x :: Int
x = 2
-}

main = print test0

{-# NOINLINE test0 #-}
test0 = head ([flip (*)] <*> pure (2::Int) <*> pure 2)

