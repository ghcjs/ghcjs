{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, MagicHash #-}

import Control.Monad

import Data.Bits
import Data.Int
import Data.Word

import Control.Applicative
import Control.Monad

import GHC.Char
import GHC.Int
import GHC.Prim
import GHC.Types
import GHC.Word
-- arithmetic with various int things

values :: (Bounded a, Num a, Bits a) => [a]
values = [minBound,maxBound,negate maxBound, -3, -2, -1, 0, 1, 2, 3] ++ map (1 `shiftL`) [2,3,4,8,15,16,31,32,33]
-- , 1000000000000, -100000000]

-- i8values :: [Int8]
-- i8values = [minBound] -- , maxBound,negate maxBound, -3, -2, -1, 0, 1, 2, 3] -- , 1000000000000, -100000000]

-- unary test functions
unary :: (Integral a, Num a, Bits a) => [a -> a]
unary = [(+1),(`div`2), (`shiftL` 1), (`shiftR` 1), (`shiftL` 5) , (`shiftR` 5), (`clearBit` 0), complement]

-- binary test functions
binary :: (Integral a, Num a, Bits a) => [a -> a -> a]
binary = [(*),(+),subtract,xor,(.|.),(.&.)]

testVals :: forall a. (Integral a, Num a, Bounded a, Bits a) => [a]
testVals = liftM3 (\f a b -> f a b) binary values values

main = do
{-
 let w@(W# pw) = 4294967295 :: Word
 print (pw `ltWord#` 10##)
 print w
 -}
  print (testVals :: [Int])
  print (testVals :: [Int8])
  print (testVals :: [Int16])
  print (testVals :: [Int32])
  print (testVals :: [Int64])
  print (testVals :: [Word])
  print (testVals :: [Word8])
  print (testVals :: [Word16])
  print (testVals :: [Word32])
  print (testVals :: [Word64])

