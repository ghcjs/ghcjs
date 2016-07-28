{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, ViewPatterns #-}
{-
  Manipulation of numeric literals. Warning: the Eq
  instance is not suitable for comparing the value of
  numbers!
 -}

module GHCJS.Tyr.Number where

import Data.Binary
import Data.Data
import Data.Int
import Data.Ratio
import Data.Typeable ()

import GHC.Generics

data Number
  = IntN !Integer
  | RatN !Rational
  | NegativeZero
  | Infinity
  | NegativeInfinity
  | NaN
  deriving (Eq, Show, Data, Typeable, Generic)

instance Binary Number

zero :: Number
zero = IntN 0

one :: Number
one = IntN 1

numIsNaN :: Number -> Bool
numIsNaN NaN = True
numIsNaN _   = False

isZero :: Number -> Bool
isZero (IntN x)     = x == 0
isZero (RatN x)     = x == 0
isZero NegativeZero = True
isZero _            = False

-- | does not include NegativeZero, use hasMinusLit for that
isNegative :: Number -> Bool
isNegative (IntN x)         = x < 0
isNegative (RatN x)         = x < 0
isNegative NegativeInfinity = True
isNegative _                = False

isNegativeZeroNum :: Number -> Bool
isNegativeZeroNum NegativeZero = True
isNegativeZeroNum _            = False

-- | number would start with a minus sign if printed to JS
hasMinusLit :: Number -> Bool
hasMinusLit n = isNegative n || isNegativeZeroNum n

negateNum :: Number -> Number
negateNum (canonic -> IntN x)
  | x == 0 = NegativeZero
  | otherwise = IntN (negate x)
negateNum (RatN r)         = RatN (negate r)
negateNum NegativeZero     = zero
negateNum Infinity         = NegativeInfinity
negateNum NegativeInfinity = Infinity
negateNum NaN              = NaN

canonic :: Number -> Number
canonic (RatN r)
  | denominator r == 1 = IntN (numerator r)
canonic x              = x

-- create an int value for the number, as if |0 was done in JS
intNum :: Number -> Number
-- intNum (IntN x) = IntN (x .&. ) -- fixme
intNum (IntN x)         = let i :: Int32
                              i = fromIntegral x
                          in IntN (fromIntegral i)
intNum (RatN x)         = let i :: Int32
                              i = floor x -- fixme is this correct?
                          in IntN (fromIntegral i)
intNum NegativeZero     = IntN 0
intNum Infinity         = zero
intNum NegativeInfinity = zero
intNum NaN              = zero

-- only nonnegative finite numeric values can be formatted
-- directly as a numeric literal
formatNumLit :: Number -> Maybe String
formatNumLit (canonic -> IntN x)
  | x >= 0 = Just (show x) -- fixme scientific (or hex) notation for large nums
formatNumLit (RatN r)
  | r > 0  = let d :: Double
                 d = realToFrac r
             in  Just (show d) -- fixme use a proper formatter
formatNumLit _ = Nothing

-- intValue :: 

-- add numbers as if they were ints
-- addNumInt :: Num -> Num -> Num
