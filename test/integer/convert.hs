-- various Integer conversions

import Control.Monad
import Data.Int
import Data.Word

w32 :: [Word32]
w32 = [0,-1,1,1000000,-1000000,maxBound]

w64 :: [Word64]
w64 = [0,-1,1,1000000,-1000000,maxBound]

w8 :: [Word8]
w8 = [0,-1,1,100,-100,maxBound]

i32 :: [Int32]
i32 = [minBound,-1,0,1,maxBound]

i64 :: [Int64]
i64 = [minBound,-1,0,1,maxBound]

i :: [Int]
i = [minBound,-1,0,1,maxBound]

funs :: [Integer -> Integer]
funs = [id,(*2),(*3),(`div`2),(`div`3),(*(-2)), (+1), subtract 1]

toW64 :: Integer -> Word64
toW64 = fromInteger
toW32 :: Integer -> Word32
toW32 = fromInteger
toW8 :: Integer -> Word8
toW8 = fromInteger

toI64 :: Integer -> Int64
toI64 = fromInteger
toI32 :: Integer -> Int32
toI32 = fromInteger
toI :: Integer -> Int
toI = fromInteger

main = do
  print $ liftM2 ($) funs (map fromIntegral w64)
  print $ map toW64 $ liftM2 ($) funs (map fromIntegral w64)
  print $ liftM2 ($) funs (map fromIntegral w32)
  print $ map toW32 $ liftM2 ($) funs (map fromIntegral w32)
  print $ liftM2 ($) funs (map fromIntegral w8)
  print $ map toW8 $ liftM2 ($) funs (map fromIntegral w8)
  print $ liftM2 ($) funs (map fromIntegral i64)
  print $ map toI64 $ liftM2 ($) funs (map fromIntegral i64)
  print $ liftM2 ($) funs (map fromIntegral i32)
  print $ map toI32 $ liftM2 ($) funs (map fromIntegral i32)
  print $ liftM2 ($) funs (map fromIntegral i)
  print $ map toI $ liftM2 ($) funs (map fromIntegral i)

