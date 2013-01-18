module Main where

ones :: [Int]
ones = 1 : ones

twos :: Int -> [Int]
twos f = 2 : twos f

threes = ones where
  ones = 1 : ones

threeConflict = head threeConflict where
  threeConflict = 3 : threeConflict

main = do
  print (head (tail ones))
  print (head (tail (twos 0)))
  print (head (tail threes))
  print threeConflict
