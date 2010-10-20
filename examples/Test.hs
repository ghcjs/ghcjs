module Test where

import Data.List

test1 :: Int
test1 = sum [1..5]

test2 :: Int
test2 = product [1..5]

test3 :: Int
test3 = product [2..10]

test4 :: String
test4 = show test3

test5 :: String
test5 = "Hello World"

test6 :: String
test6 = show (sum [1..5] :: Integer)

test7 :: String
test7 = show $ take 7 primes

primes :: [Int]
primes = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

test8 :: String
test8 = show (product [1..5] :: Integer)

