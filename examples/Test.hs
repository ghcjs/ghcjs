module Test where

import Data.List

mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

test :: Integer
test = sum [1,2,3,4,5]

test1 :: Int
test1 = sum [1,2,3,4,5]

test2 :: Int
test2 = fromInteger test

test3 :: String
test3 = show $ (product [1..3] :: Integer)
