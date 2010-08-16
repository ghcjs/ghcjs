module Test (mymap, test) where

import Data.List

mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

test = sum [1,2,3,4,5]

