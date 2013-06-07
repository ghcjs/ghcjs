module Main where

-- Stack overflow if the tail recursion does not work

goInt :: Integer -> Integer -> Int
goInt 500 1000 = 0
goInt   x 1000 = goInt (x+1) 1
goInt   x    y = goInt x     (y+1)

main = print $ goInt 1 1
