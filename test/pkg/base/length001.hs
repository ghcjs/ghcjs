
module Main (main) where

import Data.List

main :: IO ()
main = do print (genericLength [1..100000] :: Int)
          print (genericLength [1..100000] :: Integer)
