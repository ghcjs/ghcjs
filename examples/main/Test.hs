{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Test where

import Data.List
import Foreign.C.Types
import Foreign.Ptr
import Control.Concurrent

data JSObject

foreign import ccall "zdhszicons" -- $hs.cons
  jscons :: CChar -> Ptr JSObject -> IO (Ptr JSObject)
foreign import ccall "zdhszinil" -- $hs.nil
  jsnil :: IO (Ptr JSObject)
foreign import ccall "alert"
  jsalert :: Ptr JSObject -> IO ()

string2JSString :: String -> IO (Ptr JSObject)
string2JSString [] = jsnil
string2JSString (x:xs) =
  do t <- string2JSString xs
     jscons (toEnum . fromEnum $ x) t

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

test9 :: IO Int
test9 =
  do s <- string2JSString "Haskell says hello"
     jsalert s
     return 9

test10 :: IO Int
test10 =
  do 
     x <- newEmptyMVar
     forkIO $ do
         putMVar x 5
     y <- takeMVar x
     return y

