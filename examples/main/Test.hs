{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables, MagicHash #-}
module Test (test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13) where

import Data.List
import Foreign.C.Types
import Foreign.Ptr
import Control.Concurrent
import System.Mem.Weak
import GHC.Prim
import GHC.CString (unpackCString#)

type CString = Addr#

data JSObject

foreign import ccall "hs_cons"
  jscons :: CChar -> Ptr JSObject -> IO (Ptr JSObject)
foreign import ccall "hs_nil"
  jsnil :: IO (Ptr JSObject)
foreign import ccall "logResult"
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

test11 :: CString -> Int
test11 s = read $ unpackCString# s

test12 :: CString -> IO Int
test12 ca = do
    let a = unpackCString# ca
    addFinalizer a $ string2JSString "Finalize Called" >>= jsalert
    string2JSString ("Work done (ref still held) " ++ show (sum [1..10000])) >>= jsalert
    string2JSString ("You passed in " ++ a ++ " (ref could now be freed)") >>= jsalert
    string2JSString ("More work done " ++ show (sum [1..10000])) >>= jsalert
    return 1

-- Recusive let
test13 :: CString -> String
test13 cx =
    let x = unpackCString# cx
        a, b :: Int -> String
        a 0 = "A"
        a n  = b (n-1)
        b 0 = "B"
        b n  = a (n-1)
    in a (read x) ++ b (read x)















