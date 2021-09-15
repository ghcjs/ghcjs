{-# LANGUAGE BangPatterns, JavaScriptFFI, GHCForeignImportPrim #-}

module Main (main) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- Taken from the definition of "IS_THUNK" in ghcjs-base,
-- this checks if the tail of the list (.d2) is a thunk (.f.t === 0)
foreign import javascript unsafe
  "$r = $1.d2.f.t === 0;"
  js_isThunk :: Any -> IO Bool

testThunk :: a -> IO ()
testThunk a = do
  b <- js_isThunk $ unsafeCoerce a
  putStrLn $ case b of
    True -> "Is a thunk"
    False -> "Is not a thunk"

{-# NOINLINE irrefutable #-}
irrefutable :: String
irrefutable = fst $ foldl select ([], []) "ababababa"
  where
    select ~(ts, fs) x
      | x == 'a' = (x : ts, fs)
      | otherwise = (ts, x : fs)

main = do
  testThunk irrefutable
  putStrLn "rnf"
  () <- evaluate $ rnf irrefutable
  testThunk irrefutable
