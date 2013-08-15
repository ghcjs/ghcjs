{-# LANGUAGE OverloadedStrings, LambdaCase #-}

{-
  Print the size of each section in a GHCJS object file
  optionally dump the sections
 -}
module Main where

import           Control.Applicative
import           Control.Monad (when)

import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Int

import           System.Environment

main = do
  getArgs >>= \case
                 ["-d", x] -> getObjInfo False x
                 [x]       -> getObjInfo True  x
                 _   -> error "usage: objInfo [-d] objfile\n   -d  : dump the sections to files"

hdrLen :: Int64
hdrLen = 32

getObjInfo dump file = do
  b <- BL.readFile file
  let ~(header, symbsLen, depsLen, idxLen) = runGet getHeader b
  if BL.length b < hdrLen || header /= "GHCJSOBJ"
    then putStrLn "not a GHCJS object"
    else do
      putStrLn ("GHCJS object `" ++ file ++ "' section sizes in bytes:")
      putStrLn ("header:   " ++ show hdrLen)
      putStrLn ("symbols:  " ++ show symbsLen)
      putStrLn ("deps:     " ++ show depsLen)
      putStrLn ("index:    " ++ show idxLen)
      putStrLn ("units:    " ++ show (BL.length b - 32 - symbsLen - depsLen - idxLen))
      putStrLn ""
      putStrLn ("total:    " ++ show (BL.length b))
      when dump $ do
        BL.writeFile (file ++ ".symbols") (BL.take symbsLen $ BL.drop hdrLen b)
        BL.writeFile (file ++ ".deps")    (BL.take depsLen  $ BL.drop (hdrLen + symbsLen) b)
        BL.writeFile (file ++ ".index")   (BL.take idxLen   $ BL.drop (hdrLen + symbsLen + depsLen) b)
        BL.writeFile (file ++ ".units")   (                   BL.drop (hdrLen + symbsLen + depsLen + idxLen) b)

getHeader :: Get (BL.ByteString, Int64, Int64, Int64)
getHeader = (,,,) <$> getLazyByteString 8 <*> gi <*> gi <*> gi
  where
    gi = fmap fromIntegral getWord64le

