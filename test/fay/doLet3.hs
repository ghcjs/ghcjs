{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data R = R Int deriving Show

main = do
  print 1
  let [a] = [2]
  print a
  let (b,c) = (3,4)
  print b
  print c
  let R d = R 5
  print d
  let [e::Int] = []
  print e
