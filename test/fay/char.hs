module Main where

main :: IO ()
main = do
  print 'a'
  putStrLn ('a' : "bc")
  print (head "abc")
