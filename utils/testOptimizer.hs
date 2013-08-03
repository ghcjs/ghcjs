{-# LANGUAGE OverloadedStrings #-}
module Main where

{-
  quicly bisect optimizer problems by running part of the code optimized

  - modify Optimizer.hs to emit if(runOptimized) { optimized } else { unoptimized } code
  - adjust the runOptimized variable to find the problem
-}
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Applicative

main = do
  src <- T.lines <$> T.readFile "all.js"
  T.putStrLn "var runOptimized = 1"
  mapM_ T.putStrLn (go src 1)

go :: [Text] -> Int -> [Text]
go [] _ = []
go (x:xs) n
  | "runOptimized" `T.isInfixOf` x =
       T.replace "runOptimized" ("runOptimized >= " <> T.pack (show n)) x : go xs (n+1)
  | otherwise = x : go xs n

