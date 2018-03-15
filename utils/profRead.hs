module Main where

import           Control.Applicative
import           System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Read
import           Data.List.Split

main = do
  args <- getArgs
  case args of
    [x]      -> profRead "ghcjs.prof" (read x)
    [file,x] -> profRead file (read x)
    _        -> putStrLn "usage: readProf [file] minimum"

profRead :: FilePath -> Double -> IO ()
profRead file m = do
  lines <- T.lines <$> T.readFile file
  mapM_ T.putStrLn (filter (isEnough m) lines)

isEnough :: Double -> T.Text -> Bool
isEnough m line
  | [fun,mod,src,no,entries,itime,ialloc,htime,halloc] <- words (T.unpack line)
      , Just ht <- readMaybe htime, Just ha <- readMaybe halloc = ht >= m || ha >= m
  | otherwise = True
