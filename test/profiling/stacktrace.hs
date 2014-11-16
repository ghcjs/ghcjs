{-# LANGUAGE FlexibleContexts #-}

module Main where

import GHC.Stack
import Control.Monad (ap)
import Control.Applicative

newtype M s a = M { unM :: s -> (s, a) }

instance Functor (M s) where
  fmap f s = s >>= \a -> return (f a)

instance Applicative (M s) where
  pure  = return
  (<*>) = ap

instance Monad (M s) where
    (M m) >>= k = M $ \s -> case m s of
                              (s', a) -> unM (k a) s'
    return a = M $ \s -> (s, a)

errorM :: String -> M s a
errorM s = M $ \_ -> errorWithStackTrace ("--- " ++ s)

runM :: M s a -> s -> a
runM (M m) s = let (_, a) = m s in a

someF :: Int -> IO String
someF n = replicateM n (return '.')

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n x = sequence (replicate n x)

bar :: [String] -> M s [String]
bar xs = mapM foo xs

foo :: String -> M s String
foo s = (\x -> (\s -> errorM s) x) s

main :: IO ()
main = do
    print =<< whoCreated 1
    print =<< whoCreated (1 :: Int)
    print =<< whoCreated '.'
    print =<< whoCreated bar
    print =<< currentCallStack
    print =<< whoCreated =<< someF 10
    print =<< whoCreated someF
    print (runM (bar ["a"]) "state")
