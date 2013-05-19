module Main where

run :: IO ()
run = do
    runSink (Sink putStrLn) "hello"

newtype Sink a = Sink { runSink :: a -> IO () }

main :: IO ()
main = run
