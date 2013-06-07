-- For testing +RTS -xc
import Control.Exception
import System.IO

main :: IO ()
main = print =<< (try (evaluate (f ())) :: IO (Either SomeException ()))

f x = g x

g x = error (show x)
