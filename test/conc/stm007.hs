{-# LANGUAGE ScopedTypeVariables #-}

-- testcase by Ryan Yates

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E

test x = x `E.catch` \(e::E.SomeException) -> print e

main = do
    (a,b) <- atomically $
        do a <- newTVar True
           b <- newTVar True
           always (writeTVar b True >> always (readTVar b) >> readTVar a)
           return (a,b)

    putStrLn "Testing b"
    test $ atomically (writeTVar b False) -- Should not fail
    putStrLn "Testing a"
    test $ atomically (writeTVar a False) -- Should fail

{- expected output:
Testing b
Testing a
*** Exception: Transactional invariant violation
-}
