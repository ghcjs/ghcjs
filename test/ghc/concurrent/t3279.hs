-- test for #3279

import System.IO.Unsafe
import GHC.Conc
import Control.Exception
import GHC.IO (unsafeUnmask)

debug _ = return ()
-- debug = putStrLn

f :: Int
f = (1 +) . unsafePerformIO $ do
        error "foo" `catch` \(SomeException e) -> do
            debug "f handler"
            myThreadId >>= flip throwTo e
            debug "exception thrown"
            -- point X
            unsafeUnmask $ (debug "unmasked" >> return 1)

main :: IO ()
main = do
    evaluate f `catch` \(SomeException e) -> (debug "got exception" >> return 0)
    debug "one"
    -- the evaluation of 'x' is now suspended at point X
    tid <- mask_ $ forkIO (evaluate f >> debug "after eval" >> return ())
    debug "two"
    killThread tid
    -- now execute the 'unblock' above with a pending exception
    debug "three"
    yield
    debug "four"
    -- should print 1 + 1 = 2
    print f
    
