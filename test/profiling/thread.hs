import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import GHC.Stack

f1 :: IO [String]
f1 = f2 >>= return

f2 :: IO [String]
f2 = currentCallStack

main1 v ret = do
    putMVar ret . show =<< f1
    putMVar v ()

someF :: Int -> IO String
someF n = replicateM n (return '.')

main2 v ret = do
    putMVar ret . show =<< whoCreated =<< someF 10
    putMVar v ()

wait [] = return ()
wait vs@(v : r) = do
    readMVar v
    wait r

main = do
    var1 <- newEmptyMVar
    var2 <- newEmptyMVar

    ret1 <- newEmptyMVar
    ret2 <- newEmptyMVar

    i1 <- forkIO (main1 var1 ret1)
    i2 <- forkIO (main2 var2 ret2)

    wait [var1, var2]
    putStrLn =<< readMVar ret1
    putStrLn =<< readMVar ret2
