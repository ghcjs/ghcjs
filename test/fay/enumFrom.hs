import Control.Monad

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' n []     = []
take' n (x:xs) = x : take' (n-1) xs

main :: IO ()
main = do
    forM_ [1..5] $ \i -> print i
    forM_ (take' 5 [1..]) $ \i -> print i
    forM_ [1,3..9] $ \i -> print i
    forM_ (take' 3 [1,3..]) $ \i -> print i
