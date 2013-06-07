isPositive :: Double -> Bool
isPositive x | x > 0 = True
             | x <= 0 = False

threeConds x | x > 1 = 2
             | x == 1 = 1
             | x < 1 = 0

withOtherwise x | x > 1 = True
                | otherwise = False

-- Not called, throws "non-exhaustive guard"
nonExhaustive x | x > 1 = True

main = do
  putStrLn $ show [isPositive 1, isPositive 0]
  putStrLn $ show [threeConds 3, threeConds 1, threeConds 0]
  putStrLn $ show [withOtherwise 2, withOtherwise 0]
