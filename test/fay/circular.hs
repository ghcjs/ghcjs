main :: IO ()
main = let ys = 1 : xs
           xs = 2 : ys
       in print (take 25 xs)
