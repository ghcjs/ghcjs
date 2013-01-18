f :: Int -> Int -> Int
f x y = x + y

g :: (Int, Int) -> Int
g (x,y) = x + y

main = do
  print $ curry g 3 4
  print $ uncurry f (3,4)

