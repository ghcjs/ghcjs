raw :: Either Int Int -> Int
raw x = case x of Left a -> a + 1
                  Right b -> b + 2

func :: Either Int Int -> Int
func x = either (\x -> x + 1) (\x -> x + 2) x

main = do
  print $ raw $ Left 5
  print $ raw $ Right 5
  print $ func $ Left 5
  print $ func $ Right 5
