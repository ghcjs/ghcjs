someFun x = fun x
  where fun x | x < 50 = "ok"
              | otherwise = "nop"

main = do
    putStrLn (someFun 30)
    putStrLn (someFun 100)
