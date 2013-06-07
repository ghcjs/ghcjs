import System.IO

f :: String -> String
f x = friends ++ family
  where friends = x
        family = " and family"

main = hSetBuffering stdout NoBuffering >> putStrLn (f "my friends")

