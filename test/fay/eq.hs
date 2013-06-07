import Control.Monad

main = do
  when ((1::Int) == 1) $ putStrLn "Expected =="
  when ((1::Int) == 2) $ putStrLn "Unexpected =="
  when ((1::Int) /= 1) $ putStrLn "Unexpected /="
  when ((1::Int) /= 2) $ putStrLn "Expected /="
  when ((1::Double) == 1) $ putStrLn "Expected =="
  when ((1::Double) == 2) $ putStrLn "Unexpected =="
  when ((1::Double) /= 1) $ putStrLn "Unexpected /="
  when ((1::Double) /= 2) $ putStrLn "Expected /="
