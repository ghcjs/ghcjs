main = putStrLn (show (take 5 (let ns = 1 : map' (\x -> x + 1) ns in ns)))

map' f []     = []
map' f (x:xs) = f x : map' f xs
