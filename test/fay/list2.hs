main = putStrLn (show (take 5 (let ns = 1 : map' (foo 123) ns in ns)))

foo x y = x * y / 2

map' f []     = []
map' f (x:xs) = f x : map' f xs

