main = print testFn

testFn = let f a b = snd (a/b,10) in f 1 0 -- undefined undefined
