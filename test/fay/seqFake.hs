fakeSeq :: a -> b -> b
fakeSeq x y = y

main = error "You shall not pass!" `fakeSeq` return ()
