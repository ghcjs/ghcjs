import GHC.Stack

f p1 p2 p3 = p1 + p2 + p3

forceIt f' r = do
    print r -- when this line is removed GHCJS compiled program produces same output
    print =<< whoCreated r
    print =<< whoCreated (f 1 2 3)
    print =<< whoCreated (f' 3)

main = do
    print =<< whoCreated f
    let f' = f 1 2
    print =<< whoCreated f'
    let r = f' 3
    forceIt f' r
