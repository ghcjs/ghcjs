import System.Environment (getArgs)

main = doStuff <$> getArgs <*> (read <$> getContents) >>= putStr

doStuff [key] map = case lookup key map of
    Just s -> s
    Nothing -> error $ key ++ " not present"
doStuff [] _ = error "no lookup key provided"
doStuff _ _ = error "invalid argument"