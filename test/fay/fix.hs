main = print (head (tail (fix (\xs -> (123::Int) : xs))))

fix f = let x = f x in x
