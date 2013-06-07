data Ty1 = Int `InfixConst1` Int deriving Show

data Ty2 = Bool `InfixConst2` Bool deriving Show

data Ty3 = Ty1 :=> Ty2 deriving Show

t = (123 `InfixConst1` 123) :=> (False `InfixConst2` True)

main = print t

