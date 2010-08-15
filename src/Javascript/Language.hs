module Javascript.Language
  ( Id
  , Expression
  , Program
  , int
  , float
  , string
  , list
  , bool
  , true
  , false
  , Javascript.Language.not
  , Javascript.Language.null
  , if_
  , jumpToMethod
  , Javascript.Language.return
  , assignMethodCallResult
  , declareMethodCallResult
  , assignFunctionCallResult
  , declareFunctionCallResult
  , function
  , assign
  , assignProperty
  , property
  , var
  , switch
  , declare
  , Javascript.Language.sequence
  , new
  , subscript
  , unsafeStringToExpression
  ) where

import Data.List (intercalate)

newtype Expression = E { unE :: Prelude.String }
newtype Program = P { unP :: Prelude.String }
type Id = Prelude.String

instance Show Program where
  show = unP

var :: Id -> Expression
var = E

int :: (Num a) => a -> Expression
int = E . show

float :: (Fractional a) => a -> Expression
float = E . show

string :: Prelude.String -> Expression
string = E . show

list :: [Expression] -> Expression
list xs = E $ concat ["[", intercalate ", " . map unE $ xs, "]"]

null :: Expression
null = E "null"

true :: Expression
true = E "true"

false :: Expression
false = E "false"

bool :: Bool -> Expression
bool True = true
bool False = false

not :: Expression -> Expression
not e = E $ "!" ++ unE e

if_ :: Expression -> Program -> Program
if_ test block =
  P $ concat
    [ "if (", unE test, ") {\n"
    , unP block
    , "}\n"
    ]

callMethodPrimitive :: Expression -> Id -> [Expression] -> Expression
callMethodPrimitive obj method args = E $ concat [unE obj, ".", method, "(", intercalate ", " . map unE $ args, ")"]

assignMethodCallResult :: Expression -> Expression -> Id -> [Expression] -> Program
assignMethodCallResult var obj method args = assign var $ callMethodPrimitive obj method args

declareMethodCallResult :: Id -> Expression -> Id -> [Expression] -> Program
declareMethodCallResult var obj method args = declare var $ callMethodPrimitive obj method args

callFunctionPrimitive :: Expression -> [Expression] -> Expression
callFunctionPrimitive func args = E $ concat [unE func, "(", intercalate ", " . map unE $ args, ")"]

assignFunctionCallResult :: Expression -> Expression -> [Expression] -> Program
assignFunctionCallResult var func args = assign var $ callFunctionPrimitive func args

declareFunctionCallResult :: Id -> Expression -> [Expression] -> Program
declareFunctionCallResult var func args = declare var $ callFunctionPrimitive func args

jumpToMethod :: Expression -> Id -> [Expression] -> Program
jumpToMethod obj method args = Javascript.Language.return $ callMethodPrimitive obj method args

return :: Expression -> Program
return res = P $ concat ["return ", unE res, ";\n"]

function :: [Id] -> Program -> Expression
function args body = E $ concat ["function (", intercalate ", " args, ") {\n", unP body, "}"]

assign :: Expression -> Expression -> Program
assign lval val = P $ concat [unE lval, " = ", unE val, ";\n"]

assignProperty :: Expression -> Id -> Expression -> Program
assignProperty object prop value = assign (property object prop) value

declare :: Id -> Expression -> Program
declare id expr = P $ concat ["var ", id, " = ", unE expr, ";\n"]

property :: Expression -> Id -> Expression
property obj id = E $ concat [unE obj, ".", id]

new :: Expression -> [Expression] -> Expression
new conctructor args = E $ concat["new ", unE conctructor, "(", intercalate ", " . map unE $ args, ")"]

switch :: Expression -> (Maybe Program) -> [(Expression, Program)] -> Program
switch scrut def cases = P $ concat ["switch (", unE scrut, ") {\n", casesP, defP, "}\n"]
  where defP =
          case def
          of Nothing -> ""
             Just (P prog) -> "default:\n" ++ prog
        casesP :: Prelude.String
        casesP = concat . map caseP $ cases
        caseP :: (Expression, Program) -> Prelude.String
        caseP (E expr, P prog) = concat ["case ", expr, ":\n", prog]

sequence :: [Program] -> Program
sequence = P . concat . map unP

subscript :: Expression -> Expression -> Expression
subscript a i = E $ concat [unE a, "[", unE i, "]"]

unsafeStringToExpression :: String -> Expression
unsafeStringToExpression = E
