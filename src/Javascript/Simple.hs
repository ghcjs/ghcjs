{-# LANGUAGE TypeFamilies #-}
module Javascript.Simple (Simple) where

import Javascript.Language
import Data.Monoid
import Data.List

newtype Simple = P { unP :: Prelude.String }

instance Show Simple
  where show = unP

instance Monoid Simple
  where mempty = P ""
        mappend (P a) (P b) = P (a ++ b)
        mconcat = P . concat . map unP

instance Javascript Simple
  where data Expression Simple = E { unE :: Prelude.String }
        -- expressions:
        var = E
	int = E . show
	float = E . show
	string = E . show
	list xs = E $ concat ["[", intercalate ", " . map unE $ xs, "]"]
	null = E "null"
	true = E "true"
	false = E "false"
	not e = E $ "!" ++ unE e
	property obj id = E $ concat [unE obj, ".", id]
	new conctructor args = E $ concat["new ", unE conctructor, "(", intercalate ", " . map unE $ args, ")"]
	subscript a i = E $ concat [unE a, "[", unE i, "]"]

        unsafeStringToExpression = E

        -- statements:
	assign lval val = P $ concat [unE lval, " = ", unE val, ";\n"]
	declare id expr = P $ concat ["var ", id, " = ", unE expr, ";\n"]
	if_ test block =
	  P $ concat
	    [ "if (", unE test, ") {\n"
	    , unP block
	    , "}\n"
	    ]
	switch scrut def cases = P $ concat ["switch (", unE scrut, ") {\n", casesP, defP, "}\n"]
	  where defP =
		  case def
		  of Nothing -> ""
		     Just (P prog) -> "default:\n" ++ prog
		casesP :: Prelude.String
		casesP = concat . map (uncurry caseP) $ cases
		caseP :: Expression Simple -> Simple -> Prelude.String
		caseP (E expr) (P prog) = concat ["case ", expr, ":\n", prog]

        -- declare callable object
	function args body = E $ concat ["function (", intercalate ", " args, ") {\n", unP body, "}"]

        -- jumping:
	jumpToMethod obj method args = Javascript.Language.return $ callMethodPrimitive obj method args
	jumpToFunction func args = Javascript.Language.return $ callFunctionPrimitive func args

        -- calling:
	assignMethodCallResult var obj method args = assign var $ callMethodPrimitive obj method args
	declareMethodCallResult var obj method args = declare var $ callMethodPrimitive obj method args
	callMethod obj method args = P $ concat [unE $ callMethodPrimitive obj method args, ";\n"]
	assignFunctionCallResult var func args = assign var $ callFunctionPrimitive func args
	declareFunctionCallResult var func args = declare var $ callFunctionPrimitive func args
	callFunction func args = P $ concat [unE $ callFunctionPrimitive func args, ";\n"]

        -- returning result:
	return res = P $ concat ["return ", unE res, ";\n"]

callMethodPrimitive :: Expression Simple -> Id -> [Expression Simple] -> Expression Simple
callMethodPrimitive obj method args = E $ concat [unE obj, ".", method, "(", intercalate ", " . map unE $ args, ")"]

callFunctionPrimitive :: Expression Simple -> [Expression Simple] -> Expression Simple
callFunctionPrimitive func args = E $ concat [unE func, "(", intercalate ", " . map unE $ args, ")"]

