{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Javascript.Formatted (Formatted) where

import Javascript.Language
import Data.Monoid
import Data.List

import Control.Monad.Reader
import Control.Monad.Writer

type Identation = Int
newtype Formatted = P { unP :: ReaderT Identation (Writer String) () }

indent :: (MonadReader m, MonadWriter m, EnvType m ~ Int, WriterType m ~ String) => m a -> m a
indent = local (+4)

newLine :: (MonadReader m, MonadWriter m, EnvType m ~ Int, WriterType m ~ String) => m ()
newLine =
  do tell "\n"
     n <- ask
     tell $ replicate n ' '

instance Show Formatted
  where show = execWriter . flip runReaderT 0 . unP

instance Monoid Formatted
  where mempty = P (Prelude.return ())
        mappend (P a) (P b) = P (a >> b)
        mconcat = P . sequence_ . map unP

instance JavascriptBase Formatted
  where data Expression Formatted = E { unE :: ReaderT Identation (Writer String) () }

instance JavascriptExpression Formatted
  where var v = E $ tell v
	int i = E $ tell (show i)
	float f = E $ tell (show f)
	string s = E $ tell (show s)
	list xs = E $
          do tell "["
             sequence_ . intersperse (tell ", ") . map unE $ xs
             tell "]"
	null = E $ tell "null"
	true = E $ tell "true"
	false = E $ tell "false"
	not e = E $ 
          do tell "!("
             unE e
             tell ")"
	property obj id = E $
          do unE obj
             tell "."
             tell id
	new conctructor args = E $
          do tell "new "
             unE conctructor
             tell "("
             sequence_ . intersperse (tell ", ") . map unE $ args
             tell ")"
	subscript a i = E $
          do unE a
             tell "["
             unE i
             tell "]"

        unsafeStringToExpression = E . tell

instance JavascriptStatement Formatted
  where assign lval val = P $
          do newLine
             unE lval
             tell " = "
             unE val
             tell ";"
	declare id expr = P $
          do newLine
             tell $ concat ["var ", id, " = "]
             unE expr
             tell ";"
	if_ test block = P $
	    do newLine
               tell "if ("
               unE test
               tell ") {"
	       indent $ unP block
               newLine
	       tell "}"
	switch scrut def cases = P $
          do newLine
             tell "switch ("
             unE scrut
             tell ") {"
             unP casesP
             defP
             newLine
             tell "}"
	  where defP =
		  case def
		  of Nothing -> tell ""
		     Just (P prog) ->
                       do newLine
                          tell "default:"
                          indent prog
		casesP :: Formatted
		casesP = P . sequence_ . map (unP . uncurry caseP) $ cases
		caseP :: Expression Formatted -> Formatted -> Formatted
		caseP (E expr) (P prog) = P $
                  do newLine
                     tell "case "
                     expr
                     tell ":"
                     indent prog

instance JavascriptCall Formatted
  where assignMethodCallResult var obj method args = assign var $ callMethodPrimitive obj method args
	declareMethodCallResult var obj method args = declare var $ callMethodPrimitive obj method args
	callMethod obj method args = P $
          do newLine
             unE $ callMethodPrimitive obj method args
             tell ";"
	assignFunctionCallResult var func args = assign var $ callFunctionPrimitive func args
	declareFunctionCallResult var func args = declare var $ callFunctionPrimitive func args
	callFunction func args = P $
          do newLine
             unE $ callFunctionPrimitive func args
             tell ";"

instance JavascriptJump Formatted
  where jumpToMethod obj method args = Javascript.Language.return $ callMethodPrimitive obj method args
	jumpToFunction func args = Javascript.Language.return $ callFunctionPrimitive func args

instance JavascriptReturnResult Formatted
  where return res = P $
          do newLine
             tell "return "
             unE res
             tell ";"

instance JavascriptCallable Formatted
  where function args body = E $
          do tell $ concat ["function (", intercalate ", " args, ") {"]
             indent $ unP body
             newLine
             tell "}"

instance Javascript Formatted

callMethodPrimitive :: Expression Formatted -> Id -> [Expression Formatted] -> Expression Formatted
callMethodPrimitive obj method args = E $
  do unE obj
     tell "."
     tell method
     tell "("
     sequence_ . intersperse (tell ", ") . map unE $ args
     tell ")"

callFunctionPrimitive :: Expression Formatted -> [Expression Formatted] -> Expression Formatted
callFunctionPrimitive func args = E $
  do unE func
     tell "("
     sequence_ . intersperse (tell ", ") . map unE $ args
     tell ")"

