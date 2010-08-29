{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Javascript.Formatted (Formatted) where

import Javascript.Language as Js hiding (not, null, return)
import qualified Javascript.Language as Js (return)
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
  where newtype Expression Formatted = E { unE :: ReaderT Identation (Writer String) () }

instance JavascriptExpression Formatted
  where var v = E $ tell v
	int i = E $ tell (show i)
	float f = E $ tell (show f)
	string s = E $ tell (show s)
	list xs = E $
          do tell "["
             sequence_ . intersperse (tell ", ") . map unE $ xs
             tell "]"
        object xs = E $
          do tell "{"
             when (not . null $ xs) $
               indent $
                 do flip mapM_ (init xs) $ \i ->
                      do uncurry prop i
                         tell ","
                    uncurry prop (last xs)
             tell "}"
          where prop p v =
                  do newLine
                     tell p
                     tell ": "
                     unE v
	property obj id = E $
          do tell "("
             unE obj
             tell ")."
             tell id
	new conctructor args = E $
          do tell "new ("
             unE conctructor
             tell ")("
             sequence_ . intersperse (tell ", ") . map unE $ args
             tell ")"
	subscript a i = E $
          do tell "("
             unE a
             tell ")["
             unE i
             tell "]"
        binOp op a b = E $
          do tell "("
             unE a
             tell ") "
             tell op
             tell " ("
             unE b
             tell ")"
        leftUnaryOp op a = E $
          do tell op
             tell "("
             unE a
             tell ")"
        rightUnaryOp op a = E $
          do tell "("
             unE a
             tell ")"
             tell op
        ternary test t f = E $
          do tell "("
             unE test
             tell ") ? ("
             unE t
             tell ") : ("
             unE f
             tell ")"

instance JavascriptStatement Formatted
  where expression expr = P $
          do newLine
             unE expr
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
        throw e = P $
          do newLine
             tell "throw "
             unE e
             tell ";"

instance JavascriptNativeCall Formatted
  where nativeFunctionCall func args = E $
          do tell "("
             unE func
             tell ")("
             sequence_ . intersperse (tell ", ") . map unE $ args
             tell ")"
        nativeMethodCall obj method args = E $
          do tell "("
             unE obj
             tell ")."
             tell method
             tell "("
             sequence_ . intersperse (tell ", ") . map unE $ args
             tell ")"

instance JavascriptCall Formatted
  where assignMethodCallResult var obj method args = assign var $ nativeMethodCall obj method args
	declareMethodCallResult var obj method args = declare var $ nativeMethodCall obj method args
	callMethod obj method args = P $
          do newLine
             unE $ nativeMethodCall obj method args
             tell ";"
	assignFunctionCallResult var func args = assign var $ nativeFunctionCall func args
	declareFunctionCallResult var func args = declare var $ nativeFunctionCall func args
	callFunction func args = P $
          do newLine
             unE $ nativeFunctionCall func args
             tell ";"

instance JavascriptJump Formatted
  where jumpToMethod obj method args = Js.return $ nativeMethodCall obj method args
	jumpToFunction func args = Js.return $ nativeFunctionCall func args

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

