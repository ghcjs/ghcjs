-- | Define operations to build javascript expressions
--   use precedence table from
--   http://www.codehouse.com/javascript/precedence/
module Javascript.Formatted.Expression where

import Data.List (intersperse, intercalate)
import Control.Monad (when)
import Control.Monad.Writer (tell)

import Javascript.Language hiding (null)
import Javascript.Formatted.Base

leftBinOp :: String -> Precedence -> Expression Formatted -> Expression Formatted -> Expression Formatted
leftBinOp op p a b = mkOperation p $
  do tellWithPrecedenceConstraint a p
     tell " "
     tell op
     tell " "
     tellWithPrecedenceConstraint b (p - 1)

rightBinOp :: String -> Precedence -> Expression Formatted -> Expression Formatted -> Expression Formatted
rightBinOp op p a b = mkOperation p $
  do tellWithPrecedenceConstraint a (p - 1)
     tell " "
     tell op
     tell " "
     tellWithPrecedenceConstraint b p

leftUnaryOp :: String -> Precedence -> Expression Formatted -> Expression Formatted
leftUnaryOp op p a = mkOperation p $
  do tell op
     tellWithPrecedenceConstraint a p

instance JavascriptExpression Formatted
  where var = mkOperation 0 . tell
	int = mkOperation 0 . tell . show
	float = mkOperation 0 . tell . show
	string = mkOperation 0 . tell . show
	list xs = mkOperation 0 $
          do tell "["
             sequence_ . intersperse (tell ", ") . map tellUnconstraint $ xs
             tell "]"
        object xs = mkOperation 0 $
          do tell "{"
             when (Prelude.not . null $ xs) $
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
                     tellUnconstraint v
	property obj id = mkOperation 1 $
          do tellWithPrecedenceConstraint obj 1
             tell "."
             tell id
	new conctructor args = mkOperation 1 $
          do tell "new "
             tellWithPrecedenceConstraint conctructor 1
             tell "("
             sequence_ . intersperse (tell ", ") . map tellUnconstraint $ args
             tell ")"
	subscript a i = mkOperation 1 $
          do tellWithPrecedenceConstraint a 1
             tell "["
             tellUnconstraint i 
             tell "]"
        ternary test t f = mkOperation 15 $
          do tellWithPrecedenceConstraint test 15
             tell " ? "
             tellWithPrecedenceConstraint t 15
             tell " : "
             tellWithPrecedenceConstraint f 15
        assignment a b =     rightBinOp  "=" 16 a b
        equal a b =          leftBinOp   "==" 9 a b
        notEqual a b =       leftBinOp   "!=" 9 a b
        less a b =           leftBinOp   "<"  8 a b
        lessOrEqual a b =    leftBinOp   "<=" 8 a b
        greater a b =        leftBinOp   ">"  8 a b
        greaterOrEqual a b = leftBinOp   ">=" 8 a b
        plus a b =           leftBinOp   "+"  6 a b
        minus a b =          leftBinOp   "-"  6 a b
        multiply a b =       leftBinOp   "*"  5 a b
        divide a b =         leftBinOp   "/"  5 a b
        not =                leftUnaryOp "!"  4
        unaryMinus a =       leftUnaryOp "-"  4 a

instance JavascriptNativeCall Formatted
  where nativeFunctionCall func args = mkOperation 2 $
          do tellWithPrecedenceConstraint func 2
             tell "("
             sequence_ . intersperse (tell ", ") . map tellUnconstraint $ args
             tell ")"
        nativeMethodCall obj method args = mkOperation 2 $
          do tellWithPrecedenceConstraint obj 2
             tell "."
             tell method
             tell "("
             sequence_ . intersperse (tell ", ") . map tellUnconstraint $ args
             tell ")"

instance JavascriptCallable Formatted
  where function args body = mkOperation 0 $
          do tell $ concat ["function (", intercalate ", " args, ") {"]
             indent $ unP body
             newLine
             tell "}"

