-- | Define operations to build javascript expressions
--   use precedence table from
--   https://developer.mozilla.org/en/JavaScript/Reference/Operators/Operator_Precedence
module Javascript.Formatted.Expression where

import Data.Char (ord)
import Data.List (intersperse, intercalate)
import Control.Monad (when)

import Javascript.Language hiding (null)
import Javascript.Formatted.Base

leftBinOp :: String -> Precedence -> Expression Formatted -> Expression Formatted -> Expression Formatted
leftBinOp op p a b = mkOperation p $
  do tellWithPrecedenceConstraint a p
     tell op
     tellWithPrecedenceConstraint b (p - 1)

rightBinOp :: String -> Precedence -> Expression Formatted -> Expression Formatted -> Expression Formatted
rightBinOp op p a b = mkOperation p $
  do tellWithPrecedenceConstraint a (p - 1)
     tell op
     tellWithPrecedenceConstraint b p

leftUnaryOp :: String -> Precedence -> Expression Formatted -> Expression Formatted
leftUnaryOp op p a = mkOperation p $
  do tell op
     tellWithPrecedenceConstraint a p

-- Avoid the risk of getting "a--1" when we want "a- -1"
showJsInt :: (Integral a) => a -> String
showJsInt n
    | n < 0     = " -" ++ showJsInt (negate n)
    | otherwise = show . toInteger $ n

-- Avoid the risk of getting "a--1" when we want "a- -1"
showJsFrac :: (RealFrac a) => a -> String
showJsFrac f
    | f < 0     =  " -" ++ showJsFrac (negate f)
    | otherwise =  show df -- hopefully javascript always understands the output of this
    where
      df :: Double
      df = realToFrac f

instance JavascriptExpression Formatted
  where var = mkOperation 0 . tell
	int = mkOperation 0 . tell . showJsInt
	float = mkOperation 0 . tell . showJsFrac
	string = mkOperation 0 . tell . showJsString
	list xs = mkOperation 0 $
          do tell "["
             sequence_ . intersperse (tell ",") . map tellUnconstraint $ xs
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
             sequence_ . intersperse (tell ",") . map tellUnconstraint $ args
             tell ")"
	subscript a i = mkOperation 1 $
          do tellWithPrecedenceConstraint a 1
             tell "["
             tellUnconstraint i
             tell "]"
        ternary test t f = mkOperation 15 $
          do tellWithPrecedenceConstraint test 15
             tell "?"
             tellWithPrecedenceConstraint t 15
             tell ":"
             tellWithPrecedenceConstraint f 15
        assignment a b =     rightBinOp  "="  16 a b
        or a b =             leftBinOp   "||" 14 a b
        and a b =            leftBinOp   "&&" 13 a b
        bitOr a b =          leftBinOp   "|"  12 a b
        bitXOr a b =         leftBinOp   "^"  11 a b
        bitAnd a b =         leftBinOp   "&"  10 a b
        equal a b =          leftBinOp   "=="  9 a b
        notEqual a b =       leftBinOp   "!="  9 a b
        less a b =           leftBinOp   "<"   8 a b
        lessOrEqual a b =    leftBinOp   "<="  8 a b
        greater a b =        leftBinOp   ">"   8 a b
        greaterOrEqual a b = leftBinOp   ">="  8 a b
        shiftLL a b =        leftBinOp   "<<"  7 a b
        shiftRA a b =        leftBinOp   ">>"  7 a b
        shiftRL a b =        leftBinOp   ">>>" 7 a b
        plus a b =           leftBinOp   "+"   6 a b
        minus a b =          leftBinOp   "-"   6 a b
        multiply a b =       leftBinOp   "*"   5 a b
        divide a b =         leftBinOp   "/"   5 a b
        mod a b =            leftBinOp   "%"   5 a b
        not =                leftUnaryOp " !"   4
        bitNot =             leftUnaryOp " ~"   4
        unaryMinus =         leftUnaryOp " -"   4

instance JavascriptNativeCall Formatted
  where nativeFunctionCall func args = mkOperation 2 $
          do tellWithPrecedenceConstraint func 2
             tell "("
             sequence_ . intersperse (tell ",") . map tellUnconstraint $ args
             tell ")"
        nativeMethodCall obj method args = mkOperation 2 $
          do tellWithPrecedenceConstraint obj 2
             tell "."
             tell method
             tell "("
             sequence_ . intersperse (tell ",") . map tellUnconstraint $ args
             tell ")"

instance JavascriptCallable Formatted
  where function args body = mkOperation 0 $
          do tell $ concat ["function(", intercalate "," args, "){"]
             indent $ unP body
             newLine
             tell "}"

showJsString :: String -> String
showJsString xs = concat ["\"", concat . map lit $ xs, "\""]
  where lit '"'  = "\\\""
        lit '\\' = "\\\\"
        lit '\b' = "\\b"
        lit '\f' = "\\f"
        lit '\n' = "\\n"
        lit '\r' = "\\r"
        lit '\t' = "\\t"
        lit '\v' = "\\v"
        lit c
          | ' ' <= c && c < '\x7F' = [c]
          | c <= '\xFF'            = "\\x" ++ hex 2 (ord c)
          | c <= '\xFFFF'          = "\\u" ++ hex 4 (ord c)
          | otherwise              = "\\uffff" -- Javascript supports only UTF-16 characters
          where hex n = reverse . take n . map ((d!!).snd) . iterate (f . fst) . f
                d = ['0'..'9'] ++ ['a'..'f']
                f = flip divMod 16

