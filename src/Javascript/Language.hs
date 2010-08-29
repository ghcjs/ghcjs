{-# LANGUAGE TypeFamilies #-}
module Javascript.Language where

import Data.Monoid

type Id = String

class Monoid js => JavascriptBase js
  where data Expression js

class JavascriptBase js => JavascriptExpression js
  where var :: Id -> Expression js
        int :: (Num a) => a -> Expression js
        float :: (Fractional a) => a -> Expression js
        string :: String -> Expression js
        list :: [Expression js] -> Expression js
        object :: [(Id, Expression js)] -> Expression js
        subscript :: Expression js -> Expression js -> Expression js
        property :: Expression js -> Id -> Expression js
        new :: Expression js -> [Expression js] -> Expression js
        binOp :: String -> Expression js -> Expression js -> Expression js
        leftUnaryOp :: String -> Expression js -> Expression js
        rightUnaryOp :: String -> Expression js -> Expression js
        ternary :: Expression js -> Expression js -> Expression js -> Expression js

class JavascriptBase js => JavascriptStatement js
  where declare :: Id -> Expression js -> js
        if_ :: Expression js -> js -> js
        switch :: Expression js -> Maybe js -> [(Expression js, js)] -> js
        throw :: Expression js -> js
        expression :: Expression js -> js

class JavascriptBase js => JavascriptCallable js
  where function :: [Id] -> js -> Expression js

class JavascriptBase js => JavascriptNativeCall js
  where nativeFunctionCall :: Expression js -> [Expression js] -> Expression js
        nativeMethodCall :: Expression js -> Id -> [Expression js] -> Expression js

class JavascriptBase js => JavascriptCall js
  where callMethod :: Expression js -> Id -> [Expression js] -> js
        assignMethodCallResult :: Expression js -> Expression js -> Id -> [Expression js] -> js
        declareMethodCallResult :: Id -> Expression js -> Id -> [Expression js] -> js
        callFunction :: Expression js -> [Expression js] -> js
        assignFunctionCallResult :: Expression js -> Expression js -> [Expression js] -> js
        declareFunctionCallResult :: Id -> Expression js -> [Expression js] -> js
        
class JavascriptBase js => JavascriptJump js
  where jumpToMethod :: Expression js -> Id -> [Expression js] -> js
        jumpToFunction :: Expression js -> [Expression js] -> js

class JavascriptBase js => JavascriptReturnResult js
  where return :: Expression js -> js

class ( JavascriptExpression js
      , JavascriptStatement js
      , JavascriptCallable js
      , JavascriptCall js
      , JavascriptJump js
      , JavascriptReturnResult js
      , JavascriptNativeCall js
      ) => Javascript js

assignProperty :: Javascript js => Expression js -> Id -> Expression js -> js
assignProperty object prop value = assign (property object prop) value

bool :: Javascript js => Bool -> Expression js
bool True = true
bool False = false

assign :: Javascript js => Expression js -> Expression js -> js
assign a b = expression (binOp "=" a b)

not :: Javascript js => Expression js -> Expression js
not = leftUnaryOp "!"

null :: Javascript js => Expression js
null = var "null"

true :: Javascript js => Expression js
true = var "true"

false :: Javascript js => Expression js
false = var "false"

this :: Javascript js => Expression js
this = var "this"

