{-# LANGUAGE TypeFamilies #-}
module Javascript.Language where

import Data.Monoid

type Id = String

class JavascriptBase js
  where data Expression js

class JavascriptBase js => JavascriptExpression js
  where var :: Id -> Expression js
        int :: (Num a, Ord a, Show a) => a -> Expression js
        float :: (Fractional a, Ord a, Show a) => a -> Expression js
        string :: String -> Expression js
        list :: [Expression js] -> Expression js
        object :: [(Id, Expression js)] -> Expression js
        subscript :: Expression js -> Expression js -> Expression js
        property :: Expression js -> Id -> Expression js
        new :: Expression js -> [Expression js] -> Expression js
        ternary :: Expression js -> Expression js -> Expression js -> Expression js
        assignment :: Expression js -> Expression js -> Expression js
        not :: Expression js -> Expression js
        and :: Expression js -> Expression js -> Expression js
        or :: Expression js -> Expression js -> Expression js
        plus :: Expression js -> Expression js -> Expression js
        minus :: Expression js -> Expression js -> Expression js
        divide :: Expression js -> Expression js -> Expression js
        mod :: Expression js -> Expression js -> Expression js
        multiply :: Expression js -> Expression js -> Expression js
        unaryMinus :: Expression js -> Expression js
        equal :: Expression js -> Expression js -> Expression js
        notEqual :: Expression js -> Expression js -> Expression js
        less :: Expression js -> Expression js -> Expression js
        lessOrEqual :: Expression js -> Expression js -> Expression js
        greater :: Expression js -> Expression js -> Expression js
        greaterOrEqual :: Expression js -> Expression js -> Expression js
        shiftLL :: Expression js -> Expression js -> Expression js
        shiftRL :: Expression js -> Expression js -> Expression js
        shiftRA :: Expression js -> Expression js -> Expression js
        bitAnd :: Expression js -> Expression js -> Expression js
        bitXOr :: Expression js -> Expression js -> Expression js
        bitOr :: Expression js -> Expression js -> Expression js
        bitNot :: Expression js -> Expression js

class JavascriptBase js => JavascriptStatement js
  where declare :: [(Id, Expression js)] -> js
        ifthenelse :: Expression js -> js -> Maybe js-> js
        switch :: Expression js -> Maybe js -> [(Expression js, js)] -> js
        throw :: Expression js -> js
        expression :: Expression js -> js
        comment :: String -> js

class JavascriptBase js => JavascriptCallable js
  where function :: [Id] -> js -> Expression js

class JavascriptBase js => JavascriptNativeCall js
  where nativeFunctionCall :: Expression js -> [Expression js] -> Expression js
        nativeMethodCall :: Expression js -> Id -> [Expression js] -> Expression js

class JavascriptBase js => JavascriptCall js
  where callMethod :: Expression js -> Id -> [Expression js] -> js -> Expression js -> js
        assignMethodCallResult :: Id -> Expression js -> Id -> [Expression js] -> js -> Expression js -> js
        declareApplyMethodCallResult :: Id -> Expression js -> [Expression js] -> js -> Expression js -> js
        declareMethodCallResult :: Id -> Expression js -> Id -> [Expression js] -> js -> Expression js -> js
        callFunction :: Expression js -> [Expression js] -> js -> Expression js -> js
        assignFunctionCallResult :: Id -> Expression js -> [Expression js] -> js -> Expression js -> js
        declareFunctionCallResult :: Id -> Expression js -> [Expression js] -> js -> Expression js -> js
        maybeAssignApplyMethodCallResult :: Id -> Expression js -> js -> Expression js -> js

class JavascriptBase js => JavascriptJump js
  where jumpToApplyMethod :: Expression js -> [Expression js] -> js
        jumpToMethod :: Expression js -> Id -> [Expression js] -> js
        jumpToFunction :: Expression js -> [Expression js] -> js
        maybeJumpToApplyMethod :: Expression js -> js
        returnValue :: Int -> [Expression js] -> Expression js -> js

class JavascriptBase js => JavascriptReturnResult js
  where return :: Expression js -> js

class ( Monoid js
      , JavascriptExpression js
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
assign a b = expression (assignment a b)

null :: Javascript js => Expression js
null = var "null"

true :: Javascript js => Expression js
true = var "true"

false :: Javascript js => Expression js
false = var "false"

this :: Javascript js => Expression js
this = var "this"

if_ :: Javascript js => Expression js -> js -> js
if_ cond action = ifthenelse cond action Nothing

ifelse :: Javascript js => Expression js -> js -> js -> js
ifelse cond action alterAction = ifthenelse cond action (Just alterAction)

