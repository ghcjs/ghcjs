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
        string :: Prelude.String -> Expression js
        list :: [Expression js] -> Expression js
        null :: Expression js
        true :: Expression js
        false :: Expression js
        not :: Expression js -> Expression js
        subscript :: Expression js -> Expression js -> Expression js
        property :: Expression js -> Id -> Expression js
        new :: Expression js -> [Expression js] -> Expression js
        -- FIXME: hack:
        unsafeStringToExpression :: String -> Expression js

class JavascriptBase js => JavascriptStatement js
  where assign :: Expression js -> Expression js -> js
        declare :: Id -> Expression js -> js
        if_ :: Expression js -> js -> js
        switch :: Expression js -> Maybe js -> [(Expression js, js)] -> js

class JavascriptBase js => JavascriptCallable js
  where function :: [Id] -> js -> Expression js

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
      ) => Javascript js

assignProperty :: Javascript js => Expression js -> Id -> Expression js -> js
assignProperty object prop value = assign (property object prop) value

bool :: Javascript js => Bool -> Expression js
bool True = true
bool False = false

