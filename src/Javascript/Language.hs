{-# LANGUAGE TypeFamilies #-}
module Javascript.Language
  ( Javascript (..)
  , Id
  , assignProperty
  , bool
  ) where

import Data.Monoid

type Id = String

class Monoid js => Javascript js
  where data Expression js
        -- expressions:
        var :: Id -> Expression js
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

        -- statements:
        assign :: Expression js -> Expression js -> js
        declare :: Id -> Expression js -> js
        if_ :: Expression js -> js -> js
        switch :: Expression js -> Maybe js -> [(Expression js, js)] -> js

        -- calling functions and methods
        -- declare callable object:
        function :: [Id] -> js -> Expression js

        -- calling:
        callMethod :: Expression js -> Id -> [Expression js] -> js
        assignMethodCallResult :: Expression js -> Expression js -> Id -> [Expression js] -> js
        declareMethodCallResult :: Id -> Expression js -> Id -> [Expression js] -> js
        callFunction :: Expression js -> [Expression js] -> js
        assignFunctionCallResult :: Expression js -> Expression js -> [Expression js] -> js
        declareFunctionCallResult :: Id -> Expression js -> [Expression js] -> js
        
        -- jumping:
        jumpToMethod :: Expression js -> Id -> [Expression js] -> js
        jumpToFunction :: Expression js -> [Expression js] -> js

        -- returning result:
        return :: Expression js -> js
        
        -- FIXME: totally hack. Far cry for removal.
        unsafeStringToExpression :: String -> Expression js

assignProperty :: Javascript js => Expression js -> Id -> Expression js -> js
assignProperty object prop value = assign (property object prop) value

bool :: Javascript js => Bool -> Expression js
bool True = true
bool False = false

