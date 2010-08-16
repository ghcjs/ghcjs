{-# LANGUAGE TypeFamilies #-}
module Javascript.Language
  ( Javascript (..)
  , Id
  , assignProperty
  ) where

import Data.Monoid

type Id = String

class Monoid js => Javascript js
  where data Expression js
        var :: Id -> Expression js
        int :: (Num a) => a -> Expression js
        float :: (Fractional a) => a -> Expression js
        string :: Prelude.String -> Expression js
        list :: [Expression js] -> Expression js
        null :: Expression js
        true :: Expression js
        false :: Expression js
        bool :: Bool -> Expression js
        not :: Expression js -> Expression js
        if_ :: Expression js -> js -> js
        assignMethodCallResult :: Expression js -> Expression js -> Id -> [Expression js] -> js
        declareMethodCallResult :: Id -> Expression js -> Id -> [Expression js] -> js
        callMethod :: Expression js -> Id -> [Expression js] -> js
        assignFunctionCallResult :: Expression js -> Expression js -> [Expression js] -> js
        declareFunctionCallResult :: Id -> Expression js -> [Expression js] -> js
        callFunction :: Expression js -> [Expression js] -> js
        jumpToMethod :: Expression js -> Id -> [Expression js] -> js
        return :: Expression js -> js
        function :: [Id] -> js -> Expression js
        assign :: Expression js -> Expression js -> js
        declare :: Id -> Expression js -> js
        property :: Expression js -> Id -> Expression js
        new :: Expression js -> [Expression js] -> Expression js
        switch :: Expression js -> Maybe js -> [(Expression js, js)] -> js
        subscript :: Expression js -> Expression js -> Expression js
        
        -- FIXME: totally hack. Far cry for removal.
        unsafeStringToExpression :: String -> Expression js

assignProperty :: (Javascript js) => Expression js -> Id -> Expression js -> js
assignProperty object prop value = assign (property object prop) value

