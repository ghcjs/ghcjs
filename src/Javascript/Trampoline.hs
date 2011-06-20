{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Javascript.Trampoline (Trampoline) where

import Javascript.Language as Js
import Data.Monoid

newtype Trampoline js = TC js
  deriving ( Monoid
           , JavascriptCallable
           , JavascriptNativeCall
           , JavascriptExpression
           , JavascriptStatement
           , Javascript
           )

instance Show js => Show (Trampoline js)
  where show (TC js) = show js

trampoline :: Javascript js => Expression js
trampoline = var "$tr"

instance JavascriptBase js => JavascriptBase (Trampoline js)
  where newtype Expression (Trampoline js) = TCE { runTCE :: Expression js }

instance Javascript js => JavascriptReturnResult (Trampoline js)
  where return (TCE res) = TC $ Js.return $ new (property trampoline "Result") [res]

instance Javascript js => JavascriptJump (Trampoline js)
  where jumpToMethod (TCE obj) method args =
          TC $ Js.return $ new (property trampoline "Jump") [property obj method, obj, runTCE . list $ args]

        jumpToFunction (TCE func) args =
          TC $ Js.return $ new (property trampoline "Jump") [func, Js.null, runTCE . list $ args]

instance Javascript js => JavascriptCall (Trampoline js)
  where assignMethodCallResult var (TCE obj) method args (TC rest) =
          TC $ Js.return $ new (property trampoline "Call")
            [property obj method, obj, runTCE . list $ args, function [var] rest]

        declareMethodCallResult var (TCE obj) method args (TC rest) =
          TC $ Js.return $ new (property trampoline "Call")
            [property obj method, obj, runTCE . list $ args, function [var] rest]

        callMethod (TCE obj) method args (TC rest) =
          TC $ Js.return $ new (property trampoline "Call")
            [property obj method, obj, runTCE . list $ args, function ["_x"] rest]

        assignFunctionCallResult var (TCE func) args (TC rest) =
          TC $ Js.return $ new (property trampoline "Call")
            [func, Js.null, runTCE . list $ args, function [var] rest]

        declareFunctionCallResult var (TCE func) args (TC rest) =
          TC $ Js.return $ new (property trampoline "Call")
            [func, Js.null, runTCE . list $ args, function [var] rest]

        callFunction (TCE func) args (TC rest) =
          TC $ Js.return $ new (property trampoline "Call")
            [func, Js.null, runTCE . list $ args, function ["_x"] rest]

        maybeAssignMethodCallResult (TCE pred) var (TCE obj) method args (TC rest) =
          TC $ mconcat
            [ Js.declare "_f" (function [var] rest)
            , Js.if_ (pred) $
                Js.return $ new (property trampoline "Call")
                  [property obj method, obj, runTCE . list $ args, Js.var "_f"]
            , Js.return $ nativeFunctionCall (Js.var "_f") [Js.var var] 
            ]

