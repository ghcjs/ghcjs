{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Javascript.Trampoline (Trampoline) where

import Javascript.Language as Js
import Data.Monoid
import qualified RTS.Objects as RTS

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

instance JavascriptBase js => JavascriptBase (Trampoline js)
  where newtype Expression (Trampoline js) = TCE { runTCE :: Expression js }

instance Javascript js => JavascriptReturnResult (Trampoline js)
  where return (TCE res) = TC $ expression $ nativeFunctionCall (var "$r") [res]

instance Javascript js => JavascriptJump (Trampoline js)
  where jumpToApplyMethod (TCE obj) args =
          TC $ expression $ nativeMethodCall obj "J" (map runTCE args)

        jumpToMethod (TCE obj) method args =
          TC $ expression $ nativeFunctionCall (var "$j") [property obj method, obj, runTCE . list $ args]

        jumpToFunction (TCE func) args =
          TC $ expression $ nativeFunctionCall (var "$j") [func, Js.null, runTCE . list $ args]

        maybeJumpToApplyMethod (TCE obj) =
          TC $ expression $ nativeFunctionCall (var "$A") [obj]

        returnValue args = TC $ expression $ nativeFunctionCall RTS.returnData (map runTCE args)

instance Javascript js => JavascriptCall (Trampoline js)
  where assignMethodCallResult v (TCE obj) method args (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$c")
            [property obj method, obj, runTCE . list $ args, function [v] rest]

        declareApplyMethodCallResult var (TCE obj) args (TC rest) =
          TC $ expression $ nativeMethodCall obj "C"
            [runTCE . list $ args, function [var] rest]

        declareMethodCallResult v (TCE obj) method args (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$c")
            [property obj method, obj, runTCE . list $ args, function [v] rest]

        callMethod (TCE obj) method args (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$c")
            [property obj method, obj, runTCE . list $ args, function ["_"] rest]

        assignFunctionCallResult v (TCE func) args (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$c")
            [func, Js.null, runTCE . list $ args, function [v] rest]

        declareFunctionCallResult v (TCE func) args (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$c")
            [func, Js.null, runTCE . list $ args, function [v] rest]

        callFunction (TCE func) args (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$c")
            [func, Js.null, runTCE . list $ args, function ["_"] rest]

        maybeAssignApplyMethodCallResult v (TCE obj) (TC rest) =
          TC $ expression $ nativeFunctionCall (var "$M") [obj, function [v] rest]

