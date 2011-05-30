{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Javascript.YieldTrampoline (YieldTrampoline) where

import Javascript.Language as Js
import Data.Monoid

newtype YieldTrampoline js = YTC js
  deriving ( Monoid
           , JavascriptCallable
           , JavascriptNativeCall
           , JavascriptExpression
           , JavascriptStatement
           , Javascript
           )

instance Show js => Show (YieldTrampoline js)
  where show (YTC js) = show js

trampoline :: Javascript js => Expression js
trampoline = var "$yt"

instance JavascriptBase js => JavascriptBase (YieldTrampoline js)
  where newtype Expression (YieldTrampoline js) = YTCE { runYTCE :: Expression js }

instance Javascript js => JavascriptReturnResult (YieldTrampoline js)
  where return (YTCE res) = YTC $ expression $ yield $ new (property trampoline "Result") [res]

instance Javascript js => JavascriptJump (YieldTrampoline js)
  where jumpToMethod (YTCE obj) method args =
          YTC $ expression $ yield $ new (property trampoline "Jump") [property obj method, obj, runYTCE . list $ args]

        jumpToFunction (YTCE func) args =
          YTC $ expression $ yield $ new (property trampoline "Jump") [func, Js.null, runYTCE . list $ args]

instance Javascript js => JavascriptCall (YieldTrampoline js)
  where assignMethodCallResult (YTCE var) (YTCE obj) method args =
          YTC $ assign var $ yield $ nativeMethodCall obj method (map runYTCE args)

        declareMethodCallResult var (YTCE obj) method args =
          YTC $ declare var $ yield $ nativeMethodCall obj method (map runYTCE args)

        callMethod (YTCE obj) method args =
          YTC $ expression $ yield $ nativeMethodCall obj method (map runYTCE args)

        assignFunctionCallResult (YTCE var) (YTCE func) args =
          YTC $ assign var $ yield $ nativeFunctionCall func (map runYTCE args)

        declareFunctionCallResult var (YTCE func) args =
          YTC $ declare var $ yield $ nativeFunctionCall func (map runYTCE args)

        callFunction (YTCE func) args =
          YTC $ expression $ yield $ nativeFunctionCall func (map runYTCE args)


