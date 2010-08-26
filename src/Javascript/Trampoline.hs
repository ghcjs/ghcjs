{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Javascript.Trampoline (Trampoline) where

import Javascript.Language as Js
import Data.Monoid

newtype Trampoline js = TC js
  deriving ( Monoid
           , JavascriptCallable
           , JavascriptExpression
           , JavascriptStatement
           , Javascript
           )

instance Show js => Show (Trampoline js)
  where show (TC js) = show js

trampoline :: Javascript js => Expression js
trampoline = var "$trampoline"

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
  where assignMethodCallResult (TCE var) (TCE obj) method args =
          TC $ assignMethodCallResult var trampoline "trcall" [property obj method, obj, runTCE . list $ args]

	declareMethodCallResult var (TCE obj) method args =
          TC $ declareMethodCallResult var trampoline "trcall" [property obj method, obj, runTCE . list $ args]

	callMethod (TCE obj) method args =
          TC $ callMethod trampoline "trcall" [property obj method, obj, runTCE . list $ args]

	assignFunctionCallResult (TCE var) (TCE func) args =
          TC $ assignMethodCallResult var trampoline "trcall" [func, Js.null, runTCE . list $ args]

	declareFunctionCallResult var (TCE func) args =
          TC $ declareMethodCallResult var trampoline "trcall" [func, Js.null, runTCE . list $ args]

	callFunction (TCE func) args =
          TC $ callMethod trampoline "trcall" [func, Js.null, runTCE . list $ args]


