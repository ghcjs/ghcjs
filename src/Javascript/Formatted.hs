module Javascript.Formatted (Formatted) where

import Javascript.Language as Js
import Javascript.Formatted.Base (Formatted)
import Javascript.Formatted.Expression ()
import Javascript.Formatted.Statement ()
import Data.Monoid (Monoid(..))
import qualified RTS.Objects as RTS

instance JavascriptCall Formatted
  where assignMethodCallResult v obj method args rest _live = mconcat
            [ assign (var v) $ nativeMethodCall obj method args
            , rest ]
        declareApplyMethodCallResult var obj args rest _live = mconcat
            [ declare [(var, nativeMethodCall obj RTS.applyMethodName args)]
            , rest ]
        declareMethodCallResult var obj method args rest _live = mconcat
            [ declare [(var, nativeMethodCall obj method args)]
            , rest ]
        callMethod obj method args rest _live = mconcat
            [ expression $ nativeMethodCall obj method args
            , rest ]
        assignFunctionCallResult v func args rest _live = mconcat
            [ assign (var v) $ nativeFunctionCall func args
            , rest ]
        declareFunctionCallResult var func args rest _live = mconcat
            [ declare [(var, nativeFunctionCall func args)]
            , rest ]
        callFunction func args rest _live = mconcat
            [ expression $ nativeFunctionCall func args
            , rest ]
        maybeAssignApplyMethodCallResult v obj rest _live = mconcat
            [ Js.declare [(v, obj)]
            , Js.if_ (RTS.isNotEvaluatedAndNotPrimitive obj) $
                assign (Js.var v) $ nativeMethodCall obj RTS.applyMethodName []
            , rest
            ]

instance JavascriptJump Formatted
  where jumpToApplyMethod obj args = Js.return $ nativeMethodCall obj RTS.applyMethodName args
	jumpToMethod obj method args = Js.return $ nativeMethodCall obj method args
	jumpToFunction func args = Js.return $ nativeFunctionCall func args
        maybeJumpToApplyMethod obj =
            Js.ifelse (RTS.isNotEvaluatedAndNotPrimitive obj)
                (Js.jumpToApplyMethod obj [])
                (Js.return obj)
        returnValue v = Js.return $ Js.nativeFunctionCall (RTS.returnData) v

instance Javascript Formatted

