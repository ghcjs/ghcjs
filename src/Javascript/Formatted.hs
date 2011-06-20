module Javascript.Formatted (Formatted) where

import Javascript.Language as Js
import Javascript.Formatted.Base (Formatted)
import Javascript.Formatted.Expression ()
import Javascript.Formatted.Statement ()
import Data.Monoid (Monoid(..))

instance JavascriptCall Formatted
  where assignMethodCallResult v obj method args rest = mconcat
            [ assign (var v) $ nativeMethodCall obj method args
            , rest ]
        declareMethodCallResult var obj method args rest = mconcat
            [ declare var $ nativeMethodCall obj method args
            , rest ]
        callMethod obj method args rest = mconcat
            [ expression $ nativeMethodCall obj method args
            , rest ]
        assignFunctionCallResult v func args rest = mconcat
            [ assign (var v) $ nativeFunctionCall func args
            , rest ]
        declareFunctionCallResult var func args rest = mconcat
            [ declare var $ nativeFunctionCall func args
            , rest ]
        callFunction func args rest = mconcat
            [ expression $ nativeFunctionCall func args
            , rest ]
        maybeAssignMethodCallResult pred v obj method args rest = mconcat
            [ Js.declare v obj
            , Js.if_ (pred) $
                assign (Js.var v) $ nativeMethodCall obj method args
            , rest
            ]

instance JavascriptJump Formatted
  where jumpToMethod obj method args = Js.return $ nativeMethodCall obj method args
	jumpToFunction func args = Js.return $ nativeFunctionCall func args

instance Javascript Formatted

