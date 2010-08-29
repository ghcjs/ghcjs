module Javascript.Formatted (Formatted) where

import Javascript.Language as Js
import Javascript.Formatted.Base (Formatted)
import Javascript.Formatted.Expression ()
import Javascript.Formatted.Statement ()

instance JavascriptCall Formatted
  where assignMethodCallResult var obj method args = assign var $ nativeMethodCall obj method args
	declareMethodCallResult var obj method args = declare var $ nativeMethodCall obj method args
	callMethod obj method args = expression $ nativeMethodCall obj method args
	assignFunctionCallResult var func args = assign var $ nativeFunctionCall func args
	declareFunctionCallResult var func args = declare var $ nativeFunctionCall func args
	callFunction func args = expression $ nativeFunctionCall func args

instance JavascriptJump Formatted
  where jumpToMethod obj method args = Js.return $ nativeMethodCall obj method args
	jumpToFunction func args = Js.return $ nativeFunctionCall func args

instance Javascript Formatted

