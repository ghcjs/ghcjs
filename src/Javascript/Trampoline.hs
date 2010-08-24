{-# LANGUAGE TypeFamilies #-}
module Javascript.Trampoline (TailCall) where

import Javascript.Language as Js
import Data.Monoid
import Control.Arrow ((***))

newtype TailCall js = TC {runTC :: js}

instance Show js => Show (TailCall js)
  where show (TC js) = show js

instance Monoid js => Monoid (TailCall js)
  where mempty = TC mempty
        mappend (TC a) (TC b) = TC (a `mappend` b)
        mconcat = TC . mconcat . map runTC

trampoline :: Javascript js => Expression js
trampoline = var "$trampoline"

instance Javascript js => Javascript (TailCall js)
  where newtype Expression (TailCall js) = TCE { runTCE :: Expression js }
        -- declare callable object:
	function args (TC body) = TCE (function args body)

        -- returning result:
	return (TCE res) = TC $ Js.return $ new (property trampoline "Result") [res]

        -- jumping:
	jumpToMethod (TCE obj) method args =
          TC $ Js.return $ new (property trampoline "Jump") [property obj method, obj, runTCE . list $ args]

	jumpToFunction (TCE func) args =
          TC $ Js.return $ new (property trampoline "Jump") [func, Js.null, runTCE . list $ args]

        -- calling:
	assignMethodCallResult (TCE var) (TCE obj) method args =
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

        -- boilerplate:
        -- expressions
        var = TCE . var
	int = TCE . int
	float = TCE . float
	string = TCE . string
	list = TCE . list . map runTCE
	null = TCE Js.null
	true = TCE true
	false = TCE false
	not (TCE e) = TCE . Js.not $ e
	new (TCE cons) args = TCE (new cons . map runTCE $ args)
	property (TCE obj) id = TCE (property obj id)
	subscript (TCE a) (TCE i) = TCE (subscript a i)
        unsafeStringToExpression = TCE . unsafeStringToExpression

        -- statements:
	if_ (TCE test) (TC block) = TC (if_ test block)
	assign (TCE lval) (TCE val) = TC (assign lval val)
	declare id (TCE expr) = TC (declare id expr)
	switch (TCE scrut) def cases = TC (switch scrut (fmap runTC def) $ map (runTCE *** runTC) cases)

