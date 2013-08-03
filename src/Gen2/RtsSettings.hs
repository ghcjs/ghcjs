{-# LANGUAGE QuasiQuotes #-}
{-
   some settings to control debugging output of the rts
   -- fixme, make cabal flags, or runtime flags?
-}
module Gen2.RtsSettings where

import           Data.Monoid
import           Gen2.Utils
import           Language.Javascript.JMacro
import           Data.Text (Text)

-- debugging settings (fixme, make these depend on cabal flags?)

-- trace/debug messages from the garbage collector
gcDebug :: Bool
gcDebug = True

-- timing measurements for the garbage collector
gcTiming :: Bool
gcTiming = True

-- extra checks from the gc for consistency, usually shouldn't print, but make it a little slower
gcChecks :: Bool
gcChecks = True

-- extra rts assertions/checks
rtsChecks :: Bool
rtsChecks = rtsDebug

-- rts tracing/debugging
rtsDebug :: Bool
rtsDebug = False

-- Trace calls from the trampoline
rtsTraceCalls :: Bool
rtsTraceCalls = rtsDebug -- False -- True

-- print top stack frame before each call
rtsTraceStack :: Bool
rtsTraceStack = rtsDebug -- False -- True

-- trace all foreign calls
rtsTraceForeign :: Bool
rtsTraceForeign = rtsDebug

rtsHaveWeakMap :: Bool
rtsHaveWeakMap = False

-- inline all allocations, usually small allocations are done through
-- h$cN, h$dN to save some code size, these small functions are expected
-- to be inlined by the JS engine
rtsInlineAlloc :: Bool
rtsInlineAlloc = False

-- inline h$r1.f = h$blackhole; ... things
rtsInlineBlackhole :: Bool
rtsInlineBlackhole = False

rtsInlineEnter :: Bool
rtsInlineEnter = False

rtsInlinePush :: Bool
rtsInlinePush = False

------------------------------------------------------------------------------
-- end of settings
{-
   some utilities to make constructing jmacro expressions without the
   quasiquoter easier.
-}

infixr 1 |+
infixr 1 |-
infixl 3 |.
infixl 2 |!
infixl 2 |!!

-- a + b
(|+) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|+) e1 e2 = [je| `e1` + `e2` |]

-- a - b
(|-) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|-) e1 e2 = [je| `e1` - `e2` |]

-- a & b
(|&) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|&) e1 e2 = [je| `e1` & `e2` |]

-- a.b
(|.) :: ToJExpr a => a -> Text -> JExpr
(|.) e i = SelExpr (toJExpr e) (TxtI i)

-- a[b]
(|!) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|!) e i = [je| `e`[`i`] |]

-- a[b] with b int
(|!!) :: ToJExpr a => a -> Int -> JExpr
(|!!) = (|!)

-- a(b1,b2,...)
(|^) :: ToJExpr a => a -> [JExpr] -> JExpr
(|^) a bs = ApplExpr (toJExpr a) bs

(|^^) :: Text -> [JExpr] -> JExpr
(|^^) a bs = ApplExpr (jsv a) bs

(|||) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|||) a b = [je| `a` || `b` |]

(|&&) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|&&) a b = [je| `a` && `b` |]

(|===) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|===) a b = [je| `a` === `b` |]

(|!==) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|!==) a b = [je| `a` !== `b` |]

-- trace a gc-related messages if gc debug is enabled
traceGc :: ToJExpr a => a -> JStat
traceGc = traceIf gcDebug "gc"

-- run some statement if gc debugging is enabled
debugGc :: JStat -> JStat
debugGc = jstatIf gcDebug

assertGc :: ToJExpr a => JExpr -> a -> JStat
assertGc e m = jstatIf gcChecks [j| if(!`e`) { throw `m` } |]

-- run some statement if gc consistency checking is enabled
checkGc :: JStat -> JStat
checkGc = jstatIf gcChecks

-- trace a gc timing related message if gc timing is enabled
traceGcTiming :: ToJExpr a => a -> JStat
traceGcTiming = traceIf gcTiming "gc"

-- trace an rts related message if rts debugging is enabled
traceRts :: ToJExpr a => a -> JStat
traceRts = traceIf rtsDebug "rts"

assertRts :: ToJExpr a => JExpr -> a -> JStat
assertRts e m = jstatIf rtsDebug [j| if(!`e`) { throw `m` } |]

-- add a statement if rts debugging is enabled
debugRts :: JStat -> JStat
debugRts = jstatIf rtsDebug

traceIf :: ToJExpr a => Bool -> String -> a -> JStat
traceIf True  s e = [j| log(`s` + ": " + `e`); |]
traceIf False _ _ = mempty

jstatIf :: Bool -> JStat -> JStat
jstatIf False _ = mempty
jstatIf _     s = s

stringify :: ToJExpr a => a -> JExpr
stringify x = [je| JSON.stringify(`x`) |]

clName :: JExpr -> JExpr
clName c = [je| `c`.n |]

clTypeName :: JExpr -> JExpr
clTypeName c = [je| h$closureTypeName(`c`.t) |]

notUndef :: ToJExpr a => a -> JExpr
notUndef e = [je| `e` !== undefined |]

isNumber :: ToJExpr a => a -> JExpr
isNumber e = [je| typeof `e` === "number" |]

isFunction :: ToJExpr a => a -> JExpr
isFunction e = [je| typeof `e` === "function" |]

isArray :: ToJExpr a => a -> JExpr
isArray e = [je| typeof(`e`.length) !== "undefined" |] -- [je| `e` instanceof Array |] -- bah jmacro doesn't support this
{-
setObjInfo :: (ToJExpr a, ToJExpr b) => a -> b -> JStat
setObjInfo obj y = [j| _objInfo(`obj`,`y`); |]
-}

