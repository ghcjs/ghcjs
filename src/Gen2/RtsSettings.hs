{-# LANGUAGE QuasiQuotes #-}
{-
   some settings to control debugging output of the rts
   -- fixme, make cabal flags, or runtime flags?
-}
module Gen2.RtsSettings where

import           Data.Monoid
import           Gen2.Utils
import           Language.Javascript.JMacro

-- debugging settings (fixme, make these depend on cabal flags?)

-- trace/debug messages from the garbage collector
gcDebug :: Bool
gcDebug = False

-- timing measurements for the garbage collector
gcTiming :: Bool
gcTiming = True

-- extra checks from the gc for consistency, usually shouldn't print, but make it a little slower
gcChecks :: Bool
gcChecks = False

-- extra rts assertions/checks
rtsChecks :: Bool
rtsChecks = rtsDebug

-- rts tracing/debugging
rtsDebug :: Bool
rtsDebug = True

-- trace calls from the trampoline
rtsTraceCalls :: Bool
rtsTraceCalls = rtsDebug -- False -- True

-- print top stack frame before each call
rtsTraceStack :: Bool
rtsTraceStack = rtsDebug -- False -- True

------------------------------------------------------------------------------
-- end of settings
{-
   some utilities to make constructing jmacro expressions without the
   quasiquoter easier.
-}

infixr 1 |+
infixr 1 |-
infixr 3 |.
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
(|.) :: ToJExpr a => a -> String -> JExpr
(|.) e i = SelExpr (toJExpr e) (StrI i)

-- a[b]
(|!) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|!) e i = [je| `e`[`i`] |]

-- a[b] with b int
(|!!) :: ToJExpr a => a -> Int -> JExpr
(|!!) = (|!)

-- a(b1,b2,...)
(|^) :: ToJExpr a => a -> [JExpr] -> JExpr
(|^) a bs = ApplExpr (toJExpr a) bs

(|^^) :: String -> [JExpr] -> JExpr
(|^^) a bs = ApplExpr (jsv a) bs

(|||) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|||) a b = [je| `a` || `b` |]

(|&&) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|&&) a b = [je| `a` && `b` |]

(|===) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
(|===) a b = [je| `a` === `b` |]

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
clTypeName c = [je| closureTypeName(`c`.t) |]

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

