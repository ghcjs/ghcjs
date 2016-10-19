# Introduction

GHCJS Foreign Function Interface(FFI) enables users to

* Interface with javascript functions
* Emulate C dependencies in javascript so that existing haskell modules that depend on C code can be ported to GHCJS.

# Haskell concurrency on GHCJS

A basic understanding of haskell concurency on GHCJS is essential for
understanding foreign function interface(FFI) on GHCJS.

A javascript runtime environment has a single thread. Haskell
RunTimeSystem(RTS) implements its own threads that are executed on the single
javascript thread. Haskell RTS schedules thread execution to ensure smooth
transition between haskell threads and javascript code.

# Calling Javascript Functions from Haskell

GHCJS can import javascript functions with `foreign import javascript`
calling convention.

### Basic JavaScript FFI Syntax

The basic syntax is

```haskell
foreign import javascript unsafe|safe|interruptible
  "javascript expression or javascript statement"
  functionName :: ArgumentType1 -> ArgumentType2 -> ... -> ArgumentTypeN -> ReturnType -- N >= 0
```

`unsafe`, `safe`, and `interruptible` are safety levels. The safety levels will be explained in a separate section. `ArgumentType1` is represented as `$1`, `ArgumentType2` as `$2`, and so on in the string part of FFI.

```haskell
foreign import javascript unsafe "$1 + $2" add :: Int -> Int -> Int
```

If you insert a simple javascript expression such as `$1 + $2` in the string part, you don't have to use `$r`, but if javascript statements are in the string portion, you have to assign the result to `$r`.

```haskell
foreign import javascript unsafe "$r = $1 + $2;" add :: Int -> Int -> Int
```

Don't be surprised by the preceding example. Semicolon, `;` turns a javascript expression into a javascript statement. You can use loops, local variables and other JavaScript constructs. Thanks to JMacro, local variable names in the import are converted to hygienic names, so you don’t need to worry about existing local variables in the code.


```haskell
import GHCJS.Types (JSVal)

foreign import javascript unsafe
  "try { $r = $1 / $2; } catch (e) { $r = "error"; }"
  divide :: Double -> Double -> JSVal
```

```haskell
import GHCJS.Types (JSVal)

foreign import javascript unsafe
   "for(var i=0;i<$3;i++) { $1[i] = $2[i]; }"
   copyArray :: JSVal -> JSVal -> Int -> IO ()
```

In the preceding examples, `JSVal` represents any javascript type. Also, the return type of an imported javascript function should be `IO a` if the function incurs side effects such as mutation, database I/O, file I/O, etc, ... It also needs to return `IO a` if the function cannot reliably return the same output for the same input.

### Argument Types supported in Javascript FFI

#### Primitive Types

Types|Size|Javascript Type
-----|----|---------------
`Int`(32-bit), `Int8`, `Int16`, `Int32`|1|number
`Word`(32-bit), `Word8`, `Word16`, `Word32`|1|number
`Int64`|2|number x number
`Word64`|2|number x number
`Char`|1|number
`Bool`|1|boolean
`Float`|1|number
`Double`|1|number
`Ptr a`, `FunPtr a`, `StablePtr a`|2|object x number
`GHCJS.Types.JSVal`|1|Any Javascript type

Types of size 2 are called size-2 types. A separate section will explain how to handle size-2 types.

#### Newtype

* newtype wrappers to the primitive types or other newtype wrappers
  * `newtype JSString = JSString JSVal`
  * `newtype Window a b c = Window JSVal`
  * `newtype NewInt = NewInt Int`
  * `newtype NewIntA a = NewIntA Int`
  * `newtype NewNewInt = NewNewInt NewInt`
  * ...

#### Type Synonyms

* type synonyms to the preceding types or other type synonyms
  * `type Bingo a = NewIntA a`
  * `type Bingo a b = JSString`
  * `type OkInt = Int`
  * `type OkInt2 = OkInt`
  * ...

### Return Types supported in Javascript FFI

* Any argument type
  * `JSString`
  * `Int`
  * ...
* `IO AnyArgumentType`
  * `IO JSString`
  * `IO Int`
  * ...
* `IO ()`

### Javascript FFI Safety Levels

#### unsafe

The imported code is run directly. returning an incorrectly typed
value leads to undefined behaviour. JavaScript exceptions in the foreign
code kill the Haskell thread.

#### safe

Returned values are replaced with a default value if they have
the wrong type. JavaScript exceptions are caught and propagated as
Haskell exceptions (`JSException`), so they can be handled with the
standard `Control.Exception` machinery.

#### interruptible

The import involves asynchronous callbacks. The calling Haskell thread
sleeps until the foreign code calls the `$c` JavaScript function with
the result. The thread is in interruptible state while blocked, so it
can receive asynchronous exceptions.

If the callback doesn't accept an argument, the return type is `IO ()`.

```haskell
foreign import javascript interruptible "setTimeout($c, $1);"
  delay :: Int -> IO ()
```

If the callback receives an argument of type `a`, the return type is `IO a`.

```haskell
foreign import javascript interruptible "setTimeout($c, $1, param1)"
  delayWithOneParam :: Int -> IO TypeOfParam1
```

If the callback receives more than one argument, the return type is `IO (a, b, ...)`, and you have to explicitly pass arguments to `$c` by wrapping `$c` in another function as below.

```haskell
import GHCJS.Types (JSString, JSVal)

foreign import javascript interruptible
  "require('fs').stat($1, function (err, stat) { $c(err, stat); })" -- node.js API
  js_fsStat :: JSString -> IO (JSVal, JSVal) -- IO (TypeOfErr, TypeOfStat)
```

### Size-2 types in Javascript FFI

You won't need or want to use size-2 types in most cases, but it's explained here for completeness.
In Javascript FFI, arguments of size-2 types are represented as `$1_1`, `$1_2`, `$2_1`, `$2_2`, ...
Result of size-2 type is represented as `$r1` and `$r2`.

For `Int64` and `Word64`, `$1_1` represents the high 32-bits of the first argument. `$1_2` represents the low 32 bits of the first argument. As you can see below, it's better to not use `Word64` and `Int64` in Javascript FFI unless you want to split them into two parts and manipulate the parts separately.

```haskell
foreign import javascript unsafe
  "$r1=$1_1; $r2=$1_2" idInt64 :: Int64 -> Int64

foreign import javascript unsafe
  "$r1=$1_1; $r2=$1_2" idWord64 :: Word64 -> Word64
```

The pointer types(`Ptr a`, `StablePtr a`, and `FunPtr a`) are used mostly to interface with GHC constructs that depend on C code. For the pointer types, `$r1` is a javascript object, and `$r2` is an offset within the object. Examples of dealing with the pointer types are listed below.

```haskell
foreign import javascript unsafe "$r1 = $1; $r2 = 0;"
  js_mkPtr :: JSVal -> Ptr a

foreign import javascript unsafe "$r = $1_1;"
  js_ptrVal  :: Ptr a -> JSVal
```

### Caveats on Javascript FFI

* `Int64` and `Word64` can be manipulated naturally in haskell code, and `Int64` and
`Word64` can be passed into and returned from javascript functions via
Javascript FFI. However, since javascript doesn't support `Int64` and `Word64`,
they are represented as size-2 numerical types in javascript, and it'll be very very difficult to manipulate size-2 numerical types in javascript functions. Therefore, it's *strongly* recommended that you use `Double` as 53-bit integer instead of `Int64` and `Word64` in Javascript FFI.
* It's ok to pass `Float` values into Javascript FFI, and `Float` can be returned
from Javascript FFI. But, since javascript has only `Double`. Javascript FFI
can return `Double` values exceeding the limitation of `Float`. Thus, it's
recommended that you return `Double` or `JSVal` instead of `Float` in Javascript FFI.
* You won't need to use `Ptr a`, `FunPtr a`, or `StablePtr a` in Javascript FFI in most cases because they exist to interface with C code.

# Calling Haskell From Javascript

It is possible to call haskell from javascript. But, as far as I know, there is no clean way to call haskell from javascript, yet. It's better to not think about it for now. If you think calling GHCJS from javascript is doable, feel free to modify this section.

Right now, if you want to write javascript modules or javascript plugins in haskell, you should consider PureScript. It is similar to haskell and interfaces seamlessly with javascript.

# Adding global javascript functions to Cabal Packages

This section assumes that you have a basic understanding of cabal. GHCJS FFI has access to the global javascript variables. If you want to add more javascript functions to the global scope, you can add javascript files to `js-sources` in `.cabal` files as below.

```
library
  js-sources:      jsbits/array.js
                   jsbits/animationFrame.js
                   jsbits/export.js
                   jsbits/jsstring.js
                   jsbits/jsstringRaw.js
                   jsbits/foreign.js
                   jsbits/text.js
                   jsbits/utils.js
                   jsbits/xhr.js
                   jsbits/websocket.js
```

When you upload your GHCJS library or program to hackage, the javascript files in `js-sources` will be uploaded to hackage, too.
It's like `c-sources` for including snippets of C code in cabal packages. Strangely, `CPP` extension is enabled by default in `js-sources`, so you can include a C header in `js-sources`.

```javascript
/* https://github.com/ghcjs/ghcjs-base/blob/master/jsbits/array.js */
#include <ghcjs/rts.h>

/*
   convert an array to a Haskell list, wrapping each element in a
   JSVal constructor
 */
function h$fromArray(a) {
    var r = HS_NIL;
    for(var i=a.length-1;i>=0;i--) r = MK_CONS(MK_JSVAL(a[i]), r);
    return a;
}
```

Following is the included header file `ghcjs/rts.h`

```c
/* https://github.com/ghcjs/ghcjs/blob/master/lib/include/ghcjs/rts.h */
#ifndef __GHCJS_RTS_H_
#define __GHCJS_RTS_H_

#include "constants.h"

/*
 * low-level heap object manipulation macros
 */

#ifdef GHCJS_PROF
#define MK_TUP2(x1,x2)                           (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,(x1),(x2),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
#define MK_TUP3(x1,x2,x3)                        (h$c3(h$ghczmprimZCGHCziTupleziZLz2cUz2cUZR_con_e,(x1),(x2),(x3),h$currentThread?h$currentThread.ccs:h$CCS_SYSTEM))
/* ... */
```

I don't know if it's a good idea to use `CPP` in `js-sources`. Use it at your discretion. Currently, it seems that GHCJS doesn't detect duplicate names in the global scope.

# Emulating C code with `foreign import ccall`

This section assumes that you already learned `foreign import ccall` from GHC FFI. If you don't know GHC FFI already, first read

* https://wiki.haskell.org/Foreign_Function_Interface (Strongly recommended)
* https://www.haskell.org/onlinereport/haskell2010/haskellch8.html (It's a specification for GHC FFI. Recommended)
* http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html teaches you how to actually use GHC FFI. But, you don't need to read it.

Currently, GHCJS doesn't support `wrapper` and `dynamic` properly. Don't use them until https://github.com/ghcjs/ghcjs/issues/536 is fixed.

When do we need to emulate C code? When we want to use haskell libraries or modules that depend on C libraries or `c-sources` in `.cabal` files. GHCJS emulates `foreign import ccall` with javascript.

Let's assume that we have an imported C function, `add3`.

```haskell
foreign import ccall "add3" add3 :: Int -> Int
```

With GHCJS, `add3` should be available as a javascript function in the global scope. Thus, you have to add a javascript file containing `h$add3` to `js-sources` in the `.cabal` file. Beware that `h$` is prefixed to C function names in javascript.

For example, add `jsbits/anything.js` to `js-sources` in `.cabal` file.

```
library
  js-sources: jsbits/anything.js
```

The content of `jsbits/anything.js` is

```javascript
function h$add3(a) {
  return a + 3;
}
```

### Types supported in `foreign import ccall`

https://wiki.haskell.org/Foreign_Function_Interface and https://www.haskell.org/onlinereport/haskell2010/haskellch8.html specify the types supported by `foreign import ccall`, but I'm going to give a brief description.

With the exception that `foreign import ccall` doesn't support `JSVal`, argument types and return types supported by `foreign import ccall` are (probably) the same as those supported by `foreign import javascript`. Actually, `foreign import javascript` was modeled after `foreign import ccall` and supports every type supported by `foreign import ccall` and an additional type, `JSVal`.

### Using size-2 types with `foreign import ccall`

`foreign import ccall` convention doesn't support `$r1`, `$r2`, `$1_1`, `$1_2`, and so on. Thus, size-2 types are passed as two arguments to javascript functions imported by `foreign import ccall`, and if a return value is of a size-2 type, the second part of the return value should be stored in the global variable, `h$ret1`.

Here is an example for size-2 numerical types. If you have the following function,

```haskell
foreign import ccall "idInt64" idInt64 :: Int64 -> Int64
```

add `jsbits/anything.js` to `js-sources` in the project's `.cabal` file.

```
library
  js-sources: jsbits/anything.js
```

```javascript
/* jsbits/anything.js */
function h$idInt64(highInt, lowInt) {
  h$ret1 = lowInt;
  return highInt;
}
```

Here is another example for pointer types.

```haskell
foreign import ccall "idPtr" idPtr :: Ptr a -> Ptr a
```

```javascript
/* jsbits/anything.js */
function h$idPtr(data, offset) {
  h$ret1 = offset;
  return data;
}
```