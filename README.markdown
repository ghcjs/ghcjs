Haskell to Javascript translator
================================

Project aims to provide solution to

 * compile modern Haskell libraries to Javascript files and use
   them in Ajax applications or
 * develop entire Ajax application in Haskell language

Previous version of project is located at [vir.mskhug.ru](http://vir.mskhug.ru/).

Building
--------

Code builds as standard haskell package

    $ cabal Setup configure
    $ cabal Setup build
    $ cabal Setup install

Usage
-----

To compile Haskell module to Javascript use `ghcjs` command.

    $ ghcjs Test.hs

This command is merely equivalent to the following

    $ ghc --make Test.hs

but it compiles to Javascript instead of native code.

Status
------

The code is in alpha stage. Feel free to experiment with it as you wish.

Implementation
--------------

Compiler is implemented as [GHC](http://www.haskell.org/ghc/) backend
using GHC API. And been tested with GHC 6.12.1.

Differences from old version
----------------------------

[vir.mskhug.ru](http://vir.mskhug.ru/) contains the first implementation
of the idea. This version is a rewrite that inherit little code from
previous version. The differences are the following.

### New features

 * It is more modular.

   But not enough. Generator.Core module needs
   some refactoring to separate some parts into stand alone modules.

 * Generated Javascript code is pretty readable and ideomatic.

 * Lazy module loading is implemented.

   Modules are loaded (using XMLHttpRequest) on demand when they are needed.

### Removed features

 * Primitive operations and foreign function interface is not
   supported.

   Alerts are generated. I want to concentrate on loading and
   modules support at first

 * Tail recursion optimization is gone.

   Every call is native Javascript call without any magic.
   It is MUCH easier to debug this code. Tail call support can be
   easilly added as new Javascript class instance:
   
       data TailCalledJavascript js = TC js
       instance Javascript js => Javascript (TailCalledJavascript js) where
           callFunction func args = TC (... callFunction ...)

