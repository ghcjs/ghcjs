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

    $ runghc Setup configure
    $ runghc Setup build
    $ runghc Setup install

Usage
-----

To compile Haskell module to Javascript use `ghcjs` command.

    $ ghcjs Test.hs

This command is merely equivalent to the following

    $ ghc --make Test.hs

but it compiles to Javascript instead of native code.

See examples folder for an example of loading and running haskell code
from browser.

Status
------

The code is in alpha stage. Feel free to experiment with it as you wish.

Implementation
--------------

Compiler is implemented as [GHC](http://www.haskell.org/ghc/) backend
using GHC API. And been tested with GHC 6.12.1.

Building Prelude
----------------

To play with any Haskell code you'll need Haskell standard library.
GHC implements standard library as a "base" package.
You'll need to compile modules from base package.

  1. Download ghc source distribution for the same version of ghc that you
     use to build ghcjs.

  2. Build ghc

         ./configure
         make

  3.

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

Features
--------

 * Foreign function interface is not supported.
   Only minimum of primitive operations is supported.

 * Tail recursion optimization is off by default.

   Use

       $ ghcjs --calling-convention=trampoline

   To optimize tail recursion in resulting code.
