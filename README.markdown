Haskell to Javascript translator
================================

Project aims to provide solution to

 * compile modern Haskell libraries to Javascript files and use
   them in Ajax applications or
 * develop entire Ajax application in Haskell language

Previous version of project is located at [vir.mskhug.ru](http://vir.mskhug.ru/).

This version of GHCJS can be built stand alone or integrated into GHC.
You can install both (if you know you want both do Integrated first).

Integrated
 * A full GHC that laso outputs .js files and .jsexe directories when you build.
 * A patched Cabal that installs the .js files along with the .hi ones.
 * Takes a while to install (mostly just follow the regular GHC build instructions).

Stand Alone
 * Uses GHC API to make a ghcjs executable.
 * Quicker to install (because you don't have actaully build 7.4).
 * Good for trying out changes in the code generator itself.
 * Still requires some messing with GHC source.


Common Requirements
===================

 * Google Closure Compiler https://developers.google.com/closure/compiler/
 * Google Closure Library https://developers.google.com/closure/library/
 * Java (to run the Closure Compiler)

For the best JavaScript performance you should probably use a 32bit version of
of ghc.  If you use the 64bit build it will use goog.math.Long for Int and Word.

Integrated
==========

Aditional Requirements
----------------------

 * GHC install capable of building GHC 7.4.1

Getting The Source
------------------

<pre>
clone https://github.com/hamishmack/ghc
cd ghc
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r git@github.com:hamishmack --ghcjs get
./sync-all checkout ghc-7.4
</pre>

(I find the gets from github often fail hence the 4 of them)

<pre>
cd libraries/Cabal
git remote add hamishmack git@github.com:hamishmack/packages-Cabal.git
git pull hamishmack
git branch ghc-7.4 hamishmack/ghc-7.4
git checkout ghc-7.4
cd ../..
</pre>

Build
-----

Build GHC as per the normal instructions (remember you can use this GHC to build binaries too).

Typical install goes something like this.
<pre>
cp mk/build.mk.sample mk/build.mk
perl boot
./configure --prefix=/home/hamish/ghcjs
make && make install
</pre>

That last step takes a long time.
To use this compiler add /home/hamish/ghcjs to your path ahead of any other ghc.

<pre>
export PATH=/home/hamish/ghcjs/bin:$PATH
</pre>

You should be able to switch back to your main compiler at any point by simply not
including this in you path.

The global packages (including JavaScript) will be installed to something like

 * ~/ghcjs/lib/ghc-7.4.1.20120501

User cabal packages are installed to something like

 * ~/.ghc/i386-darwin-7.4.1.20120501
 * ~/.cabal/lib/*/ghc-7.4.1.20120501

Installing cabal-install with GHCJS
-----------------------------------
You need to make a version of cabal-install that uses the new Cabal package.
So that which you run "cabal install" it will copy .js files and .jsexe directories
to the install location.

<pre>
cd libraries/Cabal/cabal-install
</pre>

<pre>
cabal install --ghc-options='-XFlexibleInstances'
</pre>

There is a catch.  Because your old cabal install installed the dependancies
the .js files for these libraries will not have been installed.  So you shoule
unregister then so they will be installed again with the new cabal-install.

<pre>
ghc-pkg unregister HTTP
ghc-pkg unregister network
ghc-pkg unregister parsec
ghc-pkg unregister mtl
ghc-pkg unregister transformers
ghc-pkg unregister zlib
</pre>

 
Stand Alone 
===========


Aditional Requirements
----------------------

 * GHC 7.4.1 (it may work with earlier versions, but it has not been tested).
 * GHC 7.4.1 source configured and used to do a build

If you built the integrated version you will have the source ready to go.  If
not you will need to follow the steps in Building Prelude.  The unit tests
will lock for the ghc source in ../ghc and the for the closure compiler
in ~/closure-compiler/compiler.jar and library in ~/closure-library

Installing
----------

Code builds as standard haskell package

    $ cabal install --enable-tests

Testing
-------

To build the test Java Script run.

    $ cabal test

Open test.html (for minified code) or test_raw.html for unminified code. You may
need to open these via HTTP for them to work.
    
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
using GHC API. And been tested with 32bit GHC 7.4.1.

Building Prelude
----------------

To play with any Haskell code you'll need Haskell standard library.
GHC implements standard library as a "base" package.
You'll need to compile modules from base package.

  1. Download ghc source distribution for the same version of ghc that you
     use to build ghcjs.

  2. Customize build

         $ cd ghc-x.xx.x
         $ cd mk
         $ cp build.mk.sample build.mk

     Edit mk/build.mk

     Set build flower to the quikest: uncomment the line:

         BuildFlavour = quickest

  3. Build ghc

         $ ./configure
         $ make

  4. You should try to use BuildPackages.hs script. You can build ghc-prim,
     integer-simple and base packages in
     examples directory with the following command

         $ cd examples
         $ ./BuildPackages.hs <path-to-ghc-directory>

     You can build packages manually using instructions below.

=== Building ghc-prim

    $ cd ghc-x.xx.x
    $ cd libraries/ghc-prim
    $ ghcjs -odir <javascript files folder>/ghc-prim -hidir <javascript files folder>/ghc-prim -cpp -fglasgow-exts -package-name ghc-prim GHC/Types.hs
    $ ghcjs -odir <javascript files folder>/ghc-prim -hidir <javascript files folder>/ghc-prim -cpp -fglasgow-exts -package-name ghc-prim GHC/*

=== Building base

    $ cd ghc-x.xx.x
    $ cd libraries/base
    $ ghcjs -odir <javascript files folder>/base -hidir <javascript files folder>/base -hide-package base -package-name base -I./include -i./dist-install/build -XMagicHash -XExistentialQuantification -XRank2Types -XScopedTypeVariables -XUnboxedTuples -XForeignFunctionInterface -XUnliftedFFITypes -XDeriveDataTypeable -XGeneralizedNewtypeDeriving -XFlexibleInstances -XStandaloneDeriving -XPatternGuards -XEmptyDataDecls -XNoImplicitPrelude -XCPP Prelude.hs

This last magic command line was guessed using

    cabal build -v

to see what options are passed to GHC.

We should really use

    -odir /tmp

option to get read of useless object files, but it seems to be a bug in ghc
that cause GHC to rely on odir to be the same as hidir wich is mostly the
case in "normal" GHC usage.

Differences from old version
----------------------------

[vir.mskhug.ru](http://vir.mskhug.ru/) contains the first implementation
of the idea. This version is a rewrite that inherit little code from
previous version. The differences are the following.

### New features

 * Threading
 
 * MVar

 * integer-gmp (using goog.math.Integer)

 * Weak pointers

 * Finalizers

 * Function level linker

 * File Input (uses HTTP and is text only)

 * ByteArrays (using JavaScript ArrayBuffers)

 * More closure compiler friendly output

Features
--------

 * Foreign function interface is not supported.
   Only minimum of primitive operations is supported.

 * Tail recursion optimization is on by default.

   If you don't want it use

       $ ghcjs --calling-convention=plain

Reading The Code
----------------

It would be nice to use a source map to avoid reading the generated javascript
at all.  But until that is done here are some use things to know.

Many of the names used have been shortened to a single character to reduce the
size of the java script.  Here is a key so you can work out what they do.

Some functions include "info" strings that to try to make debugging easier
(controlled with the HS_DEBUG flag). Also some of the
functions have a list of things to keep alive with the thunk. This is also striped
out if you disable support for finalizers and weak pointers (controlled with the
HS_WEAKS flag). It is a nice feature of the Closure Compiler that if you pass more
parameters than the function needs it will remove them from the call site.

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
       <td>$f</td>
       <td>function (first param is the arity)</td>
    </tr>
    <tr>
       <td>$t</td>
       <td>thunk</td>
    </tr>
    <tr>
       <td>$R and $r</td>
       <td>are short for return</td>
    </tr>
    <tr>
       <td>$M</td>
       <td>evaluates its first param (if not already evaluated) and and passes the result to the function provided</td>
    </tr>
    <tr>
       <td>$A</td>
       <td>evaluates if not already evaluated and returns the result</td>
    </tr>
    <tr>
       <td>.C</td>
       <td>calls the function with the list of arguments and passes the result to the function provided</td>
    </tr>
    <tr>
       <td>.J</td>
       <td>jump t a function</td>
    </tr>
</table>
