Haskell to Javascript translator
================================

This project aims to make Haskell applications work online by compiling
them to JavaScript.

Applications that use the following libraries should compile with GHCJS
and run in a modern web browser and interface with DOM and JavaScript
in the browser.
 * [webkit](https://patch-tag.com/r/hamish/webkit) - Bindings for WebKitGTK+ that provide a low level DOM interface.
 * [webkit-javascriptcore](https://github.com/ghcjs/webkit-javascriptcore) - Low level bindings for JavaScriptCore
 * [jsc](https://github.com/ghcjs/jsc) - Higher level inteface for JavaScriptCore

You can use these libraries without GHCJS to build a native version of
your application (it will use WebKitGTK+ to run without a browser).
If you want to find out more about making GHCJS compatible Haskell
applications check out the [GHCJS Examples](https://github.com/ghcjs/ghcjs-examples/)

Previous version of project is located at [vir.mskhug.ru](http://vir.mskhug.ru/).

Code Generators
===============

We currently have three code generators.

 * *plain* - simple but tail calls will blow the stack
 * *trampoline* - most complete, but slow
 * *gen2* - faster, but still work in progress

Unless you are interested in helping on gen2, we suggest you stick to
the trampoline code generator for now.


Installing GHCJS
================

This version of GHCJS can be built stand alone or integrated into GHC.
You can install both (if you know you want both do Integrated first).

Integrated
 * A full GHC that also outputs .[plain | trampoline | gen2].js files and
   .[plain | trampoline | gen2].jsexe directories when you build.
 * A patched Cabal that installs the .js files along with the .hi ones.
 * Takes a while to install (mostly just follow the regular GHC build instructions).

Stand Alone
 * Uses GHC API to make a ghcjs executable.
 * Quicker to install (because you don't have actually build ghc).
 * Good for trying out changes in the code generator itself.
 * Still requires some messing with GHC source.


Common Requirements
===================

Linux.  We have no one developing on OS X or Windows at present
and if you really want GHCJS on those platforms you are bound to
have problems that we have not even seen.

 * Google Closure Compiler https://developers.google.com/closure/compiler/
 * Google Closure Library https://developers.google.com/closure/library/
 * Java (to run the Closure Compiler)

For the best JavaScript performance you should probably use a 32bit version of
of ghc.  If you use the 64bit build it will use goog.math.Long for Int and Word.


Integrated
==========

Aditional Requirements
----------------------

 * GHC install capable of building GHC 7.6.1
 * 3.5GB RAM (JavaScript linker is memory hungry)

Tested On
---------

 * Ubuntu 12.04 64bit VM with 3.5GB of system RAM using 64bit GHC 7.6.1 to build 64bit GHC

Getting The Source
------------------

<pre>
git clone https://github.com/ghcjs/ghc
cd ghc
git checkout ghc-7.6
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghcjs --ghcjs get
./sync-all checkout ghc-7.6
cabal update
./unpack.sh
</pre>

(I find the gets from github often fail hence the 4 of them)

Set WORD_SIZE_IN_BITS to match GHC
----------------------------------
If you are using a 64bit GHC edit ghc/libraries/ghcjs/rts/rts-options.js and
change WORD_SIZE_IN_BITS from 32 to a 64 before installing.

Build
-----
We have prepared a script to do the rest.

<pre>
./ghcjs-build.sh
</pre>

This script will install the following to your $HOME/ghcjs/bin
 * ghc - integrated GHCJS compiler
 * cabal and cabal-meta - versions that will install .js files and .jsexe directories
 * ghcjs-min - run this on your jsexe's to minify them

As a bonus it will also build the stand alone compiler and install it in the same directory
 * ghcjs - the stand alone GHCJS built with the integrated one
 * ghcjs-cabal - cabal-install for use with the stand alone compiler

To use this compiler add $HOME/ghcjs/bin to your path ahead of any other ghc.

<pre>
export PATH=$HOME/ghcjs/bin:$PATH
</pre>

You should be able to switch back to your main compiler at any point by simply not
including this in you path.

The global packages (including JavaScript) will be installed to something like

 * ~/ghcjs/lib/ghc-7.6.1.20121201

User cabal packages are installed to something like

 * ~/.ghc/i386-darwin-7.6.1.20121201
 * ~/.cabal/lib/*/ghc-7.6.1.20121201

Installing and running an application
-------------------------------------
With $HOME/ghcjs/bin in your PATH the follow the steps to build 
[GHCJS Examples](https://github.com/ghcjs/ghcjs-examples/) 
except "cabal install cabal-meta cabal-src" (as you don't want another
ghcjs enabled cabal-meta installed).

Then run ghcjs-min on the .jsexe this will have installed (allong side your normal
executable)

<pre>
ghcjs-min ~/.cabal/bin/ghcjs-hello
</pre>

On OS X it will be something like this...
<pre>
ghcjs-min ~/Library/Haskell/ghc-7.6.1.20120701/lib/ghcjs-hello-0.0.1/bin/ghcjs-hello
</pre>

Most browsers will not run JavaScript off the file system so the next step is to
share the .jsexe file with a web server.  ALthough you don't need the rts, hterm
and closure-library shared to run the minified version sharing those as well
makes the source maps work and allows you to run the unminified JavaScript.

I find the easiest way to do this is to use apache with the FollowSymlinks options
(not always on by default).

On Linux...
<pre>
cd ~/public_html
ln -s ../.cabal ghcjs
</pre>

On OS X you might need something like this...
<pre>
cd ~/Sites
mkdir ghcjs
mkdir ghcjs/share
mkdir ghcjs/bin
ln -s ../Library/Haskell/ghc-7.6.1.20121201/lib/ghcjs-0.1.0/share ghcjs/share/ghcjs-0.1.0
ln -s ../Library/Haskell/ghc-7.6.1.20121201/lib/ghcjs-closure-0.1.0.0/share ghcjs/share/ghcjs-closure-0.1.0.0
ln -s ../Library/Haskell/ghc-7.6.1.20121201/lib/ghcjs-hterm-0.1.0.0/share ghcjs/share/ghcjs-hterm-0.1.0.0
ln -s ../Library/Haskell/ghc-7.6.1.20121201/lib/ghcjs-hello-0.0.1/bin/ghcjs-hello.trampoline.jsexe ghcjs/bin/
</pre>

ghcjs-min will set up an index.html file (if one does not already exist in the .jsexe)
that will run the app with hterm.
<pre>
http://127.0.0.1/~hamish/ghcjs/bin/ghcjs-hello.trampoline.jsexe
</pre>

You can run the unminified version using hterm.html
<pre>
http://127.0.0.1/~hamish/ghcjs/bin/ghcjs-hello.trampoline.jsexe/hterm.html
</pre>

If you don't want the overhead of hterm then there is a version that sends stdout
to the console (and does not currently support stdin)
<pre>
http://127.0.0.1/~hamish/ghcjs/bin/ghcjs-hello.trampoline.jsexe/console.html
</pre>

If you want to minify the console.html version then run (and make it the one index.html
uses then run)...
<pre>
ghcjs-min ~/.cabal/bin/freecell.trampoline.jsexe ~/.cabal/bin/freecell.trampoline.jsexe/console.js
</pre>


Stand-alone
===========

These are out of date.

TODO : Update the stand alone instructions.

The stand-alone compiler is an attempt to make GHCJS easier to use and install. It does
not require you to replace your existing GHC. GHCJS stand-alone has its own package
database in `~/.ghcjs/` and its own programs `ghcjs-pkg` and `ghcjs-cabal` to manage
and install packages.

The stand-alone version is currently experimental, and the linker might not work yet.
Cabal-dev is also unsupported at this point.

Installing
----------

First install `ghcjs-closure`, `source-map` and `ghcjs-hterm` from their respective
repositories

Now install GHCJS itself (assuming you use ghc 7.6.1 for building ghcjs):

    $ git clone https://github.com/ghcjs/ghcjs.git
    $ cd ghcjs
    $ cabal install
    $ cd ..

Make sure that the directory containing the `ghcjs`, `ghcjs-pkg` and `ghcjs-cabal` programs is
in your PATH before proceeding:

    $ wget http://www.haskell.org/ghc/dist/7.6.1/ghc-7.6.1-src.tar.bz2
    $ tar -xjvf ghc-7.6.1-src.tar.bz2
    $ cd ghc-7.6.1
    $ ./configure
    $ make -j4
    $ ghcjs-boot
    $ cd ..

You should now have a working `ghcjs` installation with the `base`, `ghc-prim`, `rts` and `integer-gmp`
packages installed, check with:

    $ ghcjs-pkg list

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
