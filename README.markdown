Haskell to Javascript translator
================================

Project aims to provide solution to

 * compile modern Haskell libraries to Javascript files and use
   them in Ajax applications or
 * develop entire Ajax application in Haskell

Previous version of project is located at [vir.mskhug.ru](http://vir.mskhug.ru/).

This version of GHCJS can be built stand alone or integrated into GHC.
You can install both (if you know you want both do Integrated first).

Integrated
 * A full GHC that also outputs .js files and .jsexe directories when you build.
 * A patched Cabal that installs the .js files along with the .hi ones.
 * Takes a while to install (mostly just follow the regular GHC build instructions).

Stand Alone
 * Uses GHC API to make a ghcjs executable.
 * Quicker to install (because you don't have actually build 7.4).
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
 * 3.5GB RAM (JavaScript linker is memory hungry)

Tested On
---------

* OS X 10.7 using 32bit GHC 7.4.1 to build 32bit GHC
* Ubuntu 12.04 64bit VM with 3.5GB of system RAM using 64bit GHC 7.4.1 to build 64bit GHC

Getting The Source
------------------

<pre>
git clone https://github.com/ghcjs/ghc
cd ghc
git checkout ghc-7.4
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghcjs --ghcjs get
./sync-all checkout ghc-7.4
</pre>

(I find the gets from github often fail hence the 4 of them)

Build
-----

Build GHC as per the normal instructions (remember you can use this GHC to build binaries too).

Typical install goes something like this.
<pre>
cp mk/build.mk.sample mk/build.mk
perl boot
./configure --prefix=/home/hamish/ghcjs
make
make install
hash -r
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
Note: If you prefer to use cabal-dev then you should probably build a new cabal-dev
so that it knows to copy the javascript files (let us know how you get on).

You need to make a version of cabal-install that uses the new Cabal package.
So that which you run "cabal install" it will copy .js files and .jsexe directories
to the install location.

It is a good idea not to put this "cabal" in your default PATH as it may not
play nice with other version of GHC (since it will pass --enable-javascript).
You can do the following to build it and put it next to the GHCJS
enabled version of ghc.

<pre>
export PATH=/home/hamish/ghcjs/bin:$PATH
cd libraries/Cabal/cabal-install
cabal install --prefix=/home/hamish/ghcjs
hash -r
</pre>

There is a catch.  Because your old cabal install installed the dependencies
the .js files for these libraries will not have been installed.  So you should
unregister then so they will be installed again with the new cabal-install.

You can get a list of all the packages that were installed by running
<pre>
ghc-pkg list --user
</pre>

The quickest way to do this delete the directory these are in
<pre>
rm -rf ~/ghcjs/lib/ghc-7.4.1.20120501
</pre>

or you can unregister them using ghc-pkg something like this
<pre>
ghc-pkg unregister HTTP
ghc-pkg unregister network
ghc-pkg unregister parsec
ghc-pkg unregister mtl
ghc-pkg unregister transformers
ghc-pkg unregister zlib
ghc-pkg unregister random
ghc-pkg unregister text
ghc-pkg unregister time
</pre>

Installing the GHCJS package
----------------------------
We still need to install GHCJS as well.  This will include the stand alone compiler,
but the integrated compiler just needs the ghcjs runtime system (rts) and the minifier
(ghcjs-min) that are also installed.

You already have a clone of ghcjs inside the ghc folder, but we need to clone some
others to build it.

Note: If you are using a 64bit GHC edit ghc/compiler/ghcjs/rts/rts-options.js and
change WORD_SIZE_IN_BITS from 32 to a 64 before installing.

<pre>
git clone git@github.com:ghcjs/ghcjs-closure.git
cd ghcjs-closure
cabal install
cd ..
git clone git@github.com:ghcjs/ghcjs-hterm.git
cd ghcjs-closure
cabal install
cd ..
git clone git@github.com:ghcjs/source-map.git
cd ghcjs-closure
cabal install
cd ..
cd ghc/compiler/ghcjs
cabal install
</pre>

Installing and running an application
-------------------------------------
First build the application as you normally would.

<pre>
cabal install hello
</pre>

Then run ghcjs-min on the .jsexe this will have installed (allong side your normal
executable)

<pre>
ghcjs-min ~/.cabal/bin/hello.jsexe
</pre>

On OS X it will be something like this...
<pre>
ghcjs-min ~/Library/Haskell/ghc-7.4.1.20120701/lib/hello-1.0.0.2/bin/hello.jsexe
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
ln -s ../.cabal/share/ghcjs-0.1.0/rts .
ln -s ../.cabal/share/ghcjs-closure-0.1.0.0/closure-library .
ln -s ../.cabal/share/ghcjs-hterm-0.1.0.0 hterm
ln -s ../.cabal/bin/hello.jsexe .
</pre>

On OS X you might need something like this...
<pre>
cd ~/Sites
ln -s ../Library/Haskell/ghc-7.4.1.20120701/lib/ghcjs-0.1.0/share/rts .
ln -s ../Library/Haskell/ghc-7.4.1.20120701/lib/ghcjs-closure-0.1.0.0/share/closure-library .
ln -s ../Library/Haskell/ghc-7.4.1.20120701/lib/ghcjs-hterm-0.1.0.0/share hterm
ln -s ../Library/Haskell/ghc-7.4.1.20120701/lib/hello-1.0.0.2/bin/hello.jsexe .
</pre>

ghcjs-min will set up an index.html file (if one does not already exist in the .jsexe)
that will run the app with hterm.
<pre>
http://127.0.0.1/~hamish/hello.jsexe
</pre>

You can run the unminified version using hterm.html
<pre>
http://127.0.0.1/~hamish/hello.jsexe/hterm.html
</pre>

If you don't want the overhead of hterm then there is a version that sends stdout
to the console (and does not currently support stdin)
<pre>
http://127.0.0.1/~hamish/hello.jsexe/console.html
</pre>

If you want to minify the console.html version then run (and make it the one index.html
uses then run...
<pre>
ghcjs-min ~/.cabal/bin/hello.jsexe ~/.cabal/bin/hello.jsexe/console.js
</pre>


Stand-alone 
===========

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

Now install GHCJS itself (assuming you use ghc 7.4.2 for building ghcjs):

    $ git clone https://github.com/ghcjs/ghcjs.git
    $ cd ghcjs
    $ cabal install
    $ cd ..
    
Make sure that the directory containing the `ghcjs`, `ghcjs-pkg` and `ghcjs-cabal` programs is
in your PATH before proceeding:

    $ wget http://www.haskell.org/ghc/dist/7.4.2/ghc-7.4.2-src.tar.bz2
    $ tar -xjvf ghc-7.4.2-src.tar.bz2
    $ cd ghc-7.4.2
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
