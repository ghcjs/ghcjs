Haskell to JavaScript compiler
==============================

GHCJS is a Haskell to JavaScript compiler using the GHC API. Our first official release is planned
to coincide with the release of GHC 7.8.1, which is expected around ICFP 2013. We are working to get
patches to support GHCJS into GHC and Cabal.

GHCJS supports many modern Haskell features, including:

 * All type system extensions supported by GHC
 * Lightweight preemptive threading with blackholes, MVar, STM, asynchronous exceptions
 * Weak references, CAF deallocation, StableName, StablePtr
 * Unboxed arrays, emulated pointers
 * Integer support through JSBN
 * Cabal support, GHCJS has its own package database

And some JavaScript-specific features:

 * new JavaScriptFFI extension, with convenient import pattens, asynchronous FFI and a JSRef FFI type,
 * synchronous and asynchronous threads.

Installation
============

If you are from the future, you have GHC 7.8 or higher installed, and Cabal 1.18, run the following:

    $ cabal install ghcjs
    $ ghcjs-boot --auto

Done!

If you're not so lucky, the easiest way to install GHCJS is with the vagrant script from the `ghcjs-build` repository:

    $ git clone https://github.com/ghcjs/ghcjs-build.git
    $ git checkout prebuilt
    $ cd ghcjs-build
    $ vagrant up

Or to build everything completely from source (takes a few hours):

    $ git clone https://github.com/ghcjs/ghcjs-build.git
    $ cd ghcjs-build
    $ vagrant up

Log into the vagrant vm with `vagrant ssh`.

Usage
=====

`ghcjs` can be invoked with the same command line arguments as `ghc`. The generated programs can be run directly from
the shell with Node.js and jsshell.
for example:

    $ ghcjs -o helloWorld helloWorld.hs
    $ node helloWorld.jsexe/all.js
    Hello world!

Use `cabal install --ghcjs packageName` to install a package

Use `ghcjs-pkg` to manipulate the GHCJS package database

See [GHCJS introduction](http://weblog.luite.com/wordpress/?p=14) for more examples.

JSC and webkit
==============

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
