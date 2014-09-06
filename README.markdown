
Haskell to JavaScript compiler
==============================

GHCJS is a Haskell to JavaScript compiler that uses the GHC API.

GHCJS supports many modern Haskell features, including:

 * All type system extensions supported by GHC
 * Lightweight preemptive threading with blackholes, MVar, STM, asynchronous exceptions
 * Weak references, CAF deallocation, StableName, StablePtr
 * Unboxed arrays, emulated pointers
 * Integer support through [JSBN](http://www-cs-students.stanford.edu/~tjw/jsbn/), 32 and 64 bit signed and unsigned arithmetic (`Word64`, `Int32` etc.)
 * Cost-centres, stack traces
 * Cabal support, GHCJS has its own package database

And some JavaScript-specific features:

 * new JavaScriptFFI extension, with convenient import pattens, asynchronous FFI and a JSRef FFI type,
 * synchronous and asynchronous threads.

Installation
============

First install GHC 7.8.2 or later and check with `ghc --version` that it's the
compiler in your `PATH`. Next, make sure that you have all the prerequisites for your platform.

### Platform-specific requirements

#### Linux / OS X

 * a recent version of `alex` and `happy` need to be in your `PATH`
 * `git`, `make`, `cpp`, `autoreconf`, `patch` need to be in your `PATH`
 * by default, `ghcjs-boot` will try to use the system GMP library, see
   `ghcjs-boot --help` for more info.

#### Windows

 * virus scanners often interfere with configure scripts (permission denied errors),
   disable on-access scanning before running `ghcjs-boot`.
 * no extra programs need to be installed, `ghcjs-boot` will download an archive (around 100MB)
   with the required programs.

### Installation steps

Run the following script to install an updated `Cabal` and `cabal-install` with GHCJS
support. Note that this will overwrite the `cabal` executable in your cabal executable
installation path (typically `~/.cabal/bin`), you might want to backup your current version.

```bash
#!/bin/sh
git clone https://github.com/ghcjs/cabal.git
cd cabal
git checkout ghcjs
cabal install ./Cabal ./cabal-install
```

Make sure that you're now running the new `cabal-install`, `GHCJS` support must be listed under the
compiler flags:

    $ cabal install --help
    ...
                                    build files (default dist)
     -g --ghc                           compile with GHC
        --ghcjs                         compile with GHCJS
        --nhc98                         compile with NHC
    ...

Next, install `ghcjs` and its `ghcjs-prim` dependency:

    $ git clone https://github.com/ghcjs/ghcjs-prim.git
    $ git clone https://github.com/ghcjs/ghcjs.git
    $ cabal install ./ghcjs ./ghcjs-prim

If `cabal install ./ghcjs ./ghcjs-prim` fails because cabal cannot resolve dependencies, try adding `--reorder-goals --max-backjumps=-1`. Sometimes the `transformers` package causes problems, since GHC ships with an older version. Try `--constraint=transformers==0.3.0.0` (or the version that came with your GHC) if the problem looks related to this package.

Build the base libraries for `GHCJS`:

    if you used the Git repository to install:
    $ ghcjs-boot --dev

    if you are using a package from hackage that includes the libraries:
    $ ghcjs-boot

Some distros install node.js as `nodejs` instead of `node`. Add `--with-node nodejs` to the `ghcjs-boot` command in that case.

Usage
=====

`ghcjs` can be invoked with the same command line arguments as `ghc`. The generated programs can be run directly from
the shell with [Node.js](http://nodejs.org/) and [SpiderMonkey jsshell](http://download.cdn.mozilla.net/pub/firefox/nightly/latest-mozilla-central/).
for example:

    $ ghcjs -o helloWorld helloWorld.hs
    $ node helloWorld.jsexe/all.js
    Hello world!

### Cabal support

Use `cabal install --ghcjs packageName` to install a package

Most packages from hackage should work out of the box. The main exception is packages with foreign (non-Haskell) dependencies.
For these packages a JavaScript implementation of the dependencies must be provided. If a package you want to use does
not work, please create a ticket.

### Sandboxes

You can create a new sandbox or add GHCJS support to an existing sandbox with:

    $ cabal sandbox init ghcjs

If you also want to set GHCJS as the default compiler in the sandbox, run:

    $ cabal sandbox init ghcjs
    $ echo "compiler: ghcjs" >> cabal.config

### Package databases

Use `ghcjs-pkg` to manipulate the GHCJS package database

The package database and runtime files from the [shims](https://github.com/ghcjs/shims.git) repository are kept in the
GHCJS application data directory, typically `~/.ghcjs/`. Remove this directory to reset your GHCJS installation, you
will need to run `ghcjs-boot --init` again.

See [GHCJS introduction](http://weblog.luite.com/wordpress/?p=14) for more examples.

Hacking GHCJS
=============

If you want to hack on GHCJS, please join our friendly community on IRC at `#ghcjs` on freenode (You're also
welcome if you only use the compiler or just want to chat about it!). Read the [HACKING.markdown](HACKING.markdown) document
to get started. The [wiki](https://github.com/ghcjs/ghcjs/wiki) may also contain useful information.

JSC and webkit
==============

Applications that use the following libraries should compile with GHCJS
and run in a modern web browser and interface with DOM and JavaScript
in the browser.
 * [webkit](https://patch-tag.com/r/hamish/webkit) - Bindings for WebKitGTK+ that provide a low level DOM interface.
 * [webkit-javascriptcore](https://github.com/ghcjs/webkit-javascriptcore) - Low level bindings for JavaScriptCore
 * [jsc](https://github.com/ghcjs/jsc) - Higher level interface for JavaScriptCore

You can use these libraries without GHCJS to build a native version of
your application (it will use WebKitGTK+ to run without a browser).
If you want to find out more about making GHCJS compatible Haskell
applications check out the [GHCJS Examples](https://github.com/ghcjs/ghcjs-examples/)
