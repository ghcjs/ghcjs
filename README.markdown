Google Summer of Code
---------------------

Haskell.org is participating in [Google Summer of Code](https://www.google-melange.com/gsoc/homepage/google/gsoc2014) again this year. If you're looking for a fun project to work on over the summer and want to learn something about Haskell compilation, a GHCJS-related project might just be the ticket for you. We've compiled a list of ideas to get you started:

[GHCJS Google Summer of Code project ideas](https://github.com/ghcjs/ghcjs/wiki/GHCJS-Google-Summer-of-Code-project-ideas)

Haskell to JavaScript compiler
==============================

GHCJS is a Haskell to JavaScript compiler that uses the GHC API. Our first official release is planned
to coincide with the release of GHC 7.8.1. 

GHCJS supports many modern Haskell features, including:

 * All type system extensions supported by GHC
 * Lightweight preemptive threading with blackholes, MVar, STM, asynchronous exceptions
 * Weak references, CAF deallocation, StableName, StablePtr
 * Unboxed arrays, emulated pointers
 * Integer support through [JSBN](http://www-cs-students.stanford.edu/~tjw/jsbn/), 32 and 64 bit signed and unsigned arithmetic (`Word64`, `Int32` etc.)
 * Cabal support, GHCJS has its own package database

And some JavaScript-specific features:

 * new JavaScriptFFI extension, with convenient import pattens, asynchronous FFI and a JSRef FFI type,
 * synchronous and asynchronous threads.

Installation
============

### Note:

    The GHCJS command line options are still in flux. Make sure that you have the
    latest Cabal patch installed when you update GHCJS. You need both an updated
    `cabal-install` executable and `Cabal` library. Rerun `ghcjs-boot --init` to get the
    latest updates for the libraries. If you get `Not in scope: data constructor ‘GHCJS’`,
    you need to update your `Cabal` library.

First install GHC 7.8 release candidate 2 or later and check with `ghc --version` that it's the
compiler in your `PATH`. Next, make sure that you have all the prerequisites for your platform:

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

Run the following script to install an updated `Cabal` and `cabal-install` with GHCJS
support. Note that this will overwrite the `cabal` executable in your cabal executable
installation path (typically `~/.cabal/bin`), you might want to backup your current version or
append a custom program suffix with a `cabal` option such as `--program-suffix=-js`.

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

Now install `GHCJS` itself:

    $ git clone https://github.com/ghcjs/ghcjs.git
    $ cabal install ./ghcjs

Check that you have the correct version of `GHCJS` in your PATH:

    $ ghcjs --version
    The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.1.0 (GHC 7.8.1)
    $ ghcjs-boot --version
    The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.1.0 (GHC 7.8.1)

Build the base libraries for `GHCJS`:

    $ ghcjs-boot --init


Usage
=====

`ghcjs` can be invoked with the same command line arguments as `ghc`. The generated programs can be run directly from
the shell with [Node.js](http://nodejs.org/) and [SpiderMonkey jsshell](http://download.cdn.mozilla.net/pub/firefox/nightly/latest-mozilla-central/).
for example:

    $ ghcjs -o helloWorld helloWorld.hs
    $ node helloWorld.jsexe/all.js
    Hello world!

Use `cabal install --ghcjs packageName` to install a package

Most packages from hackage should work out of the box. The main exception is packages with foreign (non-Haskell) dependencies.
For these packages a JavaScript implementation of the dependencies must be provided. If a package you want to use does
not work, please create a ticket.

Use `ghcjs-pkg` to manipulate the GHCJS package database

The package database and runtime files from the [shims](https://github.com/ghcjs/shims.git) repository are kept in the
GHCJS application data directory, typically `~/.ghcjs/`. Remove this directory to reset your GHCJS installation, you
need to run `ghcjs-boot --init` again.

See [GHCJS introduction](http://weblog.luite.com/wordpress/?p=14) for more examples.

Hacking GHCJS
=============

If you want to hack on GHCJS, please join our friendly community on IRC at `#ghcjs` on freenode (You're also
welcome if you only use the compiler or just want to chat about it!). The channel can be quiet from time
to time, but with a little patience you should be able to find people to get you started or point you in
the right direction.

The repositories you will be interested in are:

 * [https://github.com/ghcjs/ghcjs](https://github.com/ghcjs/ghcjs)

      The compiler itself, code generator, linker, test suite

      examples:

      * Executables ( `src-bin` )
      * Main code generator ( `src/Gen2/Generator.hs` )
      * Optimizer ( `src/Gen2/Optimizer.hs` )
      * Linker ( `src/Gen2/Linker.hs` )
      * Generated runtime code (`h$ap_x_y` etc): ( `src/Gen2/RtsApply.hs` )

 * [https://github.com/ghcjs/shims](https://github.com/ghcjs/shims)

      Implementation of much of the runtime system and JavaScript dependencies. Code from this repository is linked into
      the `lib.js` and `lib1.js` files when generating a `.jsexe`. Installed in `~/.ghcjs/version/shims` by `ghcjs-boot`. 

      examples:

      * thread scheduler, MVar implementation ( `src/thread.js` )
      * STM implementation (`src/stm.js`)
      * IO system ( `src/io.js` ) (buffered IO for `putStrLn` etc)
      * Heap scanner for weak references and finalizers ( `src/gc.js` )
      * package-specific dependencies (`pkg`), dependencies listed in `packagename.yaml` (temporary measure until Cabal supports JavaScript dependencies directly)
      * third party libraries (`lib`)

 * [https://github.com/ghcjs/ghcjs-base](https://github.com/ghcjs/ghcjs-base)

      GHCJS base library, basic marshalling between JavaScript and Haskell data

If you've changed something that affects the code generated by GHCJS, you'll need to rebuild the base packages (reboot the compiler). Often,
a full reboot is not required. You can run a quick boot to install only the essential packages (e.g. `base`, `integer-gmp`, `array`, but not `ghcjs-base`).
Adjust the `-j4` flag (number of concurrent jobs) for your system:

    $ ghcjs-boot --quick -j4

After a change, please run the test suite to
check that it does not break anything. Install [jsshell](http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-trunk/)
and [node.js](http://nodejs.org/download/) first to make sure that the tests run correctly (`js` and `node` executables should
be in your `PATH`)).

When the compiler has been booted, run `cabal test`, or run the test program directly for more options:

    $ ./dist/build/test/test --help

For example to run a specific test or tests that match a pattern:

    $ ./dist/build/test/test -t pattern

New test cases, in particular if you add new features, are always welcome. The testrunner automatically picks up new tests
(`.hs` files that start with a lowercase letter) in the existing categories.

Debugging GHCJS programs
------------------------

TODO add better instructions here

  * link your program with `-debug`: `ghcjs -debug -o test test.hs`, this adds debugging information to the generated code
  * get more informaton from the runtime system by enabling various tracing and logging options. See the code in the shims repository for more info. Examples:
      * `-DGHCJS_TRACE_SCHEDULER`: messages from the thread scheduler
      * `-DGHCJS_TRACE_CALLS`: print all function calls from the main loop. warning: lots of output. Requires `-debug` for name information
      * `-DGHCJS_TRACE_STACK`: print top of stack for every call in the main loop. warning: even more output
      * `-DGHCJS_TRACE_WEAK`: output related to weak references
      * `-DGHCJS_TRACE_STM`: output for software transactional memory
      * `-DGHCJS_TRACE_GC`: output garbage collector (heap scanner) related messages
  * see the utility programs in `utils` in the ghcjs repository for tools for inspecting object files, quickly bisecting
     bugs in the optimizer etc.

Profiling GHCJS programs
------------------------

You can use the JavaScript profiler to get a basic idea where the majority of time in your program is spent. Non-exported
names (like stack frame functions) start with `h$$`, scroll up in the code to find an exported name to which they belong.

Unfortunately due to Haskell's lazy evaluation and GHCJS's tail-call optimization, the information here is far less
useful than GHC profiles for native code. Profiling support is planned, but implementation has not been started yet.
Profiling will support cost centres like in native GHC. In particular, we intend to add memory profiling support for
interactive (reactive) systems, keeping track of allocations and retention per event. If you're interested in helping
out or discussing features, please contact us in `#ghcjs` on freenode.

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
