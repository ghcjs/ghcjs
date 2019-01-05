
Introduction
============

GHCJS is a Haskell to JavaScript compiler that uses the GHC API.

Quick Start - Developing GHCJS
==============================

Starting with GHC version 8.2, GHCJS depends on a customized `ghc` library,
installed under the name `ghc-api-ghcjs`

### Prerequisites

Make sure you have the following:

* `bash` version 4.0 or higher
* gnu `tar`
* gnu `ln`

On `Linux` you should already have the gnu versions.  On MacOS you can use
"brew install" or "port install" to install the `bash`, `gnutar` and
`coreutils` packages to get these. Make sure that these are available in your
`PATH` environment variable to pick up the right versions. "port install" may
install the gnu tar utility by the name "gnutar" which may have to linked as
"tar" for ghcjs scripts to be able to use it.

You can use `cabal-install` or `stack` to install the following haskell
binaries needed by the GHCJS build:

* cabal-install alex
* cabal-install happy

### building the compiler
#### getting and preparing the source tree

```
$ git clone --branch ghc-8.6 https://github.com/ghcjs/ghcjs.git
$ cd ghcjs
$ git submodule update --init --recursive
$ ./utils/makePackages.sh
```

If for some reason you need to do a clean build use `./utils/cleanPackages.sh`
to clean, you may also have to use `make clean` in the `ghc` directory.

##### Cabal new-build

```
$ cabal new-configure
$ cabal new-build
```

Since `cabal new-build` does not install executables or wrapper scripts,
we need to make them accessible by hand. You can do this by creating symlinks
to the `/utils/dist-newstyle-wrapper.sh` script.

For example if the `.bin` directory is in your PATH:

```
$ cd .bin
$ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs
$ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-pkg
$ ln -s ../utils/dist-newstyle-wrapper.sh haddock-ghcjs
$ ln -s ../utils/dist-newstyle-wrapper.sh hsc2hs-ghcjs
$ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-boot
$ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-run
```

##### Cabal sandbox

if you want to build with a Cabal sandbox, use the `makeSandbox.sh` script
to add the local packages.

```
$ ./utils/makeSandbox.sh
$ cabal install
```

##### stack

or you can use stack:

```
$ stack build
```

#### Booting GHCJS

The `ghcjs-boot` program builds the "boot" libraries, like `ghc-prim`, `base`
and `template-haskell` with GHCJS. Once the boot libraries are built, GHCJS can
compile regular Haskell programs and packages. The sources of all the boot
libraries are available in `data/boot.tar`.

```
$ ghcjs-boot --no-haddock --no-prof -s data/boot.tar
```

`--no-haddock` and `--no-prof` options can be used to save some time and also
to avoid `haddock` issues if any.

The unpacked and patched boot libraries are found in `lib/boot`. You can also
build the boot libraries directly from there if you modified or patched some
libary:

```
$ ghcjs-boot -s ./lib/boot
```

### GHCJS executables and library paths

The GHCJS binaries like `ghcjs` and `ghcjs-pkg` are private executables
and installed in the `libexec` directory. The `Setup.hs` script installs
wrapper scripts in the `bin` directory to pass the library path to the binary.

Note: reinstalling GHCJS (`cabal install`) does not cause existing wrapper
scripts to be overwritten. Remove the wrapper scripts first if you want
a fresh copy.

Example:

`.cabal-sandbox/bin/ghcjs` might contain the following:

```
#!/bin/sh
topdir="/home/luite/.ghcjs/x86_64-linux-8.6.0.1-8.6.2/ghcjs"
executablename="/home/luite/haskell/ghcjs-8.6/ghcjs/.cabal-sandbox/libexec/x86_64-linux-ghc-8.6.2/ghcjs-8.6.0.1/ghcjs"
exec "$executablename" -B"$topdir" ${1+"$@"}
```

To change the library installation location (`topdir`), modify the scripts
prior to running `ghcjs-boot`.

on Windows, an `options` file is used instead of a wrapper script

#### Generating a source distribution

Upstream libraries can be found in `lib/upstream`. These libraries are patched
for `ghcjs` with patches in `lib/patches`. The patched libraries are placed in
`lib/boot`. If you need to fix something you can change the libraries in
`lib/boot` and then run `./utils/updatePatches.sh` to generate the patches
again, reflecting your changes. 

```
$ ./utils/updatePatches.sh
```

then regenerate the patched packages and the `/data/boot.tar` archive

```
$ ./utils/makePackages.sh
```

Note that when you run `./utils/makePackages.sh` it will overwrite the files in
`lib/boot` with patched versions, therefore you may lose any modifications to
those files. To avoid that always run `updatePatches.sh` first to save your
changes.

The source distribution archive can be generated using:

```
$ cabal sdist
```
