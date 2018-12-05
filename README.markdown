
Introduction
============

GHCJS is a Haskell to JavaScript compiler that uses the GHC API.

Quick Start - Developing GHCJS
==============================

Starting with GHC version 8.2, GHCJS depends on a customized `ghc` library,
installed under the name `ghc-api-ghcjs`

#### getting and preparing the source tree

```
$ git clone --branch ghc-8.6 https://github.com/ghcjs/ghcjs.git
$ cd ghcjs
$ git submodule update --init --recursive
$ ./utils/makePackages.sh
```

The `./utils/makePackages.sh` script requires Bash version 4.0 or greater. If you are building on macOS, you will need the gnu version of tar. You can install this with `brew install gnu-tar`, which makes it accessible at `gtar`. The `./utils/makePackages.sh` will automatically pick up on this.

#### building the compiler

GHCJS depends on a few "local" packages in the source tree. You can use
`cabal-install` and `stack` to set up a build environment that contains
these packages.

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

The `ghcjs-boot` program builds the "boot" libraries, like `ghc-prim`, `base` and `template-haskell` with GHCJS. After booting, GHCJS can compile regular
Haskell programs and packages.

```
$ ghcjs-boot
```

when invoked without arguments, ghcjs-boot will build the libraries from
`boot.tar` (unless the current directory contains a `boot.yaml` file), installed in GHCJS' data directory (`boot.tar` is generated
by the `makePackages.sh` script and included in a source distribution).

Optionally you can point `ghcjs-boot` to a different location, like another
`boot.tar` archive:

```
$ ghcjs-boot -s location/of/boot.tar
```

or a directory (must contain a `boot.yaml` file):

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

if you work on boot packages that need some for an upstream library,
make sure to update the patches in `/lib/patches` first

```
$ ./utils/updatePatches.sh
```

then regenerate the packages and the `/data/boot.tar` archive

```
$ ./utils/makePackages.sh
```

and the source distribution archive

```
$ cabal sdist
```
