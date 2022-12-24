
Introduction
============

GHCJS is a Haskell to JavaScript compiler that uses the GHC API.

Quick Start - Developing GHCJS
==============================

GHCJS contains a library, `ghcjs`, which contains the JavaScript code generator and a slightly customized variant of the `ghc` library, and several executable programs.

The repository has several submodules and some files must be generated before the package can be installed.

### prerequisites

#### GHC

You need the same major version of GHC as the version of the GHCJS branch you're building.

#### cabal-install

cabal-install 3.0 is supported

#### emscripten emsdk

GHCJS uses a C toolchain, mostly for build system related tasks like the C preprocessor, Autoconf scripts and tools like `hsc2hs`. Direct support for using compiled foreign libraries from Haskell code may follow at a later date.

Please follow the installation instructions at https://emscripten.org/docs/getting_started/index.html

GHCJS requires the "upstream" emscripten backend, which is the default now. The earlier "fastcomp" backend will not work.

### getting and preparing the source tree

```
$ git clone https://github.com/ghcjs/ghcjs.git
$ cd ghcjs
$ git submodule update --init --recursive
```



### building the compiler

GHCJS depends on a few "local" packages in the source tree. You can use
`cabal-install` and `stack` to set up a build environment that contains
these packages.

#### Cabal new-install

After the source tree has been prepared, the package can be installed.
You may want ensure that binaries of earlier versions are overwritten:

```{.shell}
cabal v2-install --overwrite-policy=always --install-method=copy --installdir=inplace/bin
```

At the time of writing, `cabal-install` does not support creating symbolic links on Windows, even though this is the default installation method. A workaround is telling it to copy the executables instead:

```{.shell}
cabal v1-install --prefix=inplace
```

#### v1 style Cabal sandbox

v1 style cabal sandboxes are also supported

if you want to build with a Cabal sandbox, use the `makeSandbox.sh` script
to add the local packages.

```
$ cabal v1-sandbox init
$ cabal v1-install
```

#### stack

or you can use stack:

```
$ stack install
```

#### Booting GHCJS

The `ghcjs-boot` program builds the "boot" libraries, like `ghc-prim`, `base` and `template-haskell` with GHCJS. After booting, GHCJS can compile regular Haskell programs and packages.

`ghcjs-boot` needs to be able to find the emscripten toolchain, a nodejs executable. The easiest way to do this is by running the `emsdk_env.sh` script. After that, you can run `ghcjs-boot` by pointing it to the boot libraries (the directory containing the `boot.yaml` file)

```
$ source ~/emsdk/emsdk_env.sh
$ ./inplace/bin/ghcjs-boot -s ./lib/boot
```

### GHCJS executables and library paths


After booting, you can add the directory containing the GHCJS binaries to
your executable PATH. The `ghcjs-boot` program prints the location after
finishing building the libraries.

You can also create a symbolic link for the `ghcjs` and `ghcjs-pkg`
programs, or use the `--with-compiler` and `--with-hc-pkg` flags
when using `cabal-install`

#### Generating a source distribution

if you work on boot packages that need some for an upstream library,
make sure to update the patches in `/lib/patches` first

```
$ ./utils/updatePatches.sh
```

then regenerate the packages

```
$ ./utils/makePackages.sh
```
