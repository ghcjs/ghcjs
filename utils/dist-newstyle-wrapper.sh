#!/usr/bin/env bash

# wrapper utility script for developming in a new-build style environment
#
# this script is less robust and less efficient than the wrappers installed
# by `cabal install`. use only for development.
#
# the script tries to automatically determine the location
# of the GHCJS executables and the appropriate global library
# path
#
# to use the script, create symlinks in a directory in the PATH
#
# for example if .bin is in the PATH:
#
# $ cd .bin
# $ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs
# $ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-pkg
# $ ln -s ../utils/dist-newstyle-wrapper.sh haddock-ghcjs
# $ ln -s ../utils/dist-newstyle-wrapper.sh hsc2hs-ghcjs
# $ ln -s ../utils/dist-newstyle-wrapper.sh ghcjs-boot
#
# this script automatically detects the program to call from the name of
# the link.

ORIGSOURCE="${BASH_SOURCE[0]}"

SOURCE="${BASH_SOURCE[0]}"
# resolve $SOURCE until the file is no longer a symlink
while [ -h "$SOURCE" ]; do
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  # if $SOURCE was a relative symlink, we need to resolve it
  # relative to the path where the symlink file was located
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
SOURCEDIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

# find our build artifacts and determine the GHCJS library directory
pushd "${SOURCEDIR}/../dist-newstyle/build" > /dev/null
ARCH=(*)
ARCH=${ARCH[0]}
pushd "${ARCH}" > /dev/null
pushd ghc-[0-9]* > /dev/null
GHCJS=(ghcjs-[0-9]*)
GHCJS=${GHCJS[0]}
pushd "${GHCJS}/build" > /dev/null
GHCJSVER="${GHCJS:6}" # drop the 'ghcjs-' prefix
GHCLIBVER=`ghcjs/ghcjs -B/ --numeric-ghc-version`
DISTDIR="$PWD"
LIBDIR="$HOME/.ghcjs/${ARCH}-${GHCJSVER}-${GHCLIBVER}/ghcjs"
popd > /dev/null
popd > /dev/null
popd > /dev/null
popd > /dev/null

PGM=${ORIGSOURCE##*/}

if [ "$PGM" = "ghcjs" ]; then
  exec "$DISTDIR/ghcjs/ghcjs" -B"$LIBDIR" ${1+"$@"}
elif [ "$PGM" = "ghcjs-pkg" ]; then
  exec "$DISTDIR/ghcjs-pkg/ghcjs-pkg" --global-package-db \
    "$LIBDIR/package.conf.d" ${1+"$@"}
elif [ "$PGM" = "hsc2hs-ghcjs" ]; then
  exec "$DISTDIR/hsc2hs-ghcjs/hsc2hs-ghcjs" ${1+"$@"}
elif [ "$PGM" = "haddock-ghcjs" ]; then
  exec "$DISTDIR/haddock-ghcjs/haddock-ghcjs" -B"$LIBDIR" ${1+"$@"}
elif [ "$PGM" = "ghcjs-boot" ]; then
  exec "$DISTDIR/ghcjs-boot/ghcjs-boot" ${1+"$@"}
else
  echo "unknown wrapped program name: $PGM"
  exit 1
fi
