#!/usr/bin/env bash

# make a sandbox for GHCJS development
#   (after running utils/boot)

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

CABALVER=$(cabal --numeric-version)
if [[ ${CABALVER:0:2} != "1." && ${CABALVER:0:2} != "2." ]]; then
  CMDPREFIX="v1-"
else
  CMDPREFIX=""
fi

(
cd "$SOURCEDIR/.."

cabal "${CMDPREFIX}sandbox" init

# fixme what do we do with these?
cabal "${CMDPREFIX}sandbox" add-source lib/ghci-ghcjs
cabal "${CMDPREFIX}sandbox" add-source lib/template-haskell-ghcjs
cabal "${CMDPREFIX}sandbox" add-source lib/ghcjs-th

)
