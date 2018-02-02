#!/usr/bin/env bash

# clean all the files linked/copied from upstream GHC or elsewhere
# in the GHCJS tree

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

TARGET="$GHCJSROOT/lib"

(
cd "$TARGET"

# customized ghcjs dependencies
rm -f ghc-api-ghcjs
rm -f ghci-ghcjs
rm -f haddock-api-ghcjs
rm -f haddock-library-ghcjs
rm -f template-haskell-ghcjs

# boot packages
rm -f boot/pkg
rm -f boot/ghcjs-node.tar

# generated from primops.txt for boot/pkg/ghc-prim
rm -f boot/data/primops.txt
rm -f boot/data/primops.txt.pp
rm -f boot/data/Prim.hs
rm -f boot/data/PrimopWrappers.hs

)
