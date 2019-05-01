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
rm -rf ghc-api-ghcjs
rm -rf ghci-ghcjs
rm -rf haddock-api-ghcjs
rm -rf haddock-library-ghcjs
rm -rf template-haskell-ghcjs

# boot packages
rm -rf boot/pkg
rm -rf boot/ghcjs-node.tar

# generated from primops.txt for boot/pkg/ghc-prim
rm -rf boot/data/primops.txt
rm -rf boot/data/primops.txt.pp
rm -rf boot/data/Prim.hs
rm -rf boot/data/PrimopWrappers.hs

)
