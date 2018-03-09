#!/usr/bin/env bash

# install and test/benchmark common packages with GHCJS
#
# the script looks for a file named testPackages.constraints
# in its source own dir
#
# this constraints set is applied to the MOREPKGS set
# use for example an lts constraint set:
#   https://www.stackage.org/lts-10.5/cabal.config
#
# the EXTRAPKGS set is installed one by one, without the
# constraints applied

set -x
# set -e

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

# included with the compiler, possibly patched
# we skip them for now
# BOOTPKGS=(base
#          binary
#          bytestring
#          Cabal
#          containers
#          deepseq
#          directory
#          filepath
#          process
#          time
#          text
#          )

MOREPKGS=(
          base-compat
          base-orphans
          call-stack

          dlist
          extensible-exceptions
          generic-deriving
          hashable
          mtl
          primitive
          random
          scientific
          syb
          tagged


          adjunctions
          auto-update
          base64-bytestring
          beam-core
          bifunctors
          bimap
          binary
          blaze-builder
          blaze-markup
          bytes
          bytestring
          bytestring-conversion
          cassava
          cereal
          comonad
          conduit
          conduit-extra
          constraints
          containers
          deburr
          deepseq
          dependent-sum-template
          directory
          directory-tree
          distributive
          double-conversion
          either
          exceptions
          exinst
          fast-logger
          filepath
          generics-sop
          haskell-src-exts
          haskell-src-meta
          heist
          hspec
          hspec-core
          hspec-expectations
          http-types
          insert-ordered-containers
          lens
          map-syntax
          monad-loops
          natural-transformation
          network-uri
          newtype-generics
          parsec
          path-pieces
          persistent
          persistent-template
          prelude-extras
          pretty
          process
          quickcheck-instances
          resourcet
          safe
          semigroupoids
          servant
          servant-cassava
          servant-docs
          silently
          singletons
          split
          streaming-commons
          string-conversions
          swagger2
          th-desugar
          th-expand-syns
          th-lift
          th-lift-instances
          th-orphans
          th-reify-many
          these
          time
          tzdata
          unix-time
          unordered-containers
          uri-bytestring
          xmlhtml
          async
          aeson-compat
        )

# outdated dependencies incompatible with main package set
# test one by one
EXTRAPKGS=(attoparsec
           aeson
           uuid-types
           blaze-html
           case-insensitive
           exception-transformers
           http-api-data
           integer-logarithms
           lifted-base
           text
           colour
           vector
          )

# move these back once they have been updated?
# http-media: test suite takes very long
NOTWORKING=(reflex
            non-empty-zipper
            vector-bytes-instances
            http-media
           )

function testPackages() {
  cabal \
    --sandbox-config-file=./cabal.sandbox.config \
    install \
    -v3 \
    --ghcjs \
    --disable-documentation \
    -j8 \
    --keep-going \
    --build-summary=log/build-summary-\$compiler-\$pkgid.txt \
    --build-log=log/build-log-\$compiler-\$pkgid.txt \
    --run-tests \
    --enable-tests \
    "$@"

}
DATE=`date '+%Y_%m_%d-%H_%M_%S'`
DIR=$(mktemp --tmpdir=. -d -t testPackages-${DATE}.XXXXX) || exit 1
echo "using ${DIR}"
(
cd "$DIR"

# make a sandbox
mkdir log
cabal sandbox init

# build MOREPKGS with our version constraints
cp "${SOURCEDIR}/testPackages.constraints" cabal.config

# testPackages "${MOREPKGS[@]}"
# XXX remove the one by one build
for pkg in "${MOREPKGS[@]}"
do
  testPackages "${pkg}"
done

# build EXTRAPKGS without version constraints
rm cabal.config
for pkg in "${EXTRAPKGS[@]}"
do
  testPackages "${pkg}"
done

# keep track of what failed
(
cd log
grep install-outcome build-summary*.txt | grep -v InstallOk | \
   grep -v DependencyFailed | uniq > ../failures.txt
)

)
