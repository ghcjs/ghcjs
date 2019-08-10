#!/usr/bin/env bash
set -euo pipefail

# pull the code for GHCJS dependencies from the GHC tree in
#   lib/ghc
#
# this script generates:
#
#  1. customized package dependencies for GHCJS
#       - lib/ghc-api-ghcjs
#       - lib/template-haskell-ghcjs
#       - lib/ghci-ghcjs
#       - lib/haddock-api-ghcjs
#       - lib/haddock-library-ghcjs
#
#  2. GHCJS boot libraries
#       - lib/boot/pkg/*
#
#  3. the boot library archive lib/boot.tar
#       (needed for `cabal sdist')
#
# if used with the "link" option, the sources for the customized packages are
# symlinked to the upstream GHC tree and the customized files in the GHCJS tree
#
# GHCJS-specific configuration files and modified cabal files are
# copied from the 'utils/pkg-input' path. the intention is that
# the upstream GHC tree remains usable to build/validate the upstream
# compiler.

if [ $# -ne 1 ];
then
  echo "usage: $0 [link|copy]"
  echo ""
  echo "build a standalone GHC package from the GHC sources"

  # exit 1
fi

if [ "${1:-link}" = "copy" ]; then
  echo "populating source tree by copying upstream files"
  LINK=0
else
  echo "populating source tree with symbolic links to upstream files"
  LINK=1
fi

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

PKGINPUT="$SOURCEDIR/pkg-input"
PKGCACHE="$SOURCEDIR/pkg-cache"
GHCJSROOT="$SOURCEDIR/.."
TARGET="$GHCJSROOT/lib"

GHCSRC="$GHCJSROOT/ghc"

WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/$(basename $0).XXXXXXXXXXXX")

CABALVER=$(cabal --numeric-version)
if [[ ${CABALVER:0:2} != "1." && ${CABALVER:0:2} != "2." ]]; then
  CMDPREFIX="v1-"
else
  CMDPREFIX=""
fi

# we need ln to support the -r option.
# the BSD ln program on macOS doesn't support it
# try the `gln` program first
gnuln() {
    if hash gln 2>/dev/null; then
        gln "$@"
    else
        ln "$@"
    fi
}

gnucp() {
    if hash gcp 2>/dev/null; then
        gcp "$@"
    else
        cp "$@"
    fi
}

# apply overrides
apply_overrides() {
  echo "apply_overrides: $1"
  pushd "$PKGINPUT/$1"
  for i in *; do
    if [ -d "$i" ];then
      echo "dir: $i"
      mkdir -p "$TARGET/$1/$i"
      apply_overrides "$1/$i"
    elif [ -f "$i" ]; then
      echo "override: $1/$i"
      # remove link if it exists
      rm -f "$TARGET/$1/$i"

      # copy or link override file
      if [ $LINK -ne 0 ]; then
        gnuln -rs "$PKGINPUT/$1/$i" "$TARGET/$1/$i"
      else
        gnucp "$PKGINPUT/$1/$i" "$TARGET/$1/$i"
      fi
    fi
 done
 popd
}

copy_file() {
  SRCPATH=$1
  FILE=$2
  # echo "copy_file:"
  # echo "  $PWD"
  # echo "  $SRCPATH"
  # echo "  $FILE"
  if [ $LINK -ne 0 ]; then
    rm -f "$FILE"
    gnuln -rs "$SRCPATH/$FILE" "$FILE"
  else
    gnucp "$SRCPATH/$FILE" "."
  fi
}

copy_dir() {
  SRCPATH=$1
  DIR=$2
  # echo "copy_dir:"
  # echo "  $PWD"
  # echo "  $SRCPATH"
  # echo "  $DIR"
  if [ $LINK -ne 0 ]; then
    rm -f "$DIR"
    gnuln -rs "$SRCPATH/$DIR" "$DIR"
  else
    gnucp -r "$SRCPATH/$DIR" "."
  fi
}

apply_patch() {
  DEST="$1"
  PATCH="$2"
  (
    cd "$DEST"
    patch -p3 < "$PATCH"
  )
}

# copy a package by running `cabal sdist' and unpacking the source distribution
copy_patch_boot_package_sdist() {
  PKG="$1"
  SRCPREFIX="$2"
  PKGSRC="$GHCSRC/libraries/${SRCPREFIX}${PKG}"
  PKGDST="$TARGET/boot/pkg/$PKG"
  PKGORIG="$TARGET/upstream/pkg/$PKG"
  PKGPATCH="$TARGET/patches/$PKG.patch"
  mkdir -p "$PKGDST"
  echo "copying package (source distribution): $PKG"
  # collect the files of the upstream package by making a source distribution
  (
  cd "$PKGSRC"
  rm -f "dist-install/$PKG-*.tar.gz"
  cabal "${CMDPREFIX}sdist" --builddir=dist-install
  )
  # unpack the source distribution
  for SRCDISTTMP in $PKGSRC/dist-install/$PKG-*.tar.gz; do
    SRCDIST="$SRCDISTTMP"
  done
  (
  cd "$PKGDST"
  echo "unpacking sources: $SRCDIST"
  tar --strip 1 -xzf "$SRCDIST"
  )
  # apply patch if it exists
  if [ -f "$PKGPATCH" ]; then
    echo "patching package: $PKG"
    # unpack original sources again to pkg/upstream
    (
      mkdir -p "$PKGORIG"
      cd "$PKGORIG"
      tar --strip 1 -xzf "$SRCDIST"
    )
    apply_patch "$PKGDST" "$PKGPATCH"
  else
    echo "no patch for package: $PKG"
  fi
}

# copy a package from a list of files and directories
copy_patch_boot_package_list() {
  PKG="$1"
  LIST="$2"
  PKGSRC="$GHCSRC/libraries/$PKG"
  PKGDST="$TARGET/boot/pkg/$PKG"
  PKGORIG="$TARGET/upstream/pkg/$PKG"
  PKGPATCH="$PWD/patches/$PKG.patch"
  echo "copying package (file list): $PKG"

  # copy files
  (
    mkdir -p "$PKGDST"
    cd "$PKGDST"
    for FILE in $LIST; do
      # echo "copying file: $PKGSRC/$FILE"
      # echo "   $PWD"
      gnucp -a "$PKGSRC/$FILE" "."
    done
  )

  # apply patch if it exists
  if [ -f "$PKGPATCH" ]; then
    echo "patching package: $PKG"
    # copy original sources
    (
      mkdir -p "$PKGORIG"
      cd "$PKGORIG"
      for FILE in $LIST; do
        cp -a "$PKGSRC/$FILE" "."
      done
    )
    apply_patch "$PKGDST" "$PKGPATCH"
  else
    echo "no patch for package: $PKG"
  fi

}

# prepare the GHC source tree if needed
(
cd "$GHCSRC"
if [ ! -f ./configure ]; then
    echo "booting GHC tree"
    ./boot
fi

if [ ! -f ./compiler/ghc.cabal ]; then
    echo "configuring GHC tree"
    ./configure
fi

if [ ! -f ./libraries/time/lib/include/HsTimeConfig.h ]; then
    echo "configuring time package"
(
cd libraries/time
./configure
)
fi


# genprimopcode tool
if [ ! -f ./inplace/bin/genprimopcode ]
then
(
mkdir -p inplace/bin
cd utils/genprimopcode
cabal ${CMDPREFIX}build --builddir=dist
gnucp dist/build/genprimopcode/genprimopcode ../../inplace/bin
)
fi

# add cached files if needed
(
echo "processing cache"
cd "$PKGCACHE/ghc"
shopt -s globstar
for CACHED in ./**/*
do
    # echo "cached entry: $CACHED"
    if [ -f "$CACHED" ]
    then
        if [ -f "$GHCSRC/$CACHED" ]
        then
          # exists: update cached version
          gnucp "$GHCSRC/$CACHED" "$CACHED"
        else
          # does not exist: copy cached version into GHC tree
          mkdir -p "$GHCSRC/${CACHED%/*}"
          gnucp "$CACHED" "$GHCSRC/$CACHED"
        fi
    fi
done

)

)

# update the file lists below lists if files are added to GHC

# copy Haskell source files
(
cd $TARGET
TARGET=$PWD

###############################################################################
# Boot Packages

# clean packages first
rm -rf "boot/pkg"

mkdir -p "boot/pkg"

# copy packages by source distribution
for PKG in base array binary bytestring containers deepseq directory \
           filepath ghc-boot ghc-heap ghc-compact \
           ghc-boot-th ghci integer-gmp integer-simple \
           parallel pretty process stm template-haskell \
           mtl parsec text time transformers unix Win32; do
  copy_patch_boot_package_sdist "$PKG" ""
done

# Cabal has an additional level of nesting
copy_patch_boot_package_sdist "Cabal" "Cabal/"

# packages for which source distribution does not work
copy_patch_boot_package_list "ghc-prim" "changelog.md cbits/ GHC/ ghc-prim.cabal LICENSE Setup.hs tests/"

# GHCJS packages
(
cd "boot/pkg"
mkdir -p "ghcjs-prim"
(
cd "ghcjs-prim"
for DIR in cbits GHCJS; do
  copy_dir "$TARGET/ghcjs-prim" "$DIR"
done

for FILE in ghcjs-prim.cabal LICENSE Setup.hs; do
  copy_file "$TARGET/ghcjs-prim" "$FILE"
done
) # boot/pkg/ghcjs-prim

mkdir -p "ghcjs-th"
(
cd "ghcjs-th"
for DIR in  GHCJS; do
  copy_dir "$TARGET/ghcjs-th" "$DIR"
done

for FILE in ghcjs-th.cabal LICENSE Setup.hs; do
  copy_file "$TARGET/ghcjs-th" "$FILE"
done
) # boot/pkg/ghcjs-th

# generate stub ghc package, copy the version number from the ghc-boot package
mkdir -p "ghc"
(
cd "ghc"

for FILE in LICENSE; do
  copy_file "$TARGET/ghc" "$FILE"
done

for DIR in main; do
  copy_dir "$TARGET/ghc" "$DIR"
done

UPSTREAMGHC=`awk '$1 == "version:" { print $2 }' < "$TARGET/boot/pkg/ghc-boot/ghc-boot.cabal"`
echo "$PWD"
echo "upstream GHC version: ${UPSTREAMGHC}"
sed "s/@GhcVersion@/${UPSTREAMGHC}/g" "$TARGET/ghc/ghc.cabal.in" > "$TARGET/boot/pkg/ghc/ghc.cabal"
) # boot/pkg/ghc

) # boot/pkg

# we need to preserve symlinks for our cached npm packages
# store them in a separate tar file
(
tar -cvf "$TARGET/boot/ghcjs-node.tar" ghcjs-node/package.json ghcjs-node/LICENSE ghcjs-node/README.markdown ghcjs-node/node_modules
) # ghcjs-node

###############################################################################
# GHCJS dependencies

echo "creating GHCJS dependencies"

mkdir -p "ghc-api-ghcjs"
(
cd "ghc-api-ghcjs"


mkdir -p "compiler"
(
cd "compiler"

for DIR in backpack basicTypes cbits cmm codeGen coreSyn deSugar ghci hsSyn \
           iface llvmGen main nativeGen parser prelude profiling rename \
           simplCore simplStg specialise stgSyn stranal typecheck types \
           utils; do
  copy_dir "$GHCSRC/compiler" "$DIR"
done

for FILE in HsVersions.h Unique.h; do
  copy_file "$GHCSRC/compiler" "$FILE"
done
) # ghc-api-ghcjs/compiler

for FILE in LICENSE; do
  copy_file "$GHCSRC" "$FILE"
done

# copy includes
mkdir -p "includes"
(
cd "includes"

for DIR in rts stg; do
  copy_dir "$GHCSRC/includes" "$DIR"
done

for FILE in Cmm.h CodeGen.Platform.hs ghcautoconf.h ghcconfig.h ghcplatform.h \
            HsFFI.h MachDeps.h RtsAPI.h Rts.h Stg.h; do
  copy_file "$GHCSRC/includes" "$FILE"
done

# fixme, should we generate the constants?
for FILE in GHCConstantsHaskellType.hs GHCConstantsHaskellExports.hs \
            GHCConstantsHaskellWrappers.hs; do
  copy_file "$GHCSRC/includes/dist-derivedconstants/header" "$FILE"
done

) # ghc-api-ghcjs/includes

) # ghc-api-ghcjs


mkdir -p "template-haskell-ghcjs"
(
cd "template-haskell-ghcjs"

for FILE in changelog.md LICENSE Setup.hs; do
  copy_file "$GHCSRC/libraries/template-haskell" "$FILE"
done

for DIR in Language tests; do
  copy_dir "$GHCSRC/libraries/template-haskell" "$DIR"
done
) # template-haskell-ghcjs

# ghci
mkdir -p "ghci-ghcjs"
(
cd "ghci-ghcjs"

for FILE in changelog.md LICENSE SizedSeq.hs; do
  copy_file "$GHCSRC/libraries/ghci" "$FILE"
done

copy_dir "$GHCSRC/libraries/ghci" "GHCi"
) # ghci-ghcjs

# haddock-api-ghcjs
mkdir -p "haddock-api-ghcjs"
(
cd "haddock-api-ghcjs"

for FILE in CHANGES.md LICENSE Setup.lhs; do
  copy_file "$GHCSRC/utils/haddock/haddock-api" "$FILE"
done

for DIR in resources test; do
  copy_dir "$GHCSRC/utils/haddock/haddock-api" "$DIR"
done

mkdir -p "src"
(
cd "src"

for FILE in haddock.sh; do
  copy_file "$GHCSRC/utils/haddock/haddock-api/src" "$FILE"
done

copy_dir "$GHCSRC/utils/haddock/haddock-api/src" "Documentation"

mkdir -p "Haddock"
(
cd "Haddock"
for FILE in Convert.hs Doc.hs GhcUtils.hs InterfaceFile.hs Interface.hs ModuleTree.hs Options.hs Parser.hs Syb.hs Utils.hs; do
  copy_file "$GHCSRC/utils/haddock/haddock-api/src/Haddock" "$FILE"
done

for DIR in Backends Interface Utils; do
  copy_dir "$GHCSRC/utils/haddock/haddock-api/src/Haddock" "$DIR"
done
) # haddock-api-ghcjs/src/Haddock

) # haddock-api-ghcjs/src

) # haddock-api-ghcjs

# haddock-library-ghcjs
mkdir -p "haddock-library-ghcjs"
(
cd "haddock-library-ghcjs"
for FILE in LICENSE Setup.hs CHANGES.md; do
  copy_file "$GHCSRC/utils/haddock/haddock-library" "$FILE"
done

for DIR in src test fixtures; do
  copy_dir "$GHCSRC/utils/haddock/haddock-library" "$DIR"
done
)

# copy our own cabal and configuration files
echo "before apply_overrides"
echo "  $PWD"
apply_overrides "."

# generate primops for ghc-api-ghcjs and boot/pkg/ghc-prim
(
cd "ghc-api-ghcjs"

gcc -E -undef -traditional -P -Iincludes -x c "$GHCSRC/compiler/prelude/primops.txt.pp" | grep -v '^#pragma GCC' > "$WORKDIR/primops.txt"

(
cd "includes"
GENPRIM="$GHCSRC/inplace/bin/genprimopcode"

# ghc-api-ghcjs
$GENPRIM --data-decl          < "$WORKDIR/primops.txt" > primop-data-decl.hs-incl
$GENPRIM --primop-tag         < "$WORKDIR/primops.txt" > primop-tag.hs-incl
$GENPRIM --primop-list        < "$WORKDIR/primops.txt" > primop-list.hs-incl
$GENPRIM --has-side-effects   < "$WORKDIR/primops.txt" > primop-has-side-effects.hs-incl
$GENPRIM --out-of-line        < "$WORKDIR/primops.txt" > primop-out-of-line.hs-incl
$GENPRIM --commutable         < "$WORKDIR/primops.txt" > primop-commutable.hs-incl
$GENPRIM --code-size          < "$WORKDIR/primops.txt" > primop-code-size.hs-incl
$GENPRIM --can-fail           < "$WORKDIR/primops.txt" > primop-can-fail.hs-incl
$GENPRIM --strictness         < "$WORKDIR/primops.txt" > primop-strictness.hs-incl
$GENPRIM --fixity             < "$WORKDIR/primops.txt" > primop-fixity.hs-incl
$GENPRIM --primop-primop-info < "$WORKDIR/primops.txt" > primop-primop-info.hs-incl
$GENPRIM --primop-vector-uniques     < "$WORKDIR/primops.txt" > primop-vector-uniques.hs-incl
$GENPRIM --primop-vector-tys         < "$WORKDIR/primops.txt" > primop-vector-tys.hs-incl
$GENPRIM --primop-vector-tys-exports < "$WORKDIR/primops.txt" > primop-vector-tys-exports.hs-incl
$GENPRIM --primop-vector-tycons      < "$WORKDIR/primops.txt" > primop-vector-tycons.hs-incl

# boot/pkg/ghc-prim
$GENPRIM --make-haskell-source < "$WORKDIR/primops.txt" > "$TARGET/boot/data/Prim.hs"
$GENPRIM --make-haskell-wrappers < "$WORKDIR/primops.txt" > "$TARGET/boot/data/PrimopWrappers.hs"
gnucp "$WORKDIR/primops.txt" "$TARGET/boot/data/primops.txt"

) # ghc-api-ghcjs/includes
) # ghc-api-ghcjs

$SOURCEDIR/updateBootArchive.sh

)
