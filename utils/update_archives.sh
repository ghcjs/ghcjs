#!/bin/sh
###############################################################################
#
# build the sdist cache for shims, ghcjs-boot and the test suite
#
#   run from the toplevel dir of a GHCJS git repository
#
#   - the test suite is taken from the current working
#       tree (which must be clean)
#   - the shims/boot repositories are freshly cloned, set to the
#       latest commit on the branch with the same name as the GHCJS
#       working tree
#
# requires some tools to be in the PATH:
#    git, tar, autoreconf, alex, happy
#
# GHCJS has to be booted, since it is used to compile the Setup.hs scripts
# for the stage1 packages.
#
# ccjs is used to minify the precompiled Setup.hs scripts, install with
# npm -g closurecompiler
#
###############################################################################

set -e

SHIMS_ORIGIN=https://github.com/ghcjs/shims.git
BOOT_ORIGIN=https://github.com/ghcjs/ghcjs-boot.git

# file patters to ignore from the distribution archives
SHIMS_EXCLUDE="
shims/lib/closure-library/third_party
shims/lib/closure-library/*_test.html
shims/lib/closure-library/closure/goog/demos
.gitignore
.git
.gitmodules
.#*
*~
#*#
"

BOOT_EXCLUDE="
.gitignore
.git
.gitmodules
.hgignore
.hgtags
.#*
*~
#*#
"

TEST_EXCLUDE="
.gitignore
.git
.#*
*~
#*#
"

BUILDDIR=`mktemp -d ghcjs-archives.tmp.XXXXXX`

# prepare a dir of ghcjs-boot packages
prepare_packages() {
    echo "preparing packages: $1"
    ( cd "$1"
      for pkg in *;
      do
          [ -d "${pkg}" ] || continue
          echo "found package: ${pkg} in ${1}"
          (
              cd "$pkg"
              PWDDD=`pwd`
              echo "working dir: ${PWDDD}"
              if [ -f "../../patches/${pkg}.patch" ]
              then
                echo "patching package: ${pkg}"
                git apply "../../patches/${pkg}.patch"
              fi
              if [ -f "configure.ac" ]
              then
                 echo "generating autoconf scripts for: ${pkg}"
                 autoreconf
              else
                 echo "${pkg} does not use autoconf"
              fi
          )
      done
    )
    echo "done preparing packages: $1"
}

prepare_primops() {
    echo "preparing parser and lexer for the genprimopcode tool"
    ( cd "utils/genprimopcode"
      alex Lexer.x
      happy Parser.y
    )
} 

# this builds precompiled JavaScript for the Setup.hs scripts for the
# stage1 packages. ghc-prim is the only stage1 package that has a custom
# setup script, the other ones are either simple or autoconf
prepare_setup_scripts() {
  ( mkdir "setup-tmp"
    cd "setup-tmp"
    echo "import Distribution.Simple"                    >  setupSimple.hs
    echo "main = defaultMain"                            >> setupSimple.hs
    echo "import Distribution.Simple"                    >  setupAutoconf.hs
    echo "main = defaultMainWithHooks autoconfUserHooks" >> setupAutoconf.hs
    cp   ../boot/ghc-prim/Setup.hs setupGhcPrim.hs
    ghcjs -o setupSimple   -O setupSimple.hs
    ghcjs -o setupAutoconf -O setupAutoconf.hs
    ghcjs -o setupGhcPrim  -O setupGhcPrim.hs
    cp setupSimple.jsexe/all.js ../boot/SetupSimple.precompiled.js
    cp setupAutoconf.jsexe/all.js ../boot/SetupAutoconf.precompiled.js
    cp setupGhcPrim.jsexe/all.js ../boot/ghc-prim/Setup.hs.precompiled.js
    cd ..
    rm -r "setup-tmp"
  )
}

if [ -f ghcjs.cabal ] && [ -d .git ]
then
    echo "building sdist cache from GHCJS repository"
else
    echo "this doesn't look like a GHCJS repository, aborting"
    exit 1
fi

git update-index --assume-unchanged lib/cache/boot.tar
git update-index --assume-unchanged lib/cache/shims.tar
git update-index --assume-unchanged lib/cache/test.tar

# fixme this is actually not enough to ensure that there is no cruft in the
# test tree, since git may ignore files
STATUS=`git status --porcelain`
if [ ${#STATUS} -gt 0 ]
then
    echo "working tree is dirty, run from a clean working tree"
    exit 1
fi

# This is a simulation of git symbolic-ref --short -q HEAD, which doesn't work with
# older versions of git.  The --short option is supported by git-1.9.1.
if BRANCH=$(git symbolic-ref -q HEAD | sed 's:^.*/\([^/]*\)$:\1:')
then
    echo "using git branch: ${BRANCH}"
else
    echo "not on a git branch, aborting"
    exit 1
fi

echo "preparing boot and shims cache in ${BUILDDIR}"
( cd "${BUILDDIR}"
  # collect shims
  ( git clone "${SHIMS_ORIGIN}"
    cd shims
    git checkout "${BRANCH}"
    cd ..
    echo "${SHIMS_EXCLUDE}" > shims.exclude
    tar -X shims.exclude -cf shims.tar shims
  )
  
  # prepare ghcjs-boot repository
  ( git clone "${BOOT_ORIGIN}"
    cd ghcjs-boot
    git checkout "${BRANCH}"
    git submodule update --init --recursive
    prepare_packages "boot"
    
    prepare_primops
    prepare_setup_scripts
    cd ..
    echo "${BOOT_EXCLUDE}" > boot.exclude    
    tar -X boot.exclude -cf boot.tar ghcjs-boot
  )
)
echo "preparing test suite cache"
echo "${TEST_EXCLUDE}" > "${BUILDDIR}/test.exclude"
tar -X "${BUILDDIR}/test.exclude" -cf "${BUILDDIR}/test.tar" test

echo "copying cache files"
mkdir -p "lib/cache"
cp "${BUILDDIR}/boot.tar"  "lib/cache/boot.tar"
cp "${BUILDDIR}/shims.tar" "lib/cache/shims.tar"
cp "${BUILDDIR}/test.tar"  "lib/cache/test.tar"

echo "cleaning temporary files"
rm -rf "${BUILDDIR}"
