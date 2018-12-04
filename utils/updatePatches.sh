#!/usr/bin/env bash

# update the patches for the boot packages
# (only existing patches are updated, do the first update by hand)

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

clean_package() {
  $SOURCEDIR/cleanPackage.sh "$PWD"
}

update_patch() {
  PKG=$1
  echo "updating patch for: $PKG"
  (
  # clean the package tree
  cd "$TARGET/lib/upstream/pkg/$PKG"
  clean_package
  cd "$TARGET/lib/boot/pkg/$PKG"
  clean_package
  cd "$TARGET/lib"
  diff -X "../utils/updatePatches.excludes" -Nru "upstream/pkg/$PKG" "boot/pkg/$PKG" > "$PATCHDIR/$PKG.patch"
  )
}

(
cd "$SOURCEDIR/.."
TARGET=$PWD
PATCHDIR="$TARGET/lib/patches"

(
cd "$PATCHDIR"

for PATCH in *.patch; do
  BPATCHE=${PATCH##*/}
  BPATCH=${BPATCHE%.*}
  update_patch "$BPATCH"
done
)

)
