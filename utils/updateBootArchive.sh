#!/usr/bin/env bash

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
GHCJSROOT="$SOURCEDIR/.."
TARGET="$GHCJSROOT/lib"

(

cd "$TARGET"

echo "creating boot archive"
if [ -x "$(command -v gtar)" ]; then
  gtar --dereference --exclude-backups -X "$SOURCEDIR/updateBootArchive.tarExcludes" -cvf boot.tar boot
else
  tar --dereference --exclude-backups -X "$SOURCEDIR/updateBootArchive.tarExcludes" -cvf boot.tar boot
fi
rm -f "$TARGET/../data/boot.tar"
mv boot.tar "$TARGET/../data/boot.tar"

)
