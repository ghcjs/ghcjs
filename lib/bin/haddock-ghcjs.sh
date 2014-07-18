#!/bin/sh
exedir="{bindir}"
exeprog="haddock-ghcjs-{version}-{ghcversion}.bin"
executablename="$exedir/$exeprog"
topdir="{topdir}"

exec "$executablename" -B"$topdir" ${1+"$@"}
