#!/bin/sh
exedir="{libexecdir}"
exeprog="haddock-ghcjs"
executablename="$exedir/$exeprog"
topdir="{topdir}"

exec "$executablename" -B"$topdir" ${1+"$@"}
