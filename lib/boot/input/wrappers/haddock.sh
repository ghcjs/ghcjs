#!/bin/sh
exedir="{libexecdir}"
exeprog="haddock"
executablename="$exedir/$exeprog"
topdir="{topdir}"

exec "$executablename" -B"$topdir" -l"$topdir" ${1+"$@"}
