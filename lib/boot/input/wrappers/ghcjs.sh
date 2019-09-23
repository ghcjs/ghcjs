#!/bin/sh

# wrapper script to pass the correct -B option to ghcjs

topdir="{topdir}"
executablename="{libexecdir}/ghcjs"
exec "$executablename" -B"$topdir" ${1+"$@"}
