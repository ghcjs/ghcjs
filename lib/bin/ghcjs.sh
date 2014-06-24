#!/bin/sh

# wrapper script to pass the correct -B option to ghcjs.bin
# ghcjs-boot does substitution of {...} sections

topdir="{topdir}"
executablename="{bindir}/ghcjs-{version}-{ghcversion}.bin"
exec "$executablename" -B"$topdir" ${1+"$@"}
