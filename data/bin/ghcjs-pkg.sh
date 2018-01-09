#!/bin/sh

# wrapper script to pass the correct options to ghcjs-pkg.bin

executablename="{libexecdir}/ghcjs-pkg"
topdir="{topdir}"

PKGCONF="$topdir/package.conf.d"
exec "$executablename" --global-package-db "$PKGCONF" ${1+"$@"}
