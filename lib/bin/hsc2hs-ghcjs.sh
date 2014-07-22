#!/bin/sh

# wrapper script to pass the correct options to hsc2hs.bin

executablename="{bindir}/hsc2hs-ghcjs-{version}-{ghcversion}.bin"
topdir="{topdir}"

exec "$executablename" ${1+"$@"}

