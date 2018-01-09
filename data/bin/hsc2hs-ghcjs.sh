#!/bin/sh

# wrapper script to pass the correct options to hsc2hs

executablename="{libexecdir}/hsc2hs-ghcjs"
topdir="{topdir}"

exec "$executablename" ${1+"$@"}
