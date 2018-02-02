#!/usr/bin/env bash

# generic/hacky package cleaning

if [ $# -ne 1 ];
then
  echo "usage: $0 <dir>"
  echo ""
  echo "clean cabal package build artifacts from <dir>"
  exit 1
fi

(

cd "$1" || exit 1;
echo "cleaning: $PWD"

rm -rf dist dist-install autom4te.cache
rm -f  config.status config.log
rm -f  *.buildinfo
rm -f ghc.mk
rm -f gmp/config.mk gmp/ghc.mk gmp/gmpsrc.patch gmp/GNUmakefile
rm -f include/HsUnixConfig.h
rm -f include/HsIntegerGmp.h
rm -f include/HsBaseConfig.h
rm -f include/EventConfig.h
rm -f include/HsProcessConfig.h
rm -f HsDirectoryConfig.h
rm -f GHC/PrimopWrappers.hs
rm -f GHC/Prim.hs
)
