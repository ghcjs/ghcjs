#!/bin/bash
set -oue pipefail
# $1 - package db
# $2 - path to package
# $3 - package-name
# HC and HOST_HC are expected env vars.
PKG_DB=$1; shift
PKG=$1; shift
PKG_NAME=$1; shift

BUILD="${PWD}/build/${PKG_NAME}"
PREFIX="${PWD}/inst"
mkdir -p "${BUILD}"
mkdir -p "${PREFIX}"

SETUP_HS=exit

if [[ -f ${PKG}/Setup.hs ]];  then
    ${HOST_HC} -o ${BUILD}/Setup ${PKG}/Setup.hs
    SETUP_HS="${BUILD}/Setup"
elif [[ -f ${PKG}/Setup.lhs ]]; then
    ${HOST_HC} -o ${BUILD}/Setup ${PKG}/Setup.lhs
    SETUP_HS="${BUILD}/Setup"
else
    SETUP_HS=exit
fi

cd "$PKG"

sed -i 's/impl(ghcjs)/os(ghcjs)/g' *.cabal

# configure the package
CFLAGS="-I${PKG} ${CFLAGS}" \
    ${SETUP_HS} configure --with-ghc=${HC} \
	--disable-library-stripping \
	--disable-executable-stripping \
    --prefix="${PREFIX}" \
    --package-db=clear \
    --package-db="${PKG_DB}" \
    --builddir=${BUILD} \
    --ipid=${PKG_NAME} \
    --ghc-option=-Dghcjs_HOST_OS \
    $@

#cd "${BUILD}"
# build it.
CFLAGS="-I${PKG} ${CFLAGS}" \
    ${SETUP_HS} build --builddir=${BUILD} \
    --ghc-option=-Dghcjs_HOST_OS #\
#    --ghc-option=-v \
#    -v
# copy the lib, ...
${SETUP_HS} copy --builddir=${BUILD}
# and generate the pkg-config file.
${SETUP_HS} register --builddir=${BUILD} \
    --gen-pkg-config="${BUILD}/${PKG_NAME}.conf"