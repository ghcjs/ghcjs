#!/usr/bin/env bash
if [ $# -ne 2 ];
then
    echo "usage: updatePrimops version ghc-path"
    echo "   ghc-path must point to a GHC source/build directory (inplace must be populated)"
    exit 1;
fi
VERSION=$1
GHCPATH=$2
TMPDIR=`mktemp -d updatePrimops.XXXXXXXX`
`touch "${TMPDIR}/MachDeps.h"`
`touch "${TMPDIR}/ghc_boot_platform.h"`
`cpp -traditional -P -undef "-I${TMPDIR}" -DWORD_SIZE_IN_BITS=32 "${GHCPATH}/compiler/prelude/primops.txt.pp" > "primops-${VERSION}.txt"`
`rm -rf "${TMPDIR}"`
`${GHCPATH}/inplace/bin/genprimopcode --strictness < "primops-${VERSION}.txt" > "primop-strictness-${VERSION}.hs-incl"`
`${GHCPATH}/inplace/bin/genprimopcode --primop-primop-info < "primops-${VERSION}.txt" > "primop-primop-info-${VERSION}.hs-incl"`
