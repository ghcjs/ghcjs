#!/bin/sh

awk '
BEGIN {
  # Path to ghcjs executable.
  ghcjs = "../dist/build/ghcjs/ghcjs"

  # Path to ghc source distribution directory.
  ghc_dir = "'$1'"

  # Destintation directory to put results into.
  js_dir = "."
}
{
  package = $1
  print ghcjs, "-odir", js_dir "/" package, "-hidir", js_dir "/" package,
        "-package-name", package,
        "-XMagicHash -XExistentialQuantification -XRank2Types",
        "-XScopedTypeVariables -XUnboxedTuples -XForeignFunctionInterface",
        "-XUnliftedFFITypes -XDeriveDataTypeable -XGeneralizedNewtypeDeriving",
        "-XFlexibleInstances -XStandaloneDeriving -XPatternGuards",
        "-XEmptyDataDecls -XNoImplicitPrelude -XCPP",
        "-I" ghc_dir "/libraries/" package "/include",
        "-i" ghc_dir "/libraries/" package "/dist-install/build",
        "-i" ghc_dir "/libraries/" package,
        $2, $3, $4, $5, $6, $7, $8, $9, $10
}' <<END
ghc-prim GHC.Bool GHC.Debug GHC.Generics GHC.IntWord32 GHC.IntWord64 GHC.Ordering GHC.Tuple GHC.Types GHC.Unit
integer-simple GHC.Integer
base Prelude
END

