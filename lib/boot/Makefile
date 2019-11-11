# Expect the whole build process to have been completed until the `ghcjs-boot` process is started.

# We expect to have a `ghcjs` binary in `.bin` or somewhere. And a `ghc` binary for the host.

# enter some shell
# nix-shell --pure -p '[ stdenv ghc cabal-install which zlib zlib.dev git cacert haskellPackages.alex haskellPackages.happy bash autoconf automake less ]'

# Need this to sidestep booting restrictions in ghcjs
# export GHCJS_BOOTING=1
# need this as cabal otherwise defines this, but won't unless --ghcjs is provided.
# also ghcjs lies about it's target. And fails to claim it's a cross compiler!
# export CFLAGS="-D__GHCJS__ -Dghcjs_HOST_OS -I/p/iohk/ghcjs/lib/ghc-api-ghcjs/includes -I/p/iohk/ghcjs/lib/boot/data/include -I/usr/local/Cellar/gmp/6.1.2_2/include"

# Install some fake rts
# cat fake-rts.conf | ghc-pkg --global-package-db=inst/package.conf register -
# where fake-rts is
#  name: rts
#  version: 1.0
#  id: rts
#  key: rts
#  license: BSD-3-Clause
#  maintainer: glasgow-haskell-users@haskell.org
#  exposed: True

# For each library:
# 1. build the Setup.hs with $(HOST_HC)
# 2. $(HC_PKG) init inst/package.conf
# 3. run ./Setup configure --with-ghc=$(CROSS_HC) --disable-library-stripping --disable-executable-stripping --prefix=$PWD/inst --package-db=clear --package-db=$PWD/inst/package.conf
# 4. run ./Setup build
# 5. run ./Setup register --gen-pkg-config=$(PKG).conf
# 5. $(HC_PKG) --global-package-db=inst/package.conf register $(PKG).conf

HC_PKG:=ghc-pkg
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PKG_DB:=$(ROOT_DIR)/inst/package.conf
HOST_HC:=ghc
GHCJS_BOOTING=1

# We'll need those in commands we call.
# Make sure HOST_HC and HC are in the environment.
export HOST_HC
export HC
export GHCJS_BOOTING
export CFLAGS
#export CFLAGS="-D__GHCJS__ -Dghcjs_HOST_OS -I/p/iohk/ghcjs/lib/ghc-api-ghcjs/includes -I/p/iohk/ghcjs/lib/boot/data/include -I/usr/local/Cellar/gmp/6.1.2_2/include $(CFLAGS)"

inst/package.conf :
	$(HC_PKG) init $@

inst/package.conf/rts.conf : inst/package.conf
	cat fake-rts.conf | $(HC_PKG) --global-package-db=$(PKG_DB) register --force - || true

inst/package.conf/ghc-prim.conf : inst/package.conf/rts.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-prim \
		ghc-prim
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghc-prim/ghc-prim.conf

inst/package.conf/integer-gmp.conf : inst/package.conf/ghc-prim.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/integer-gmp \
		integer-gmp \
		--extra-include-dirs=/usr/local/Cellar/gmp/6.1.2_2/include
	# Need the extra-include-dirs, to ensure we find <gmp.h>

	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/integer-gmp/integer-gmp.conf

inst/package.conf/base.conf : inst/package.conf/integer-gmp.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/base \
		base \
		--extra-include-dirs=/p/iohk/ghcjs/lib/ghc-api-ghcjs/includes \
		--extra-include-dirs=I/p/iohk/ghcjs/lib/boot/data/include
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/base/base.conf

inst/package.conf/transformers.conf : inst/package.conf/base.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/transformers \
		transformers
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/transformers/transformers.conf

inst/package.conf/ghcjs-prim.conf : inst/package.conf/transformers.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghcjs-prim \
		ghcjs-prim
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghcjs-prim/ghcjs-prim.conf

inst/package.conf/array.conf : inst/package.conf/ghcjs-prim.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/array \
		array
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/array/array.conf

inst/package.conf/bytestring.conf : inst/package.conf/array.conf inst/package.conf/deepseq.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/bytestring \
		bytestring
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/bytestring/bytestring.conf

inst/package.conf/containers.conf : inst/package.conf/bytestring.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/containers \
		containers
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/containers/containers.conf

inst/package.conf/binary.conf : inst/package.conf/containers.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/binary \
		binary
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/binary/binary.conf

inst/package.conf/deepseq.conf : inst/package.conf/array.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/deepseq \
		deepseq
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/deepseq/deepseq.conf

inst/package.conf/directory.conf : inst/package.conf/filepath.conf inst/package.conf/time.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/directory \
		directory
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/directory/directory.conf

inst/package.conf/filepath.conf : inst/package.conf/base.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/filepath \
		filepath
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/filepath/filepath.conf

inst/package.conf/ghc.conf : inst/package.conf/ghc-boot.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghc \
		ghc
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghc/ghc.conf

inst/package.conf/ghc-boot.conf : inst/package.conf/directory.conf inst/package.conf/filepath.conf inst/package.conf/ghc-boot-th.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-boot \
		ghc-boot
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghc-boot/ghc-boot.conf

inst/package.conf/ghc-boot-th.conf : inst/package.conf/base.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-boot-th \
		ghc-boot-th
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghc-boot-th/ghc-boot-th.conf

inst/package.conf/ghc-compact.conf : inst/package.conf/bytestring.conf inst/package.conf/ghc-prim.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-compact \
		ghc-compact
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghc-compact/ghc-compact.conf

inst/package.conf/ghc-heap.conf : inst/package.conf/ghc-prim.conf inst/package.conf/base.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-heap \
		ghc-heap
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghc-heap/ghc-heap.conf

inst/package.conf/ghci.conf : inst/package.conf/template-haskell.conf inst/package.conf/transformers.conf inst/package.conf/filepath.conf inst/package.conf/ghc-boot.conf inst/package.conf/ghc-heap.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/ghci \
		ghci
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/ghci/ghci.conf

inst/package.conf/pretty.conf : inst/package.conf/deepseq.conf inst/package.conf/ghc-prim.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/pretty \
		pretty
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/pretty/pretty.conf

inst/package.conf/template-haskell.conf : inst/package.conf/ghc-boot-th.conf inst/package.conf/pretty.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/template-haskell \
		template-haskell
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/template-haskell/template-haskell.conf

inst/package.conf/parsec.conf : inst/package.conf/mtl.conf inst/package.conf/text.conf inst/package.conf/bytestring.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/parsec \
		parsec
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/parsec/parsec.conf

inst/package.conf/process.conf : inst/package.conf/directory.conf inst/package.conf/deepseq.conf inst/package.conf/ghcjs-prim.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/process \
		process
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/process/process.conf

inst/package.conf/time.conf : inst/package.conf/deepseq.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/time \
		time
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/time/time.conf

inst/package.conf/text.conf : inst/package.conf/deepseq.conf inst/package.conf/bytestring.conf inst/package.conf/array.conf inst/package.conf/ghc-prim.conf inst/package.conf/binary.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/text \
		text
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/text/text.conf

inst/package.conf/mtl.conf : inst/package.conf/transformers.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/mtl \
		mtl
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/mtl/mtl.conf

inst/package.conf/unix.conf : inst/package.conf/time.conf inst/package.conf/bytestring.conf
	./build-package.sh $(PKG_DB) \
		$(ROOT_DIR)/pkg/unix \
		unix
	$(HC_PKG) --global-package-db=$(PKG_DB) register $(ROOT_DIR)/build/unix/unix.conf

clean :
	rm -fR inst/package.conf