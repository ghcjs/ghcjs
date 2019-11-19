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
# cat fake-rts.conf | ghc-pkg --global-package-db=$(FULL_PKG_DB) register -
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
# 2. $(HC_PKG) init $(PKG_DB)
# 3. run ./Setup configure --with-ghc=$(CROSS_HC) --disable-library-stripping --disable-executable-stripping --prefix=$PWD/inst --package-db=clear --package-db=$PWD/$(PKG_DB)
# 4. run ./Setup build
# 5. run ./Setup register --gen-pkg-config=$(PKG).conf
# 5. $(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(PKG).conf

HC_PKG:=ghc-pkg
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
GHC_LIB_DIR:=$(shell ghc --info | runghc Settings.hs LibDir)
PKG_DB:=inst/lib/package.conf.d
FULL_PKG_DB:=$(ROOT_DIR)/inst/lib/package.conf.d
HOST_HC:=ghc
GHCJS_BOOTING=1

# We'll need those in commands we call.
# Make sure HOST_HC and HC are in the environment.
export HOST_HC
export HC
export GHCJS_BOOTING
export CFLAGS
#export CFLAGS="-D__GHCJS__ -Dghcjs_HOST_OS -I/p/iohk/ghcjs/lib/ghc-api-ghcjs/includes -I/p/iohk/ghcjs/lib/boot/data/include -I/usr/local/Cellar/gmp/6.1.2_2/include $(CFLAGS)"

.PHONY: inst/lib

# This frankensteins lots of includes together. It's by far the most messy stuff
# ever.
inst/lib :
	mkdir -p inst/lib/include
	mkdir -p inst/lib/include_native
	# cp -r ghc --info `LibDir`/include -> include_native
	cp -r shims/. inst/lib
	# copy over settings, llvm-passes and llvm-targets from the
	# ghc source. (Note: settings us usually created from settings.in)
	cp -f ../../ghc/settings inst/lib/
	cp -f ../../ghc/llvm-passes inst/lib
	cp -f ../../ghc/llvm-targets inst/lib

	# pretend that platformConstants are identical to those from the build.
	# They are not really, but we'll get away with it.
	cp -f "$(GHC_LIB_DIR)/platformConstants" inst/lib

	# copy over all the data include stuff we have.
	cp -r data/include/. inst/lib/include
	# clone the include_native from the build compiler. This will
	# be configured for the wrong architecture... but ohh well..
#	cp -f -r "$(GHC_LIB_DIR)/include/." inst/lib/include_native || true
	# These should all be part of the `rts`'s includes.
	cp -f -r ../../ghc/includes/Cmm.h \
		../../ghc/includes/HsFFI.h \
		../../ghc/includes/MachDeps.h \
		../../ghc/includes/Rts.h \
		../../ghc/includes/RtsAPI.h \
		../../ghc/includes/Stg.h \
		../../ghc/includes/ghcconfig.h \
		inst/lib/include
	cp -f -r inst/lib/include_native/stg inst/lib/include || true
	mkdir -p inst/lib/include/rts
	cp -f -r ../../ghc/rts/*.h inst/lib/include/rts/
	cp -f -r ../../ghc/includes/rts/* inst/lib/include/rts/

	# js support code
	cp -r shims inst/lib/
	cp data/runmain.js inst/lib/
	cp data/template.html inst/lib/
	cp data/manifest.webapp inst/lib/

	echo $(GHC_LIB_DIR)

inst/lib/ghcjs_boot.completed :
	touch inst/lib/ghcjs_boot.completed

inst/lib/wiredinkeys.yaml :
	echo "ghcjs-prim: ghcjs-prim" > inst/lib/wiredinkeys.yaml
	echo "ghcjs-th: ghcjs-th" >> inst/lib/wiredinkeys.yaml

$(PKG_DB) :
	$(HC_PKG) init $@

$(PKG_DB)/rts.conf : $(PKG_DB)
	cat fake-rts.conf | $(HC_PKG) --global-package-db=$(FULL_PKG_DB) register --force - || true

$(PKG_DB)/ghc-prim.conf : $(PKG_DB)/rts.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-prim \
		ghc-prim
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghc-prim/ghc-prim.conf

$(PKG_DB)/integer-gmp.conf : $(PKG_DB)/ghc-prim.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/integer-gmp \
		integer-gmp \
		--extra-include-dirs=/usr/local/Cellar/gmp/6.1.2_2/include
	# Need the extra-include-dirs, to ensure we find <gmp.h>

	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/integer-gmp/integer-gmp.conf

$(PKG_DB)/base.conf : $(PKG_DB)/integer-gmp.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/base \
		base \
		--extra-include-dirs=/p/iohk/ghcjs/lib/ghc-api-ghcjs/includes \
		--extra-include-dirs=I/p/iohk/ghcjs/lib/boot/data/include
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/base/base.conf

$(PKG_DB)/transformers.conf : $(PKG_DB)/base.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/transformers \
		transformers
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/transformers/transformers.conf

$(PKG_DB)/ghcjs-prim.conf : $(PKG_DB)/transformers.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghcjs-prim \
		ghcjs-prim
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghcjs-prim/ghcjs-prim.conf

$(PKG_DB)/array.conf : $(PKG_DB)/ghcjs-prim.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/array \
		array
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/array/array.conf

$(PKG_DB)/bytestring.conf : $(PKG_DB)/array.conf $(PKG_DB)/deepseq.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/bytestring \
		bytestring
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/bytestring/bytestring.conf

$(PKG_DB)/containers.conf : $(PKG_DB)/bytestring.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/containers \
		containers
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/containers/containers.conf

$(PKG_DB)/binary.conf : $(PKG_DB)/containers.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/binary \
		binary
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/binary/binary.conf

$(PKG_DB)/deepseq.conf : $(PKG_DB)/array.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/deepseq \
		deepseq
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/deepseq/deepseq.conf

$(PKG_DB)/directory.conf : $(PKG_DB)/filepath.conf $(PKG_DB)/time.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/directory \
		directory
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/directory/directory.conf

$(PKG_DB)/filepath.conf : $(PKG_DB)/base.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/filepath \
		filepath
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/filepath/filepath.conf

$(PKG_DB)/ghc.conf : $(PKG_DB)/ghc-boot.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghc \
		ghc
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghc/ghc.conf

$(PKG_DB)/ghc-boot.conf : $(PKG_DB)/directory.conf $(PKG_DB)/filepath.conf $(PKG_DB)/ghc-boot-th.conf $(PKG_DB)/binary.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-boot \
		ghc-boot
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghc-boot/ghc-boot.conf

$(PKG_DB)/ghc-boot-th.conf : $(PKG_DB)/base.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-boot-th \
		ghc-boot-th
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghc-boot-th/ghc-boot-th.conf

$(PKG_DB)/ghc-compact.conf : $(PKG_DB)/bytestring.conf $(PKG_DB)/ghc-prim.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-compact \
		ghc-compact
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghc-compact/ghc-compact.conf

$(PKG_DB)/ghc-heap.conf : $(PKG_DB)/ghc-prim.conf $(PKG_DB)/base.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghc-heap \
		ghc-heap
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghc-heap/ghc-heap.conf

$(PKG_DB)/ghci.conf : $(PKG_DB)/template-haskell.conf $(PKG_DB)/transformers.conf $(PKG_DB)/filepath.conf $(PKG_DB)/ghc-boot.conf $(PKG_DB)/ghc-heap.conf $(PKG_DB)/unix.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghci \
		ghci
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghci/ghci.conf

$(PKG_DB)/pretty.conf : $(PKG_DB)/deepseq.conf $(PKG_DB)/ghc-prim.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/pretty \
		pretty
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/pretty/pretty.conf

$(PKG_DB)/template-haskell.conf : $(PKG_DB)/ghc-boot-th.conf $(PKG_DB)/pretty.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/template-haskell \
		template-haskell
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/template-haskell/template-haskell.conf

$(PKG_DB)/parsec.conf : $(PKG_DB)/mtl.conf $(PKG_DB)/text.conf $(PKG_DB)/bytestring.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/parsec \
		parsec
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/parsec/parsec.conf

$(PKG_DB)/process.conf : $(PKG_DB)/directory.conf $(PKG_DB)/deepseq.conf $(PKG_DB)/ghcjs-prim.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/process \
		process
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/process/process.conf

$(PKG_DB)/time.conf : $(PKG_DB)/deepseq.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/time \
		time
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/time/time.conf

$(PKG_DB)/text.conf : $(PKG_DB)/deepseq.conf $(PKG_DB)/bytestring.conf $(PKG_DB)/array.conf $(PKG_DB)/ghc-prim.conf $(PKG_DB)/binary.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/text \
		text
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/text/text.conf

$(PKG_DB)/mtl.conf : $(PKG_DB)/transformers.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/mtl \
		mtl
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/mtl/mtl.conf

$(PKG_DB)/unix.conf : $(PKG_DB)/time.conf $(PKG_DB)/bytestring.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/unix \
		unix
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/unix/unix.conf

$(PKG_DB)/ghcjs-th.conf : $(PKG_DB)/template-haskell.conf $(PKG_DB)/ghci.conf
	./build-package.sh $(FULL_PKG_DB) \
		$(ROOT_DIR)/pkg/ghcjs-th \
		ghcjs-th
	$(HC_PKG) --global-package-db=$(FULL_PKG_DB) register $(ROOT_DIR)/build/ghcjs-th/ghcjs-th.conf


clean :
	rm -fR $(PKG_DB)