FROM ubuntu:22.04

## ensure locale is set during build
ENV LANG            C.UTF-8

## Haskell environment
RUN apt-get update && \
    # ghcup deps
    apt-get install -y \
        build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 \
        libnuma-dev && \
    # ghcjs deps
    apt-get install -y --no-install-recommends \
      happy \
      alex \
      zlib1g-dev \
      libtinfo-dev \
      libsqlite3-0 \
      libsqlite3-dev \
      ca-certificates \
      build-essential \
      libgmp-dev \
      autoconf \
      automake \
      curl \
      g++ \
      python3 \
      git
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7 \
    sh
ENV PATH /root/.ghcup/bin:$PATH

## llvm
RUN apt-get update && apt-get install -y llvm

## node.js
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs


## build GHCJS
WORKDIR /opt

RUN cabal update

ADD . ./ghcjs

RUN cd /opt/ghcjs && \
    git submodule update --init --recursive && \
    ./utils/makePackages.sh && \
    cabal install

RUN cd /opt/ghcjs && \
    ghcjs-boot -v2 -s ./lib/boot/

ENTRYPOINT ["ghcjs"]
