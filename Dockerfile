FROM ubuntu:16.04

## ensure locale is set during build
ENV LANG            C.UTF-8

## Haskell environment
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > \
      /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      cabal-install-2.2 \
      ghc-8.4.2 \
      happy-1.19.5 \
      alex-3.1.7 \
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

ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/cabal/bin:/opt/ghc/8.4.2/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:$PATH

## node.js
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
    && apt-get install -y nodejs


## build GHCJS
WORKDIR /opt

RUN cabal update

ADD . ./ghcjs

RUN cd /opt/ghcjs && \
    ./utils/makePackages.sh && \
    ./utils/makeSandbox.sh && \
    cabal install

ENV PATH /opt/ghcjs/.cabal-sandbox/bin:$PATH

RUN cd /opt/ghcjs && \
    ghcjs-boot -v2 -s ./lib/boot/

ENTRYPOINT ["ghcjs"]
