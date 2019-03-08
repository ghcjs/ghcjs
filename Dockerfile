FROM ubuntu:18.04

## ensure locale is set during build
ENV LANG            C.UTF-8

## Haskell environment
RUN apt-get -yq update && \
    apt-get -yq --no-install-recommends install \
        build-essential \
        ca-certificates \
        software-properties-common \
        && \
    apt-add-repository -y "ppa:hvr/ghc" && \
    apt-get -yq update && \
    apt-get install -y --no-install-recommends \
        cabal-install-2.4 \
        ghc-8.6.4 \
        happy-1.19.5 \
        alex-3.1.7 \
        zlib1g-dev \
        libtinfo-dev \
        libsqlite3-0 \
        libsqlite3-dev \
        libgmp-dev \
        autoconf \
        automake \
        curl \
        g++ \
        python3 \
        gpg \
        gpg-agent \
        git \
        && \
    apt-get autoremove -y --purge && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/cabal/bin:/opt/ghc/8.6.4/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:$PATH

## node.js
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
    && apt-get install -y nodejs


## build GHCJS
WORKDIR /opt

RUN cabal update

ADD . ./ghcjs

RUN cd /opt/ghcjs && \
    ./utils/makePackages.sh link && \
    ./utils/makeSandbox.sh && \
    cabal install

ENV PATH /opt/ghcjs/.cabal-sandbox/bin:$PATH

RUN cd /opt/ghcjs && \
    ghcjs-boot -v2 -s ./lib/boot/

ENTRYPOINT ["ghcjs"]
