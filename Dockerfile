FROM haskell:7.8

ENV DEBIAN_FRONTENT non-interactive

ADD . ./ghcjs

RUN apt-get update \
    && apt-get -y install build-essential git zlib1g-dev libtinfo-dev libgmp-dev autoconf curl

RUN curl -sL https://deb.nodesource.com/setup | bash - \
    && apt-get install -y nodejs

ENV PATH /root/.cabal/bin:$PATH

RUN cabal update && \
    cabal install cabal-install && \
    cabal install Cabal

RUN echo $PATH && which cabal && cabal --version

RUN git clone https://github.com/ghcjs/ghcjs-prim.git && \
    cabal install --reorder-goals --max-backjumps=-1 ./ghcjs ./ghcjs-prim

RUN which ghcjs-pkg

RUN ghcjs-boot --dev
CMD ["/bin/bash"]


