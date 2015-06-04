FROM haskell:7.10

RUN cabal update

ADD moz.cabal /src/

WORKDIR /src

RUN cabal install --dependencies-only --enable-tests

ADD . /src
