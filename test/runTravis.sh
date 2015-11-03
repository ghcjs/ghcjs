#!/usr/bin/env bash
set -e

NODE="node"
PART="$TEST_PART"
CABAL="cabal"
GHCJSBOOT="ghcjs-boot"
TESTRUNNER="./dist/build/test/test"

travis_boot() {
    case "$PART" in
        CORE1)
            ghcjs_boot -j2 --build-stage1-unbooted --no-prof
            cabal_install random QuickCheck stm syb parsec parallel
        ;;
        CORE2)
            ghcjs_boot -j2 --build-stage1-unbooted --no-prof
            cabal_install random QuickCheck stm syb
        ;;
        PROFILING)
            ghcjs_boot -j2 --build-stage1-unbooted
        ;;
        GHCJS)
            ghcjs_boot -j1 --no-prof
            cabal_install random
        ;;
        *)
            echo $"Unknown test part: $PART"
            exit 1
    esac
}

travis_test() {
    case "$PART" in
        CORE1)
            run_tests --no-profiling -t ghc -t integer
        ;;
        CORE2)
            run_tests --no-profiling -t pkg -t fay
        ;;
        PROFILING)
            run_tests -t profiling
        ;;
        GHCJS)
            run_tests --no-profiling -t ffi -t conc
        ;;
        *)
            echo $"Unknown test part: $PART"
            exit 1
    esac
}

ghcjs_boot() {
    "$GHCJSBOOT" --dev --ghcjs-boot-dev-branch "$TRAVIS_BRANCH" --shims-dev-branch "$TRAVIS_BRANCH" --no-haddock --with-node "$NODE" "$@"
}

cabal_install() {
    "$CABAL" install -j2 --ghcjs "$@"
}

run_tests() {
    "$TESTRUNNER" --travis --with-spidermonkey=none --with-javascriptcore=none --with-node="$NODE" "$@" -j2
}

case "$1" in
    boot)
      travis_boot
      ;;
    "test")
      travis_test
      ;;
    *)
      echo $"Usage: $0 {boot|test}"
      exit 1
esac
