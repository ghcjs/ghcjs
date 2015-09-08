#!/usr/bin/env bash
set -e

NODE="nodejs"
PART="$TEST_PART"
CABAL="cabal"
GHCJSBOOT="ghcjs-boot"
TESTRUNNER="./dist/build/test/test"

travis_boot() {
    case "$PART" in
        CORE1)
            ghcjs_boot -j2 --build-stage1-unbooted --no-prof
            cabal_install random QuickCheck stm syb
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
        ;;
        *)
            echo $"Unknown test part: $PART"
            exit 1
    esac
}

travis_test() {
    case "$PART" in
        CORE1)
            run_tests --no-profiling -t ghc -t conc -t integer
        ;;
        CORE2)
            run_tests --no-profiling -t pkg -t fay
        ;;
        PROFILING)
            run_tests -t profiling
        ;;
        GHCJS)
            run_tests --no-profiling -t ffi
        ;;
        *)
            echo $"Unknown test part: $PART"
            exit 1
    esac
}

ghcjs_boot() {
    "$GHCJSBOOT" --dev --no-haddock --with-node "$NODE" "$@" --ghcjs-boot-dev-repo=https://github.com/seereason/ghcjs-boot.git
}

cabal_install() {
    "$CABAL" install -j2 --ghcjs "$@"
}

run_tests() {
    "$TESTRUNNER" --travis --with-node="$NODE" "$@" -j2
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
