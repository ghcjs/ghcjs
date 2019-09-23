#!/usr/bin/env bash
export EMSDK="{emsdk}"
# export EMSDK_NODE=/Users/luite/haskell/iohk/emsdk/node/12.9.1_64bit/bin/node
# export EM_CONFIG=/Users/luite/.emscripten

executablename="{emsdk}/upstream/emscripten/emcc"
exec "$executablename" ${1+"$@"}
