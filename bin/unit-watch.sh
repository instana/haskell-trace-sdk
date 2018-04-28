#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
find . -iname \*\.hs -o -iname \*\.yaml -o -iname \*\.cabal | entr stack test instana-haskell-trace-sdk:instana-haskell-trace-sdk-unit-tests

