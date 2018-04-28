#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
stack build
INSTANA_LOG_LEVEL_STDOUT=DEBUG stack exec instana-haskell-sample-exe

