#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
stack test --flag instana-haskell-trace-sdk:dev

