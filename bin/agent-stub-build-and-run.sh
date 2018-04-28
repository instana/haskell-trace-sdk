#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
stack build instana-haskell-trace-sdk:instana-haskell-agent-stub
stack exec instana-haskell-agent-stub

