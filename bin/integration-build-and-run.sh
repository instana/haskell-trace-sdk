#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
stack build --flag instana-haskell-trace-sdk:dev instana-haskell-trace-sdk:instana-haskell-agent-stub
stack build --flag instana-haskell-trace-sdk:dev instana-haskell-trace-sdk:instana-haskell-trace-sdk-integration-tests

