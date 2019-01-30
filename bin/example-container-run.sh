#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..

docker run -e "INSTANA_LOG_LEVEL_STDOUT=DEBUG" basti1302/instana-haskell-trace-sdk-example-exe /usr/local/bin/instana-haskell-example-exe
