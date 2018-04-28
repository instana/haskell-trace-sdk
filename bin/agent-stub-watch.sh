#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
find test/agent-stub -iname \*\.hs | entr -r bin/agent-stub-build-and-run.sh

