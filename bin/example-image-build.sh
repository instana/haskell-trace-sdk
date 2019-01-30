#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..

# This is required to build the Docker image of the example app on a
# non-Linux OS like MacOS. On Linux, you can omit
#   stack docker pull
# and build the image with
#   stack image container
# (that is, omitting the --docker argument).
#
# Reason: The Docker image needs to be build on the same architecture it runs
# on. So the --docker makes the Docker build happen in a Docker container
# (which is acquired by stack docker pull).

stack docker pull
docker build --tag basti1302/fpco-stack-run-extended docker/stack-run-extended
stack image container --docker
