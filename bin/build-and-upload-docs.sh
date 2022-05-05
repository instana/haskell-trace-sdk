#!/bin/bash

# Taken from https://hackage.haskell.org/upload -> Notes

set -eo pipefail

cd `dirname $BASH_SOURCE`/..

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

dir=$(mktemp -d build-docs.XXXXXX)

# Don't even ask. For some reason, when building the Haddock docs, we need to
# change unsafeFdSocket to fdSocket. We will revert this change when we
# are done.
if [ $(uname -s) = "Darwin" ]; then
  sed -i '' 's/Socket.unsafeFdSocket socket/Socket.fdSocket socket/g' src/Instana/SDK/SDK.hs
else
  sed -i 's/Socket.unsafeFdSocket socket/Socket.fdSocket socket/g' src/Instana/SDK/SDK.hs
fi

cleanup () {
  rm -r "$dir"
  if [[ $(uname -s) = "Darwin" ]]; then
    sed -i '' 's/Socket.fdSocket socket/Socket.unsafeFdSocket socket/g' src/Instana/SDK/SDK.hs
  else
    sed -i 's/Socket.fdSocket socket/Socket.unsafeFdSocket socket/g' src/Instana/SDK/SDK.hs
  fi
}

trap cleanup EXIT

export PATH=$(stack path --bin-path)

# Only installing stack will not suffice because it does not have the cabal
# executable?!?
# Use https://www.haskell.org/ghcup/ to install everything you need.
ghc --version
cabal --version

cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

# check credentials in ~/.cabal/config if this fails with "Error: Username or password incorrect"
cabal upload -d --publish $dir/*-docs.tar.gz

if [ $(uname -s) = "Darwin" ]; then
  sed -i '' 's/Socket.fdSocket socket/Socket.unsafeFdSocket socket/g' src/Instana/SDK/SDK.hs
else
  sed -i 's/Socket.fdSocket socket/Socket.unsafeFdSocket socket/g' src/Instana/SDK/SDK.hs
fi

