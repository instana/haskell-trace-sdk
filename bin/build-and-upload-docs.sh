#!/bin/bash
set -e

# Taken from https://hackage.haskell.org/upload -> Notes

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

dir=$(mktemp -d build-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

export PATH=$(stack path --bin-path)

# Only installing stack will not suffice because it does not have the cabal
# executable?!?
# Use https://www.haskell.org/ghcup/ to install everything you need.
ghc --version
cabal --version

cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload -d --publish $dir/*-docs.tar.gz

