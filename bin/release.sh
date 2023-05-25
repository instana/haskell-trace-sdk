#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..

command -v node >/dev/null 2>&1 || {
  cat <<EOF >&2
This script requires Node.js to be installed but it isn't.

Aborting.
EOF
  exit 1
}
command -v npm >/dev/null 2>&1 || {
  cat <<EOF >&2
This script requires npm to be installed but it isn't.

Aborting.
EOF
  exit 1
}
command -v grep >/dev/null 2>&1 || {
  cat <<EOF >&2
This script requires grep to be installed but it isn't.

Aborting.
EOF
  exit 1
}
command -v awk >/dev/null 2>&1 || {
  cat <<EOF >&2
This script requires awk to be installed but it isn't.

Aborting.
EOF
  exit 1
}
command -v sed >/dev/null 2>&1 || {
  cat <<EOF >&2
This script requires sed to be installed but it isn't.

Aborting.
EOF
  exit 1
}
command -v git >/dev/null 2>&1 || {
  cat <<EOF >&2
This script requires git to be installed but it isn't.

Aborting.
EOF
  exit 1
}

# TODO check for successful CircleCI build of current commit

if [[ -z $DRY_RUN ]]; then
  if [[ $(git branch --show-current) != main ]]; then
    echo "Not on branch main. Aborting release."
    exit 1
  fi

  if [[ -n "$(git log origin/main..main)" ]]; then
    echo "There are unpushed commits changes. Aborting release."
    exit 1
  fi

  if [[ -n "$(git status --porcelain)" ]]; then
    echo "There are uncommitted changes. Aborting release."
    exit 1
  fi
fi

pushd 2> /dev/null bin/release-util
npm install
popd 2> /dev/null

echo Checking if there are new commits that should be released.
if node bin/release-util needs-release; then
  echo Found releasable commits. Continuing with the release.
else
  echo No releasable commits since the last release. Aborting.
  exit 1
fi

current_version=$(grep "^version:" instana-haskell-trace-sdk.cabal | awk '{print $2}')
next_version=$(node bin/release-util next-version)

sed_i=(-i)
case "$(uname)" in
  # For macOS, use two parameters
  Darwin*) sed_i=(-i "")
esac

sed_script="s/$current_version/$next_version/g"

set -x
node bin/release-util update-changelog
sed "${sed_i[@]}" $sed_script README.md
sed "${sed_i[@]}" $sed_script instana-haskell-trace-sdk.cabal
sed "${sed_i[@]}" $sed_script package.yaml

echo
echo Committing and pushing the new version and tag.
if [[ -z DRY_RUN ]]; then
  git add README.md instana-haskell-trace-sdk.cabal package.yaml CHANGELOG.md
  git commit -m"chore: version $next_version"
  git tag v$next_version
  git push
  git push --tags
fi

echo
echo Preparing the distributable and uploading to hackage...
if [[ -z DRY_RUN ]]; then
  stack haddock && stack sdist && stack upload .
fi

echo
echo Waiting a minute to make sure the package has become available on hackage...
if [[ -z DRY_RUN ]]; then
  sleep 60
fi

echo
echo Uploading the documentation...
if [[ -z DRY_RUN ]]; then
  bin/build-and-upload-docs.sh
fi

echo Done

