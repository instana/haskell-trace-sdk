#!/usr/bin/env node

/*
 * (c) Copyright IBM Corp. 2023
 */

// Adapted from https://github.com/conventional-changelog/conventional-changelog/blob/58f0887283a200d52e49607baf7b352f26177b05/packages/conventional-recommended-bump/index.js.

'use strict';

const concat = require('concat-stream');
const conventionalCommitsParser = require('conventional-commits-parser');
const conventionalRecommendedBump = require('conventional-changelog-conventionalcommits/conventional-recommended-bump');
const fs = require('fs/promises');
const gitRawCommits = require('git-raw-commits');
const semverTags = require('./version-tags');
const yargs = require('yargs/yargs');

const cabalFileName = 'instana-haskell-trace-sdk.cabal';
const changelogFilename = 'CHANGELOG.md';
const commitLinkPrefix = 'https://github.com/instana/haskell-trace-sdk/commit/';

const typesThatTriggerARelease = [
  //
  'feat',
  'fix',
  'perf'
];

// Other types, that will not trigger a release:
// 'build',
// 'chore',
// 'ci',
// 'docs',
// 'style',
// 'refactor',
// 'test'

function needsRelease() {
  countReleaseWorthyCommits((err, numberOfRelevantCommits) => {
    if (err) {
      console.error(err);
      process.exit(1);
      return;
    }

    if (numberOfRelevantCommits > 0) {
      console.error(
        `There ${numberOfRelevantCommits > 1 ? 'were' : 'was'} ${numberOfRelevantCommits} commit${
          numberOfRelevantCommits > 1 ? 's' : ''
        } with a type from the list [${typesThatTriggerARelease.join(
          ', '
        )}], thus a new release will be published. Terminating with exit code 0.`
      );
      process.exit(0);
    } else {
      console.error(
        `There were no commits with a type from the list [${typesThatTriggerARelease.join(
          ', '
        )}], thus no new release will be published. Terminating with exit code 1.`
      );
      process.exit(1);
    }
  });
}

async function nextVersion(cb) {
  let version = [];
  const cabalFileHandle = await fs.open(cabalFileName);
  for await (const line of cabalFileHandle.readLines()) {
    if (line.startsWith('version')) {
      const match = /^version:\s*(\d+)\.(\d+)\.(\d+)\.(\d+).*$/.exec(line);
      if (match) {
        // see https://pvp.haskell.org/#version-numbers
        version[0] = parseInt(match[1], 10);
        version[1] = parseInt(match[2], 10);
        version[2] = parseInt(match[3], 10);
        version[3] = parseInt(match[4], 10);
      }
    }
  }
  if (version.length !== 4) {
    const msg = `Could not parse current version from cabal file: ${cabalFileName}`;
    if (typeof cb === 'function') {
      return cb(new Error(msg));
    } else {
      console.error(msg);
      process.exit(1);
    }
  }

  fetchReleaseWorthyCommits({ includeAllBreakingChanges: true }, (err, commits) => {
    if (err) {
      if (typeof cb === 'function') {
        return cb(err);
      } else {
        console.error(err);
        process.exit(1);
        return;
      }
    }

    const result = conventionalRecommendedBump({}).whatBump(commits);
    const { level } = result;

    // level 0: break
    // level 1: feat
    // level 2: patch
    if (level >= 0 && level <= 2) {
      version[level + 1]++;
      for (let i = level + 2; i < 3; i++) {
        version[i] = 0;
      }
      const next = version.join('.');
      if (typeof cb === 'function') {
        cb(null, next, commits);
      } else {
        console.log(next);
      }
    }
  });
}

function updateChangelog() {
  nextVersion(async (err, version, commits) => {
    if (err) {
      console.error(err);
      process.exit(1);
    }

    let changelogEntry = `## ${version}\n\n`;
    commits.forEach(commit => {
      changelogEntry += `- ${commit.header} ([commit](${commitLinkPrefix}${commit.hash}))\n`;
      for (let n = 0; n < commit.notes.length; n++) {
        changelogEntry += `  - ${commit.notes[n].title}: ${commit.notes[n].text}\n`;
      }
    });

    const changelog = await fs.readFile(changelogFilename, { encoding: 'utf8' });
    const changelogLines = changelog.split('\n');
    changelogLines.splice(2, 0, ...changelogEntry.split('\n'));
    const updatedChangelog = changelogLines.join('\n');
    await fs.writeFile(changelogFilename, updatedChangelog);
  });
}

function countReleaseWorthyCommits(cb) {
  fetchReleaseWorthyCommits({}, (err, commits) => {
    if (err) {
      return cb(err);
    }
    cb(null, commits.length);
  });
}

function fetchReleaseWorthyCommits({ includeAllBreakingChanges = false }, cb) {
  console.error('Fetching commits that should be released as a new version...');

  semverTags((err, tags) => {
    if (err) {
      return cb(err);
    }

    const commitStream = gitRawCommits({
      format: '%B%n-hash-%n%H',
      from: tags[0] || ''
    });
    commitStream.pipe(conventionalCommitsParser()).pipe(
      concat(data => {
        data = data.filter(commit => {
          return (
            typesThatTriggerARelease.includes(commit.type) || (includeAllBreakingChanges && commit.notes.length > 0)
          );
        });

        cb(null, data);
      })
    );
  });
}

const processArguments = process.argv.slice(2);

const parsedArgs = yargs(processArguments)
  .usage('Usage: $0 [options]')
  .command(
    'needs-release',
    'determine whether a release should be published (whether there are unreleased feat, fix or perf commits)',
    () => {
      needsRelease();
    }
  )
  .command(
    'next-version',
    'determine the next version number based on the commit messages since the last release',
    () => {
      nextVersion();
    }
  )
  .command(
    'update-changelog',
    'collect all relevant commits since the last release and update the changelog file',
    () => {
      updateChangelog();
    }
  )
  .demandCommand()
  .version(false);

const options = parsedArgs.argv;
