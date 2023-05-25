// A modified version of
// https://github.com/conventional-changelog/conventional-changelog/tree/master/packages/git-semver-tags
// to deal with versions with four components.

'use strict';

const proc = require('process');
const exec = require('child_process').exec;
const tagRegex = /tag:\s*(.+?)[,)]/gi;
const versionRegex = /v\d+\.\d+\.\d+\.\d+/;
const cmd = 'git log --decorate --no-color';
const unstableTagTest = /.+-\w+\.\d+$/;

function lernaTag(tag, pkg) {
  if (pkg && !new RegExp('^' + pkg + '@').test(tag)) {
    return false;
  } else {
    return /^.+@[0-9]+\.[0-9]+\.[0-9]+(-.+)?$/.test(tag);
  }
}

module.exports = function gitSemverTags(opts, callback) {
  if (typeof opts === 'function') {
    callback = opts;
    opts = {};
  }
  const options = Object.assign({maxBuffer: Infinity, cwd: proc.cwd()}, opts);

  if (options.package && !options.lernaTags) {
    callback(
      new Error('opts.package should only be used when running in lerna mode'),
    );
    return;
  }

  exec(cmd, options, function (err, data) {
    if (err) {
      callback(err);
      return;
    }

    const tags = [];
    let tagPrefixRegexp;
    if (options.tagPrefix) {
      tagPrefixRegexp = new RegExp('^' + options.tagPrefix + '(.*)');
    }
    data.split('\n').forEach(function (decorations) {
      let match;
      while ((match = tagRegex.exec(decorations))) {
        const tag = match[1];

        if (options.skipUnstable && unstableTagTest.test(tag)) {
          // skip unstable tag
          continue;
        }

        if (versionRegex.test(tag)) {
          tags.push(tag);
        }
      }
    });

    callback(null, tags);
  });
};
