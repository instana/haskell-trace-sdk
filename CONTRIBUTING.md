Contributing to the Instana Haskell Trace SDK
=============================================

We'd love for you to contribute to the SDK and to make it even better than it is today! Here are some pointers to get you started.

Setting Up a Local Development Environment
------------------------------------------

* Install stack: <https://docs.haskellstack.org/en/stable/install_and_upgrade/>
* `git clone https://github.com/instana/haskell-trace-sdk.git` (or create a fork on GitHub and clone your fork)
* `cd haskell-trace-sdk`
* `stack setup`
* `stack build`
* `stack exec instana-haskell-example-exe`

The command `stack setup` will download the appropriate version of the compiler (GHC) if necessary in an isolated location (default ~/.stack) that won't interfere with any system-level GHC installations. `stack build` will build the project and `stack exec instana-haskell-example-exe` will start the example application, which tries to connect to an Instana agent and send trace data to it.

After that, you can always use `stack build` to rebuild everything or use one of the convenience scripts located in the `bin` directory:

* `example-app-build-and-run.sh`: Builds everything and starts the example application (see below).
* `example-app-watch.sh`: Watches the source files, rebuilds everything and starts the example application if a file changes.
* `integration-build-and-run.sh`: Builds and runs the integration tests.
* `integration-watch.sh`: Watches the source files, rebuilds and starts the integrations tests if a file changes.
* `unit-watch.sh`: Watches the source files, rebuilds and starts the unit tests if a file changes.
* `all-tests.sh`: Runs the unit and integration tests.
* `agent-stub-build-and-run.sh`: Builds and runs the agent stub (see below).
* `agent-stub-watch.sh`: Watches the agent stub's source files, rebuilds and starts the agent stub if a file changes.

**All `watch` scripts require [entr](http://www.entrproject.org/) to be installed.**

You can also start the individual executables directly via stack:

* `stack exec instana-haskell-example-exe`
* `stack exec  instana-haskell-agent-stub`

Or you can run one of the test suites directly via stack:

* `stack test instana-haskell-trace-sdk:instana-haskell-trace-sdk-unit-tests`
* `stack build instana-haskell-trace-sdk:instana-haskell-trace-sdk-integration-tests`

Example Application
------------------

A trivial application that does nothing much except creating spans in an endless loop. It's behaviour depends on the environmentt variable `KEEP_ALIVE`. When `KEEP_ALIVE` is not set, the app will just run in an endless loop. When started with an explicit `KEEP_ALIVE=false stack exec instana-haskell-example-exe`, it will only run its loop once and then terminated. With `KEEP_ALIVE=60 stack exec instana-haskell-example-exe`, it will run its loop for 60 seconds before terminating. Note that you can also use this with the convenience scripts mentioned above, e.g., `KEEP_ALIVE=60 example-app-watch.sh`.

Agent Stub
----------

A Haskell app that emulates the Instana agent. It is used in the integration tests, but it can also be used to replace the Instana agent for testing purposes, for example, when adding Instana tracing to your application.

The agent stub can be configured with a number of environment variables:

* `HOST` (default: `127.0.0.1`): The bind address of the agent stub. `127.0.0.1` is usually fine, but you can also use for example `*4` to bind to any IPv4 or IPv6 hostname, IPv4 preferred. See <https://hackage.haskell.org/package/warp-3.2.9/docs/Network-Wai-Handler-Warp.html#t:HostPreference>.
* `PORT` (default: `1302`): The port to bind to.
* `STARTUP_DELAY` (default: `0`): An artificial startup delay in milliseconds, used for integration testing.
* `SIMULATE_CONNECTION_LOSS` (default: `false`): Used for integration testing.
* `SIMULATE_PID_TRANSLATION` (default: `false`): Used for integration testing.

Publishing a New Release
------------------------

* Make sure CHANGELOG.md has an entry for the upcoming release.
* Bump the version in:
    * `README.md` (installation instructions/extra-deps)
    * `instana-haskell-trace-sdk.cabal`
    * `package.yaml`
    * `test/integration/Instana/SDK/IntegrationTest/Metrics.hs` (assertion for `sensorVersion`)
* Update `README.md` with API changes.
* Update `CHANGELOG.md` (update/add entry)
* Commit and push this change with a commit comment like `chore: version a.b.c.d`
* Wait for the CI build for branch main.
* Build the package with stack and upload it to Hackage:
    * `stack haddock && stack sdist && stack upload .`

Actually, the Hackage server should build the haddock docs after the package has been uploaded and add it to the package version. You can check if this has worked - if the individual modules on <http://hackage.haskell.org/package/instana-haskell-trace-sdk> are links to the documentation, it worked. Legend has it that this takes a few minutes - you could theoretically wait to see if the docs show up. Most of the times this does not actually work for reasons unknown. In these cases you can build the docs manually in the format Hackage expects and upload them for an already published package version by running
```
bin/build-and-upload-docs.sh $YOUR_HACKAGE_USER
```
