Instana Haskell SDK
===================

Monitor your Haskell application with Instana.

Usage
-----

TODO Document usage: installation (cabal, ...), typical usage with code snippets, link to API docs

Configuration
-------------

TODO Document configuration mechanisms (via code versus via environment variables versus default values)

TODO Document environment variables.

* `INSTANA_AGENT_HOST`
* `INSTANA_AGENT_PORT`
* `INSTANA_FORCE_TRANSMISSION_STARTING_AFTER`
* `INSTANA_FORCE_TRANSMISSION_STARTING_AT`
* `INSTANA_MAX_BUFFERED_SPANS`
* `INSTANA_LOG_LEVEL`: See section "Configure Debug Logging".
* `INSTANA_LOG_LEVEL_STDOUT`: See section "Configure Debug Logging".
* `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER`: See section "Configure Debug Logging".


### Configure Debug Logging

If required, the Instana Haskell SDK can produce logs via [hslogger](http://hackage.haskell.org/package/hslogger). Under normal circumstances, the SDK does not emit any log output at all. It will only output log messages when logging is explicitly enabled via certain environment variables. This can be useful to troubleshoot tracing in production settings or during development.

#### Logging Related Environment Variables

* `INSTANA_LOG_LEVEL`: Sets the log level for SDK's log file. The log file will be written to the system's temporary directory (in particular, whatever `System.Directory.getTemporaryDirectory` returns) as `instana-haskell-sdk.{pid}.log`.
* `INSTANA_LOG_LEVEL_STDOUT`: Sets the level for emitting log messages to stdout.
* `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER`: Controls whether the SDK sets an hslogger at the root logger level, see below.

#### When To Set `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER`

Setting up hslogger correctly inside a library like the Instana Haskell SDK (as opposed to an application which has full control) can be tricky. For the Instana Haskell SDK to be able to correctly configure hslogger, it is important to know whether the app in question (or some part of it) already uses hslogger. In particular, does some part of the code set a handler on hslogger's root logger? Is a call like the following executed somewhere in the application?

```
import System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger)

updateGlobalLogger rootLoggerName $ setHandlers [...]
```

If this is the case, you can simply use `INSTANA_LOG_LEVEL` (or `INSTANA_LOG_LEVEL_STDOUT`) without further configuration. If the app in question does not use hslogger, that is if no `setHandler` call on `rootLoggerName` is executed, you should also set
`INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER` to a non-empty string (for example, `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true`).

#### Troubleshooting Tracing In Production

If your app already uses hslogger, use:

* `INSTANA_LOG_LEVEL=DEBUG`

Otherwise, use:

* `INSTANA_LOG_LEVEL=DEBUG INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true`

#### Development

During development (that is, working on the Instana Haskell SDK), use either

* `INSTANA_LOG_LEVEL_STDOUT=DEBUG`

or

* `INSTANA_LOG_LEVEL_STDOUT=DEBUG`
* `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true`

depending on whether the application already uses and configures hslogger. The application `sample-app` that is contained in the Instana Haskell SDK's repo confiugures hslogger, so simply using `INSTANA_LOG_LEVEL_STDOUT=DEBUG` is correct.

