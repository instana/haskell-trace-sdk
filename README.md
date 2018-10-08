# Instana Haskell Trace SDK &nbsp; [![Build Status](https://travis-ci.org/instana/haskell-trace-sdk.svg?branch=master)](https://travis-ci.org/instana/haskell-trace-sdk)

Monitor your Haskell application with Instana! 🎉

Disclaimer
----------

The Instana Haskell Trace SDK is a labor of love from some of our engineers and work on it is done in their spare time. Haskell is currently not a platform that we officialy support. The experience may differ from other programming languages and platforms that Instana actively supports (such as Java, .NET, Node.js, Python, Ruby, Go, PHP, ...). That said, the SDK is a fully functional piece of software, so don't let this disclaimer discourage you from using it. If you use Instana or consider using it and Haskell support is crucial for you, make sure to give us a ping and let's talk about it.

What The Haskell Trace SDK Is And What It Is Not
------------------------------------------------

The Instana Haskell Trace SDK does not support automatic instrumentation/tracing in the way we support it in most other languages. Instead, the SDK enables you to create spans manually, much like the [Instana Trace SDK for Java](https://docs.instana.io/core_concepts/tracing/java_trace_sdk/) does. Besides offering a convenient API to create spans, the Haskell Trace SDK also takes care of establishing a connection to the Instana Agent and sending spans to the agent in an efficient way, that does not impede the performance of your production code.

Installation
------------

**NOTE: Currently, the `instana-haskell-trace-sdk` has not been published on Hackage yet, so adding it to your dependencies will not work yet. It will be published soon. Stay tuned!**

<s>To use the Instana Haskell Trace SDK in your application, add `instana-haskell-trace-sdk` to your dependencies (for example to the `build-depends` section of your cabal file).</s>

Usage
-----

### Initialization

Before using the SDK, you need to initialize it once, usually during application startup.

```
import qualified Instana.SDK.SDK as InstanaSDK

main :: IO ()
main = do
  -- ... initialize things ...

  -- initialize Instana
  instana <- InstanaSDK.initInstana

  -- ... initialize more things
```

The value `instana :: Instana.SDK.SDK.InstanaContext` that is returned by `InstanaSDK.initInstana` is required for all further calls, that is, for creating spans that will be send to the agent. The SDK will try to connect to an agent (asynchronous, in a a separate thread) as soon as it receives the `initInstana` call.

The SDK can be configured via environment variables or directly in the code by passing configuration parameters to the initialization function, or both.

If you would like to pass configuration parameters programatically, use `initConfiguredInstana` instead of `initInstana`:

```
import qualified Instana.SDK.SDK as InstanaSDK

main :: IO ()
main = do

  -- Example snippet for using the Instana SDK and providing a configuration
  -- (agent host, port, ...) directly in code. You only need to specify the
  -- configuration values you are interested in and can omit everything else
  -- (see https://www.yesodweb.com/book/settings-types).
  let
    config =
      InstanaSDK.defaultConfig
        { InstanaSDK.agentHost = Just "127.0.0.1"
        , InstanaSDK.agentPort = Just 42699
        , InstanaSDK.forceTransmissionAfter = Just 1000
        , InstanaSDK.forceTransmissionStartingAt = Just 500
        , InstanaSDK.maxBufferedSpans = Just 1000
        }
  instana <- InstanaSDK.initConfiguredInstana config
```

For configuration parameters that are omitted when creating the config record or are set to `Nothing`, the SDK will fall back to environment variables (see below) and then to default values.

There are also [bracket-style](https://wiki.haskell.org/Bracket_pattern) variants of the initialization function, called `withInstana` and `withConfiguredInstana`:

```
import qualified Instana.SDK.SDK as InstanaSDK

main :: IO ()
main = do
  InstanaSDK.withInstana runApp

runApp :: InstanaContext -> IO ()
runApp instana = do
  -- do your thing here :-)
```

or, with bracket style and a configuration record:

```
import qualified Instana.SDK.SDK as InstanaSDK

main :: IO ()
main = do
  let
    config =
      InstanaSDK.defaultConfig
        { InstanaSDK.agentHost = Just "127.0.0.1"
        , InstanaSDK.agentPort = Just 42699
        , InstanaSDK.forceTransmissionAfter = Just 1000
        , InstanaSDK.forceTransmissionStartingAt = Just 500
        , InstanaSDK.maxBufferedSpans = Just 1000
        }

  InstanaSDK.withConfiguredInstana config runApp

runApp :: InstanaContext -> IO ()
runApp instana = do
  -- do your thing here :-)
```

### Creating Spans

#### High Level API/Bracket Style

All functions starting with `with` accept (among other parameters) an IO action. The SDK will start a span before, then execute the given IO action and complete the span afterwards. Using this style is recommended over the low level API that requires you to start and complete spans yourself.

* `withRootEntrySimple`: Creates an entry span that is the root of a trace (it has no parent span). A convenience wrapper for `withRootEntry` that requires less parameters.
* `withRootEntry`: Creates an entry span that is the root of a trace (it has no parent span).
* `withEntrySimple`: Creates an entry span that has a parent span. A convenience wrapper for `withEntry` that requires less parameters.
* `withEntry`: Creates an entry span that has a parent span.
* `withExitSimple`: Creates an exit span. A convenience wrapper for `withExit` that requires less parameters.

* `withExit`: Creates an exit span.

#### Low Level API/Explicit Start And Complete

* `startRootEntry`: Starts an entry span that is the beginning of a trace (has no parent span). You need to call `completeEntry`/`completeEntryWithData` at some point with the partial entry span value that is returned by this function.

* `startRootEntryWithData`: Starts an entry span that is the beginning of a trace (has no parent span). You need to call `completeEntry`/`completeEntryWithData` at some point with the partial entry span value that is returned by this function.

* `startEntry`: Starts an entry span. You need to call `completeEntry`/`completeEntryWithData` at some point with the partial entry span value that is returned by this function.
* `startEntryWithData`:  Starts an entry span. You need to call `completeEntry`/`completeEntryWithData` at some point with the partial entry span value that is returned by this function.

* `startExit`: Starts an exit span. You need to call `completeExit`/`completeExitWithData` at some point with the partial exit span value that is returned by this function.

* `startExitWithData`: Starts an exit span. You need to call `completeExit`/`completeExitWithData` at some point with the partial exit span value that is returned by this function.


* `completeEntry`: Finalizes an entry span. This will put the span into the SDK's span buffer for transmission to the Instana agent.
* `completeEntryWithData`: Finalizes an entry span. This will put the span into the SDK's span buffer for transmission to the Instana agent.
* `completeExit`: Finalizes an exit span. This will put the span into the SDK's span buffer for transmission to the Instana agent.
* `completeExitWithData`: Finalizes an exit span. This will put the span into the SDK's span buffer for transmission to the Instana agent.

### Configuration Via Environment Variables

Instead of configuring the SDK programatically, as seen above, it can also be configured via environment variables:

* `INSTANA_AGENT_HOST`: The IP or the host of the Instana agent to connect to. Default: 127.0.0.1.
* `INSTANA_AGENT_PORT`: The port of the Instana agent to connect to. Default: 42699.
* `INSTANA_AGENT_NAME`: When establishing a connection to the Instana agent, the SDK validates the Instana agent's `Server` HTTP response header. Should you have changed the Server name on the agent side, you can use this environment variable to provide the name to match that header against.
* `INSTANA_FORCE_TRANSMISSION_STARTING_AFTER`: Spans are usually buffered before being transmitted to the agent. This setting forces the transmission of all buffered spans after the given amount of milliseconds. Default: 1000.
* `INSTANA_FORCE_TRANSMISSION_STARTING_AT`: This setting forces the transmission of all buffered spans when the given number of spans has been buffered.
* `INSTANA_MAX_BUFFERED_SPANS`: Limits the number of spans to buffer. When the limit is reached, spans will be dropped. This setting is a safe guard against memory leaks from buffering excessive amounts of spans. It must be larger than `INSTANA_FORCE_TRANSMISSION_STARTING_AT`.
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

If this is the case, you can simply use `INSTANA_LOG_LEVEL` (or `INSTANA_LOG_LEVEL_STDOUT`) without further configuration. If the app in question does not use hslogger, that is, if no `setHandler` call on `rootLoggerName` is executed, you should also set
`INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER` to a non-empty string (for example, `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true`).

#### Troubleshooting Tracing In Production

If your app already uses hslogger, use:

* `INSTANA_LOG_LEVEL=DEBUG`

Otherwise, use:

* `INSTANA_LOG_LEVEL=DEBUG INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true`

#### Development

During development (that is, when working on the Instana Haskell SDK), use either

* `INSTANA_LOG_LEVEL_STDOUT=DEBUG`

or

* `INSTANA_LOG_LEVEL_STDOUT=DEBUG`
* `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true`

depending on whether the application already uses and configures hslogger. The application `sample-app` that is contained in the Instana Haskell SDK's repo configures hslogger, so simply using `INSTANA_LOG_LEVEL_STDOUT=DEBUG` is correct.

Contributing
------------

See [CONTRIBUTING.md](https://github.com/instana/haskell-trace-sdk/blob/master/CONTRIBUTING.md).

