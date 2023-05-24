# Instana Haskell Trace SDK &nbsp; [![Build Status](https://circleci.com/gh/instana/haskell-trace-sdk.svg?style=svg)](https://circleci.com/gh/instana/haskell-trace-sdk)


Monitor your Haskell application with [Instana](https://www.instana.com/)! 🎉

What The Haskell Trace SDK Is And What It Is Not
------------------------------------------------

The Instana Haskell Trace SDK does not support automatic instrumentation/tracing in the way we support it in most other languages. Instead, the SDK enables you to create spans manually, much like the [Instana Trace SDK for Java](https://docs.instana.io/core_concepts/tracing/java_trace_sdk/) does. Besides offering a convenient API to create spans, the Haskell Trace SDK also takes care of establishing a connection to the Instana Agent and sending spans to the agent in an efficient way, that does not impede the performance of your production code. Last but not least, it collects runtime metrics and reports them to Instana.

Installation
------------

To use the Instana Haskell Trace SDK in your application, add `instana-haskell-trace-sdk` to your dependencies (for example to the `build-depends` section of your cabal file). If you are using [stack](https://docs.haskellstack.org/en/stable/README/) you might need to add the SDK (with the version number you want to use) to the `extra-deps` section in your `stack.yaml` file:

```
extra-deps:
- instana-haskell-trace-sdk-0.10.2.0
```

Depending on the stack resolver you use, you might also need to add `aeson-extra` to your extra-deps:

```
- aeson-extra-0.4.1.3@sha256:8ad8e4d28e46280ea98af2e94dcaaf524c396c9d91b1eba867961efc6e7f923f,2997
```

Usage
-----

<i>Example app: Take a look at the [Monad Shop](https://github.com/instana/monad-shop) To see the Instana Haskell Trace SDK in action.</i>

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

The value `instana :: Instana.SDK.InstanaContext` that is returned by `InstanaSDK.initInstana` is required for all further calls, that is, for creating spans that will be send to the agent. The SDK will try to connect to an agent (asynchronous, in a a separate thread) as soon as it receives the `initInstana` call.

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
        , InstanaSDK.serviceName = Just "A Great Hakell Service"
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
        , InstanaSDK.serviceName = Just "A Great Hakell Service"
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

#### Trace HTTP Entries Automatically

You can let the SDK automatically create entry spans for all incoming HTTP requests in a WAI application by using it as a WAI middleware plug-in. Instana tracing headers as well as W3C trace context headers are automatically taken into account by the plug-in. Note that exit spans still need to be created manually via the `withHttpExit` or `startHttpExit`/`completeExit` functions (see below).

The middleware plug-in will also add an additional HTTP response header (`Server-Timing`) to HTTP responses. This header enables website monitoring back end correlation.

```
import qualified Instana.Wai.Middleware.Entry as InstanaWaiMiddleware

main = do
  Warp.run 3000 $ InstanaWaiMiddleware.traceHttpEntries instana $ app
```

#### Bracket Style (High Level API)

All functions starting with `with` accept (among other parameters) an IO action. The SDK will start a span before, then execute the given IO action and complete the span afterwards. Using this style is recommended over the low level API that requires you to start and complete spans yourself.

* `withRootEntry`: Creates an entry span that is the root of a trace (it has no parent span).
* `withEntry`: Creates an entry span that has a parent span.
* `withHttpEntry`: A convenience function that examines an incoming HTTP request for Instana tracing headers or W3C trace context headers and creates an entry span. It will automatically add the correct metadata to the span. Note that you do not need to handle incoming HTTP requests at all when using the Instana WAI middleware plug-in (see above).
* `withHttpEntry_`: A variant of `withHttpEntry` with a more general type signature, but less features. It will automatically continue the trace from incoming headers (Instana headers or W3C trace context) just like `withHttpEntry` does, but it will not capture the status code of the HTTP response or add the response header for website monitoring back end correlation (Server-Timing). It is recommended to use `withHttpEntry` instead of this function, if possible. Alternatively, you can also call `postProcessHttpResponse` inside the `withHttpEntry_` block to cover the two missing features mentioned above. Note that you do not need to handle incoming HTTP requests at all when using the Instana WAI middleware plug-in.
* `withExit`: Creates an exit span. This can only be called inside a `withRootEntry` or an `withEntry` call, as an exit span needs an entry span as its parent.
* `withHttpExit`: Creates an exit span for a given HTTP client request. It will automatically add the correct metadata to the span so it should be preferred to `withExit` when tracing outgoing HTTP requests. It will also add Instana tracing HTTP headers as well as W3C trace context HTTP headers HTTP to the request to propagate the trace context downstream.
* `postProcessHttpResponse`: Processes the response of an HTTP entry. This function needs be called while the HTTP entry span is still active. It can be used inside a `withHttpEntry_` block or between `startHttpEntry` and `completeEntry`. This function accomplishes two things: It captures the HTTP status code from the response and adds it as an annotation to the currently active span. It also adds an additional HTTP response header (Server-Timing) to the given HTTP response that enables website monitoring back end correlation. Client code should rarely have the need to call this directly. Instead, capture incoming HTTP requests with `withHttpEntry`, which does both of these things automatically.

#### Low Level API/Explicit Start And Complete

* `startRootEntry`: Starts an entry span that is the beginning of a trace (has no parent span). You will need to call `completeEntry` at some point.
* `startEntry`: Starts an entry span. You will need to call `completeEntry` at some point.
* `startHttpEntry`: Starts an entry span for an incoming HTTP request. It will automatically add the correct metadata to the span. You do not need to handle incoming HTTP requests at all when using the WAI middleware plug-in (see above). You will need to call `completeEntry` at some point.
* `startExit`: Starts an exit span. You will need to call `completeExit` at some point.
* `startHttpExit`: Starts an exit span for an outgoing HTTP request. It will automatically add the correct metadata to the span so it should be preferred to `startExit` when tracing outgoing HTTP requests. It will also add HTTP headers to the request to propagate the trace context downstream. You will need to call `completeExit` at some point.
* `completeEntry`: Finalizes an entry span. This will put the span into the SDK's span buffer for transmission to the Instana agent.
* `completeExit`: Finalizes an exit span. This will put the span into the SDK's span buffer for transmission to the Instana agent.

#### Best Practices

Make sure you have read Instana's [docs on custom tracing](https://docs.instana.io/quick_start/custom_tracing/#tips--best-practices) and in particular the [best practices section](https://docs.instana.io/quick_start/custom_tracing/#tips--best-practices). This documentation contains a lot of useful info for integrating Instana tracing into your code; among other things, it explains which [annotations](https://docs.instana.io/quick_start/custom_tracing/#list-of-processed-tags) can be added to spans (via `InstanaSDK.addAnnotation`, `InstanaSDK.addAnnotationAt`, `InstanaSDK.addAnnotationValueAt`, and `InstanaSDK.addJsonValueAt`).

Instana differentiates between so-called registered spans and SDK spans. Registered spans are usually created by automatic tracing and there is specialized handling for each registered in Instana's processing pipeline. SDK spans, in contrast, are the type of spans created by using a trace SDK (like the Haskell trace SDK or other, similar SDKs for other runtime platforms). SDK span are processed in a more generic fashion by Instana's processing pipeline.

Note that nearly all spans created with this SDK should be SDK spans. The are only two exceptions, for which this SDK creates registered spans:
- HTTP/WAI entry (server) spans, and
- HTTP exit (client) spans.

The SDK offers special functions to create these registered spans (`withHttpEntry`, `withHttpExit` as well as the corresponding lower level functions `startHttpEntry` and `startHttpExit`, see above).

### Configuration Via Environment Variables

Instead of configuring the SDK programatically, as seen above, it can also be configured via environment variables:

* `INSTANA_AGENT_HOST`: The IP or the host of the Instana agent to connect to. Default: 127.0.0.1.
* `INSTANA_AGENT_PORT`: The port of the Instana agent to connect to. Default: 42699.
* `INSTANA_SERVICE_NAME`: Override the default service name in Instana.
* `INSTANA_FORCE_TRANSMISSION_STARTING_AFTER`: Spans are usually buffered before being transmitted to the agent. This setting forces the transmission of all buffered spans after the given amount of milliseconds. Default: 1000.
* `INSTANA_FORCE_TRANSMISSION_STARTING_AT`: This setting forces the transmission of all buffered spans when the given number of spans has been buffered.
* `INSTANA_MAX_BUFFERED_SPANS`: Limits the number of spans to buffer. When the limit is reached, spans will be dropped. This setting is a safe guard against memory leaks from buffering excessive amounts of spans. It must be larger than `INSTANA_FORCE_TRANSMISSION_STARTING_AT`.
* `INSTANA_LOG_LEVEL`: See section "Configure Debug Logging".
* `INSTANA_LOG_LEVEL_STDOUT`: See section "Configure Debug Logging".
* `INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER`: See section "Configure Debug Logging".

### Configure Debug Logging

If required, the Instana Haskell SDK can produce logs via [hslogger](http://hackage.haskell.org/package/hslogger). Under normal circumstances, the SDK does not emit any log output at all. It will only output log messages when logging is explicitly enabled via certain environment variables. This can be useful to troubleshoot tracing in production settings or during development.

#### Logging Related Environment Variables

* `INSTANA_LOG_LEVEL`: Sets the log level for the SDK's log file. The log file will be written to the system's temporary directory (in particular, whatever `System.Directory.getTemporaryDirectory` returns) as `instana-haskell-sdk.{pid}.log`. All [log priorities from hslogger](http://hackage.haskell.org/package/hslogger-1.3.1.0/docs/System-Log-Logger.html#t:Priority) can be used, case insensitive.
* `INSTANA_LOG_LEVEL_STDOUT`: Sets the level for emitting log messages to stdout. All [log priorities from hslogger](http://hackage.haskell.org/package/hslogger-1.3.1.0/docs/System-Log-Logger.html#t:Priority) can be used, case insensitive.
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

depending on whether the application already uses and configures hslogger. The application `example-app` that is contained in the Instana Haskell SDK's repo configures hslogger, so simply using `INSTANA_LOG_LEVEL_STDOUT=DEBUG` is correct.

Contributing
------------

See [CONTRIBUTING.md](https://github.com/instana/haskell-trace-sdk/blob/main/CONTRIBUTING.md).

