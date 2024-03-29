{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.SDK
Description : The main API of the Instana Haskell Trace SDK.

Instana.SDK.SDK is the main API of the Instana Haskell Trace SDK. Use one of
'initInstana', 'initConfiguredInstana', 'withInstana', or
'withConfiguredInstana' to get an InstanaContext. Then use the context with any
of the 'withRootEntry', 'withEntry', 'withExit' functions for tracing.
-}
module Instana.SDK.SDK
    ( Config
    , InstanaContext
    , addAnnotation
    , addAnnotationAt
    , addAnnotationToEntrySpan
    , addAnnotationToEntrySpanAt
    , addAnnotationValueAt
    , addAnnotationValueToEntrySpanAt
    , addHttpTracingHeaders
    , addJsonValueAt
    , addJsonValueToEntrySpanAt
    , addToErrorCount
    , addWebsiteMonitoringBackEndCorrelation
    , agentHost
    , agentPort
    , captureHttpStatus
    , completeEntry
    , completeExit
    , currentParentId
    , currentSpan
    , currentSpanId
    , currentTraceId
    , currentTraceIdInternal
    , defaultConfig
    , forceTransmissionAfter
    , forceTransmissionStartingAt
    , incrementErrorCount
    , initConfiguredInstana
    , initInstana
    , isConnected
    , maxBufferedSpans
    , postProcessHttpResponse
    , readHttpTracingHeaders
    , serviceName
    , setCorrelationId
    , setCorrelationType
    , setServiceName
    , setSynthetic
    , startEntry
    , startExit
    , startHttpEntry
    , startHttpExit
    , startRootEntry
    , withConfiguredInstana
    , withEntry
    , withExit
    , withHttpEntry
    , withHttpEntry_
    , withHttpExit
    , withInstana
    , withRootEntry
    ) where


import           Control.Concurrent                   (ThreadId)
import qualified Control.Concurrent                   as Concurrent
import           Control.Concurrent.STM               (STM)
import qualified Control.Concurrent.STM               as STM
import           Control.Monad                        (join, when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Aeson                           (ToJSON)
import qualified Data.ByteString.Char8                as BSC8
import           Data.CaseInsensitive                 (CI)
import qualified Data.CaseInsensitive                 as CI
import qualified Data.List                            as List
import qualified Data.Map.Strict                      as Map
import qualified Data.Maybe                           as Maybe
import qualified Data.Sequence                        as Seq
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Types                   as HTTPTypes
import qualified Network.Socket                       as Socket
import qualified Network.Wai                          as Wai
import           System.Log.Logger                    (warningM)
import qualified System.Posix.Process                 as Process

import           Instana.SDK.Config
import           Instana.SDK.Internal.Command         (Command)
import qualified Instana.SDK.Internal.Command         as Command
import           Instana.SDK.Internal.Config          (FinalConfig)
import qualified Instana.SDK.Internal.Config          as InternalConfig
import           Instana.SDK.Internal.Context         (ConnectionState (..),
                                                       InternalContext (InternalContext))
import qualified Instana.SDK.Internal.Context         as InternalContext
import           Instana.SDK.Internal.Id              (Id)
import qualified Instana.SDK.Internal.Id              as Id
import           Instana.SDK.Internal.Logging         (instanaLogger)
import qualified Instana.SDK.Internal.Logging         as Logging
import qualified Instana.SDK.Internal.Metrics.Sample  as Sample
import qualified Instana.SDK.Internal.Secrets         as Secrets
import qualified Instana.SDK.Internal.ServerTiming    as ServerTiming
import           Instana.SDK.Internal.SpanStack       (SpanStack)
import qualified Instana.SDK.Internal.SpanStack       as SpanStack
import           Instana.SDK.Internal.Util            ((|>))
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)
import qualified Instana.SDK.Internal.W3CTraceContext as W3CTraceContext
import qualified Instana.SDK.Internal.Worker          as Worker
import           Instana.SDK.Span.EntrySpan           (EntrySpan (..))
import qualified Instana.SDK.Span.EntrySpan           as EntrySpan
import           Instana.SDK.Span.ExitSpan            (ExitSpan (ExitSpan))
import qualified Instana.SDK.Span.ExitSpan            as ExitSpan
import           Instana.SDK.Span.NonRootEntry        (NonRootEntry (NonRootEntry))
import qualified Instana.SDK.Span.NonRootEntry        as NonRootEntry
import           Instana.SDK.Span.RootEntry           (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry           as RootEntry
import           Instana.SDK.Span.SimpleSpan          (SimpleSpan)
import qualified Instana.SDK.Span.SimpleSpan          as SimpleSpan
import           Instana.SDK.Span.Span                (Span (..), SpanKind (..))
import qualified Instana.SDK.Span.Span                as Span
import           Instana.SDK.Span.SpanData            (Annotation (..),
                                                       AnnotationValue)
import qualified Instana.SDK.Span.SpanData            as SpanData
import           Instana.SDK.Span.SpanType            (SpanType (RegisteredSpan))
import qualified Instana.SDK.Span.SpanType            as SpanType
import           Instana.SDK.TracingHeaders           (TracingHeaders,
                                                       readHttpTracingHeaders)
import qualified Instana.SDK.TracingHeaders           as TracingHeaders


{-| A container for all the things the Instana SDK needs to do its work.
-}
type InstanaContext = InternalContext


httpServerSpan :: SpanType
httpServerSpan = RegisteredSpan SpanType.HaskellWaiServer


{-| Initializes the Instana SDK and the connection to the Instana agent.

The configuration is read from the environment, falling back to default values.
-}
initInstana :: MonadIO m => m InstanaContext
initInstana = do
  conf <- liftIO $ InternalConfig.readConfigFromEnvironmentAndApplyDefaults
  liftIO $ initInstanaInternal conf


{-| Initializes the Instana SDK and the connection to the Instana agent, then
calls the given function with the established connection.

The configuration is read from the environment, falling back to default values.
-}
withInstana :: MonadIO m => (InstanaContext -> m a) -> m a
withInstana fn = do
  conf <- liftIO InternalConfig.readConfigFromEnvironmentAndApplyDefaults
  withInstanaInternal conf fn


{-| Initializes the Instana SDK and the connection to the Instana agent, using
the given Instana configuration.

Configuration settings that have not been set in the given configuration are
read from the environment, falling back to default values.
-}
initConfiguredInstana :: MonadIO m => Config -> m InstanaContext
initConfiguredInstana conf  = do
  confFromEnv <- liftIO $ InternalConfig.readConfigFromEnvironment
  let
     mergedConf = InternalConfig.mergeConfigs conf confFromEnv
  liftIO $ initInstanaInternal mergedConf


{-| Initializes the Instana SDK and the connection to the Instana agent, then
calls the given function with the established connection, using the given
Instana configuration.

Configuration settings that have not been set in the given configuration are
read from the environment, falling back to default values.
-}
withConfiguredInstana :: MonadIO m => Config -> (InstanaContext -> m a) -> m a
withConfiguredInstana conf fn = do
  confFromEnv <- liftIO $ InternalConfig.readConfigFromEnvironment
  let
     mergedConf = InternalConfig.mergeConfigs conf confFromEnv
  withInstanaInternal mergedConf fn


withInstanaInternal ::
  MonadIO m =>
  FinalConfig
  -> (InstanaContext -> m a)
  -> m a
withInstanaInternal conf fn = do
  context <- liftIO $ initInstanaInternal conf
  fn context


initInstanaInternal :: FinalConfig -> IO InstanaContext
initInstanaInternal conf = do
  now <- round . (* 1000) <$> getPOSIXTime
  pid <- Process.getProcessID
  Logging.initLogger $ show pid
  commandQueue <- STM.newTQueueIO
  spanQueue <- STM.newTVarIO $ Seq.empty
  connectionState <- STM.newTVarIO $ Unconnected
  fileDescriptor <- STM.newTVarIO $ Nothing
  threadId <- Concurrent.myThreadId
  currentSpans <- STM.newTVarIO $ Map.singleton threadId SpanStack.empty
  previousMetricsSample <- STM.newTVarIO $ Sample.empty now
  -- HTTP.newManager is keep-alive by default (10 connections, we set it to 5)
  manager <- HTTP.newManager $
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 5
      , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ 5000 * 1000
      , HTTP.managerRawConnection =
          HTTP.rawConnectionModifySocket
            (\socket -> do
                fileDescriptorFromSocket <- Socket.unsafeFdSocket socket
                STM.atomically $
                  STM.writeTVar fileDescriptor (Just fileDescriptorFromSocket)
            )
      }
  let
    context =
      InternalContext
        { InternalContext.config = conf
         -- sdkStartTime will be used to approximate the process start time on
         -- non-Linux platforms where System.SysInfo is not available. The
         -- assumption is that the SDK is initialized right at the start of the
         -- process.
        , InternalContext.sdkStartTime = now
        , InternalContext.httpManager = manager
        , InternalContext.commandQueue = commandQueue
        , InternalContext.spanQueue = spanQueue
        , InternalContext.connectionState = connectionState
        , InternalContext.fileDescriptor = fileDescriptor
        , InternalContext.currentSpans = currentSpans
        , InternalContext.previousMetricsSample = previousMetricsSample
        }
  -- The worker thread will also try to establish the connection to the agent
  -- and only start its work when that was successful.
  Worker.spawnWorker context
  return context


-- |Wraps an IO action in 'startRootEntry' and 'completeEntry'. For incoming
-- HTTP requests, prefer 'withHttpEntry' instead.
withRootEntry ::
  MonadIO m =>
  InstanaContext
  -> SpanType
  -> m a
  -> m a
withRootEntry context spanType io = do
  startRootEntry context spanType
  result <- io
  completeEntry context
  return result


-- |Wraps an IO action in 'startEntry' and 'completeEntry'. For incoming HTTP
-- requests, prefer 'withHttpEntry' instead.
withEntry ::
  MonadIO m =>
  InstanaContext
  -> String
  -> String
  -> SpanType
  -> m a
  -> m a
withEntry context traceId parentId spanType io = do
  startEntry context traceId parentId spanType
  result <- io
  completeEntry context
  return result


-- |Wraps an IO action in 'startEntry' and 'completeEntry'. This internal method
-- accepts Id values instead of String values for trace ID/parent ID, to allow
-- span.lt to be transported alongside the shortened trace ID.
withEntry' ::
  MonadIO m =>
  InstanaContext
  -> Id
  -> Id
  -> SpanType
  -> m a
  -> m a
withEntry' context traceId parentId spanType io = do
  startEntry' context traceId parentId spanType
  result <- io
  completeEntry context
  return result


-- |A convenience function that examines the given incoming HTTP request for
-- Instana tracing headers
-- (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers)
-- and wraps the given IO action either in 'startRootEntry' or  'startEntry' and
-- 'completeEntry', depending on the presence or absence of these headers. It
-- will also capture the response HTTP status (and set the span's error count
-- if it is 5xx). Finally, it will add (or append to) the HTTP response header
-- (Server-Timing) that is used for website monitoring back end correlation.
-- (The latter part is the difference to 'withHttpEntry_', plus the slightly
-- different type signature.)
--
-- This function should be preferred over 'withHttpEntry_'.
--
-- You do not need to handle incoming HTTP requests at all when using the
-- Instana WAI middleware plug-in.
withHttpEntry ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> m Wai.Response
  -> m Wai.Response
withHttpEntry context request io = do
    response <- withHttpEntry_ context request $ do
      io >>= postProcessHttpResponse context
    return response


-- |A variant of 'withHttpEntry' with a more general type signature, but less
-- features. It will automatically continue the trace from incoming headers just
-- like withHttpEntry does, but it will not capture the status code of the HTTP
-- response or add the response header for website monitoring back end
-- correlation (Server-Timing).
--
-- It is recommended to use 'withHttpEntry' instead of this function, if
-- possible. Alternatively, you can also call 'postProcessHttpResponse' inside
-- the 'withHttpEntry_' block to cover the two missing features mentioned above.
--
-- Note that you do not need to handle incoming HTTP requests at all when using
-- the Instana WAI middleware plug-in.
withHttpEntry_ ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> m a
  -> m a
withHttpEntry_ context request io =
  commonHttpHandling
    context
    request
    HttpTracingHandlers
      { continueFromInstanaHeaders = withHttpEntryContinueFromInstanaHeaders
      , continueFromTraceParent = withHttpEntryContinueFromTraceParent
      , continueFromTraceStateInstanaKeyValuePair =
          withHttpEntryContinueFromTraceStateInstanaKeyValuePair
      , createRoot = withHttpEntryRoot
      }
    io


-- |A variant of 'withHttpEntry' that continues a trace from Instana headers
-- (X-INSTANA-T, X-INSTANA-S and X-INSTANA-L).
withHttpEntryContinueFromInstanaHeaders ::
  MonadIO m
  => InstanaContext
  -> String
  -> String
  -> m a
  -> m a
withHttpEntryContinueFromInstanaHeaders context t s io =
  withEntry context t s httpServerSpan io


-- |A variant of 'withHttpEntry' that continues a trace from the W3C trace
-- context headers traceparent.
withHttpEntryContinueFromTraceParent ::
  MonadIO m
  => InstanaContext
  -> W3CTraceContext
  -> m a
  -> m a
withHttpEntryContinueFromTraceParent context w3cTraceContext io =
  let
    traceParent = W3CTraceContext.traceParent w3cTraceContext
  in
  withEntry'
    context
    (W3CTraceContext.traceId traceParent)
    (W3CTraceContext.parentId traceParent)
    httpServerSpan
    (setSpanTpFlag context >> io)


-- |A variant of 'withHttpEntry' that continues a trace from the Instana
-- key-value pair from the tracestate header.
withHttpEntryContinueFromTraceStateInstanaKeyValuePair ::
  MonadIO m
  => InstanaContext
  -> Id
  -> Id
  -> m a
  -> m a
withHttpEntryContinueFromTraceStateInstanaKeyValuePair context t s io =
  withEntry' context t s httpServerSpan io


-- |A variant of 'withHttpEntry' that does not continue a trace but starts a
-- new trace.
withHttpEntryRoot ::
  MonadIO m
  => InstanaContext
  -> TracingHeaders
  -> m a
  -> m a
withHttpEntryRoot context tracingHeaders io =
  withRootEntry context httpServerSpan $
    addCorrelationTypeAndIdToSpan context tracingHeaders
    >> io


-- |Wraps an IO action in 'startExit' and 'completeExit'.
withExit ::
  MonadIO m =>
  InstanaContext
  -> SpanType
  -> m a
  -> m a
withExit context spanType io = do
  startExit context spanType
  result <- io
  completeExit context
  return result


-- |Wraps an IO action in 'startHttpExit' and 'completeExit'. The given action
-- is accepted as a function (Request -> IO a) and is expected to use the
-- provided request parameter for executing the HTTP request.
withHttpExit ::
  MonadIO m =>
  InstanaContext
  -> HTTP.Request
  -> (HTTP.Request -> m a)
  -> m a
withHttpExit context request io = do
  request' <- startHttpExit context request
  result <- io request'
  completeExit context
  return result


-- |Creates a preliminary/incomplete root entry span, which should later be
-- completed with 'completeEntry'.
startRootEntry ::
  MonadIO m =>
  InstanaContext
  -> SpanType
  -> m ()
startRootEntry context spanType = do
  liftIO $ do
    timestamp <- round . (* 1000) <$> getPOSIXTime
    traceId <- Id.generate
    let
      newSpan =
        RootEntrySpan $
          RootEntry
            { RootEntry.spanAndTraceId  = traceId
            , RootEntry.spanType        = spanType
            , RootEntry.timestamp       = timestamp
            , RootEntry.errorCount      = 0
            , RootEntry.serviceName     = Nothing
            , RootEntry.synthetic       = False
            , RootEntry.correlationType = Nothing
            , RootEntry.correlationId   = Nothing
            , RootEntry.spanData        = Span.initialData EntryKind spanType
            , RootEntry.w3cTraceContext = Nothing
            }
    pushSpan
      context
      (\stack ->
        case stack of
          Nothing ->
            -- We did not initialise the span stack for this thread, do it now.
            SpanStack.entry newSpan
          Just spanStack ->
            spanStack
            |> SpanStack.push (Entry newSpan)
      )


-- |Creates a preliminary/incomplete entry span, which should later be completed
-- by calling 'completeEntry'.
startEntry ::
  MonadIO m =>
  InstanaContext
  -> String
  -> String
  -> SpanType
  -> m ()
startEntry context traceId parentId spanType = do
  let
    tId = Id.fromString traceId
    pId = Id.fromString parentId
  startEntry' context tId pId spanType


-- |Creates a preliminary/incomplete entry span, which should later be completed
-- by calling 'completeEntry'. This internal method accepts Id values instead of
-- String values for trace ID/parent ID, to allow span.lt to be transported
-- alongside the shortened trace ID.
startEntry' ::
  MonadIO m =>
  InstanaContext
  -> Id
  -> Id
  -> SpanType
  -> m ()
startEntry' context traceId parentId spanType = do
  liftIO $ do
    timestamp <- round . (* 1000) <$> getPOSIXTime
    spanId <- Id.generate
    let
      newSpan =
        NonRootEntrySpan $
          NonRootEntry
            { NonRootEntry.traceId         = traceId
            , NonRootEntry.spanId          = spanId
            , NonRootEntry.parentId        = parentId
            , NonRootEntry.spanType        = spanType
            , NonRootEntry.timestamp       = timestamp
            , NonRootEntry.errorCount      = 0
            , NonRootEntry.serviceName     = Nothing
            , NonRootEntry.synthetic       = False
            , NonRootEntry.spanData        = Span.initialData EntryKind spanType
            , NonRootEntry.w3cTraceContext = Nothing
            , NonRootEntry.tpFlag          = False
            }
    pushSpan
      context
      (\stack ->
        case stack of
          Nothing ->
            -- We did not initialise the span stack for this thread yet, do
            -- it now.
            SpanStack.entry newSpan
          Just spanStack ->
            spanStack
            |> SpanStack.push (Entry newSpan)
      )
    return ()


-- |A convenience function that examines the given request for Instana tracing
-- headers (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers)
-- and either calls 'startRootEntry' or  'startEntry', depending on the presence
-- of absence of these headers.
startHttpEntry ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> m ()
startHttpEntry context request = do
  commonHttpHandling
    context
    request
    HttpTracingHandlers
      { continueFromInstanaHeaders = startHttpEntryContinueFromInstanaHeaders
      , continueFromTraceParent = startHttpEntryContinueFromTraceParent
      , continueFromTraceStateInstanaKeyValuePair =
          startHttpEntryContinueFromTraceStateInstanaKeyValuePair
      , createRoot = startHttpEntryRoot
      }
    (return ())


-- |A variant of 'startHttpEntry' that continues a trace from Instana headers
-- (X-INSTANA-T, X-INSTANA-S and X-INSTANA-L).
startHttpEntryContinueFromInstanaHeaders ::
  MonadIO m
  => InstanaContext
  -> String
  -> String
  -> m a
  -> m a
startHttpEntryContinueFromInstanaHeaders context t s io = do
  startEntry context t s httpServerSpan
  io


-- |A variant of 'startHttpEntry' that continues a trace from the W3C trace
-- context headers traceparent.
startHttpEntryContinueFromTraceParent ::
  MonadIO m
  => InstanaContext
  -> W3CTraceContext
  -> m a
  -> m a
startHttpEntryContinueFromTraceParent context w3cTraceContext io = do
  let
    traceParent = W3CTraceContext.traceParent w3cTraceContext
  startEntry'
    context
    (W3CTraceContext.traceId traceParent)
    (W3CTraceContext.parentId traceParent)
    httpServerSpan
  (setSpanTpFlag context >> io)


-- |A variant of 'startHttpEntry' that continues a trace from the Instana
-- key-value pair from the tracestate header.
startHttpEntryContinueFromTraceStateInstanaKeyValuePair ::
  MonadIO m
  => InstanaContext
  -> Id
  -> Id
  -> m a
  -> m a
startHttpEntryContinueFromTraceStateInstanaKeyValuePair
    context
    t
    s
    io = do
  startEntry'
    context
    t
    s
    httpServerSpan
  io


-- |A variant of 'startHttpEntry' that does not continue a trace but starts a
-- new trace.
startHttpEntryRoot ::
  MonadIO m
  => InstanaContext
  -> TracingHeaders
  -> m a
  -> m a
startHttpEntryRoot context tracingHeaders io = do
  startRootEntry context httpServerSpan
  addCorrelationTypeAndIdToSpan context tracingHeaders
  io


-- |A set of handlers to continue a trace from incoming headers or create a
-- new trace, which can be either used from withHttpEntry or startHttpEntry.
data HttpTracingHandlers m a = HttpTracingHandlers
  { continueFromInstanaHeaders ::
      InstanaContext
      -> String
      -> String
      -> m a
      -> m a
  , continueFromTraceParent ::
      InstanaContext
      -> W3CTraceContext
      -> m a
      -> m a
  , continueFromTraceStateInstanaKeyValuePair ::
      InstanaContext
      -> Id
      -> Id
      -> m a
      -> m a
  , createRoot ::
      InstanaContext
      -> TracingHeaders
      -> m a
      -> m a
  }


-- |Bundles common handling for startHttpEntry and withHttpEntry.
commonHttpHandling ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> HttpTracingHandlers m a
  -> m a
  -> m a
commonHttpHandling context request httpTracingHandlers io = do
  let
    tracingHeaders = readHttpTracingHeaders request
    traceId = TracingHeaders.traceId tracingHeaders
    spanId = TracingHeaders.spanId tracingHeaders
    level = TracingHeaders.level tracingHeaders
    traceparent = TracingHeaders.traceparent tracingHeaders
    tracestate = TracingHeaders.tracestate tracingHeaders

    -- discard incoming X-INSTANA-T/-S if eum correlation data is present
    (traceId', spanId') =
      case TracingHeaders.correlationId tracingHeaders of
        Nothing ->
          (traceId, spanId)
        Just _ ->
          (Nothing, Nothing)

    w3cTraceContext =
      case traceparent of
        Just tp ->
          W3CTraceContext.decode tp tracestate
        Nothing ->
          Nothing

  case level of

    TracingHeaders.Trace ->
      executeTracedHttpRequest
        context
        request
        httpTracingHandlers
        tracingHeaders
        w3cTraceContext
        traceId'
        spanId'
        io

    TracingHeaders.Suppress ->
      executeSuppressedHttpRequest context w3cTraceContext io


-- |Evaluates the incoming headers (Instana headers and W3C trace context) and
-- decides from which set of headers to continue the trace (or to start a new
-- trace).
executeTracedHttpRequest ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> HttpTracingHandlers m a
  -> TracingHeaders
  -> Maybe W3CTraceContext
  -> Maybe String
  -> Maybe String
  -> m a
  -> m a
executeTracedHttpRequest
    context
    request
    httpTracingHandlers
    tracingHeaders
    w3cTraceContext
    traceId
    spanId
    io = do
  let
    synthetic = TracingHeaders.synthetic tracingHeaders

    io' =
      (setW3cTraceContext context w3cTraceContext)
      >> io
      >>= (\ioResult -> do
        addHttpData context request synthetic
        return ioResult
      )

    w3cTsInKvPair =
      join $
        W3CTraceContext.instanaKeyValuePair <$>
          W3CTraceContext.traceState <$> w3cTraceContext
    tIdFromW3cInKvPair = W3CTraceContext.instanaTraceId <$> w3cTsInKvPair
    pIdFromW3cInKvPair = W3CTraceContext.instanaParentId <$> w3cTsInKvPair

    w3cTraceCorrelationDisabled =
      InternalConfig.disableW3cTraceCorrelation . InternalContext.config $
        context

  case ( traceId
       , spanId
       , w3cTraceContext
       , w3cTraceCorrelationDisabled
       , tIdFromW3cInKvPair
       , pIdFromW3cInKvPair
       ) of

    (Just t, Just s, _, _, _, _) ->
      (continueFromInstanaHeaders httpTracingHandlers)
        context
        t
        s
        io'

    (_, _, Just w3cCtx, False, _, _) ->
      (continueFromTraceParent httpTracingHandlers)
        context
        w3cCtx
        io'

    (_, _, _, True, Just t, Just s) ->
      (continueFromTraceStateInstanaKeyValuePair httpTracingHandlers)
        context
        t
        s
        io'

    _                ->
      (createRoot httpTracingHandlers)
        context
        tracingHeaders
        io'


-- |Handles an incoming HTTP request when tracing is suppressed.
executeSuppressedHttpRequest ::
  MonadIO m =>
  InstanaContext
  -> Maybe W3CTraceContext
  -> m a
  -> m a
executeSuppressedHttpRequest context maybeW3cTraceContext io = do
  liftIO $ do
    w3cTraceContext <-
      case maybeW3cTraceContext of
        Just w3cCtx ->
          return w3cCtx
        Nothing -> do
          W3CTraceContext.initBogusContextForSuppressedRequest
    pushSpan
      context
      (\stack ->
        case stack of
          Nothing ->
            -- We did not initialise the span stack for this thread, do it
            -- now.
            SpanStack.suppress w3cTraceContext
          Just spanStack ->
            SpanStack.pushSuppress w3cTraceContext spanStack
      )
  io


addHttpData ::
  MonadIO m =>
  InstanaContext ->
  Wai.Request ->
  Bool ->
  m ()
addHttpData context request synthetic = do
  extraHeadersConfig <- liftIO $ InternalContext.readExtraHeaders context
  secretsMatcher <- liftIO $ InternalContext.readSecretsMatcher context
  let
    host :: String
    host =
      Wai.requestHeaderHost request
      |> fmap BSC8.unpack
      |> Maybe.fromMaybe ""
    capturedHeaders = collectHeaders extraHeadersConfig $ Wai.requestHeaders request
    httpAnnotations =
      [ SpanData.simpleAnnotation "method" $
          Wai.requestMethod request |> BSC8.unpack
      , SpanData.simpleAnnotation "url"    $
          Wai.rawPathInfo request |> BSC8.unpack
      , SpanData.simpleAnnotation "host" $ host
      , SpanData.optionalAnnotation "params" $
           (processQueryString secretsMatcher (Wai.rawQueryString request))
      ]
    httpAnnotations' =
      case capturedHeaders of
        Just headers ->
          (SpanData.objectAnnotation "header" headers) : httpAnnotations
        Nothing ->
          httpAnnotations

  addAnnotation context (Object "http" httpAnnotations')
  setSynthetic context synthetic


collectHeaders ::
  [CI BSC8.ByteString]
  -> [HTTPTypes.Header]
  -> Maybe [Annotation]
collectHeaders extraHeadersConfig allHeaders =
  let
    filtered = filterHeaders extraHeadersConfig allHeaders
    listOfStringTuples =
      fmap
        (\(name, value) -> (BSC8.unpack $ CI.original name, BSC8.unpack value))
        filtered
    headersAsAnnotations =
      fmap
        (\(name, value) -> SpanData.simpleAnnotation (T.pack name) value)
        listOfStringTuples
  in
  if null headersAsAnnotations
    then Nothing
    else Just $ headersAsAnnotations


filterHeaders :: [CI BSC8.ByteString] -> [HTTPTypes.Header] -> [HTTPTypes.Header]
filterHeaders configuredList allHeaders =
  let
    filterFn (name, _) =
      elem name configuredList
  in
  filter filterFn allHeaders


-- |Adds website correlation annotations to the HTTP entry span.
addCorrelationTypeAndIdToSpan ::
  MonadIO m =>
  InstanaContext
  -> TracingHeaders
  -> m ()
addCorrelationTypeAndIdToSpan context tracingHeaders = do
  let
    correlationType = TracingHeaders.correlationType tracingHeaders
    correlationId = TracingHeaders.correlationId tracingHeaders
  case (correlationType, correlationId) of
    (Nothing, Nothing) ->
      return ()
    (Just crtp, Nothing) -> do
      setCorrelationType context (T.pack crtp)
      return ()
    (Nothing, Just crid) -> do
      setCorrelationId context (T.pack crid)
      return ()
    (Just crtp, Just crid) -> do
      setCorrelationType context (T.pack crtp)
      setCorrelationId context (T.pack crid)
      return ()


-- |Processes the response of an HTTP entry. This function needs be called while
-- the HTTP entry span is still active. It can be used inside a 'withHttpEntry_'
-- block or between 'startHttpEntry' and 'completeEntry'.
--
-- This function accomplishes two things:
-- * It captures the HTTP status code from the response and adds it as an
--   annotation to the currently active span.
-- * It adds an additional HTTP response header (Server-Timing) to the given HTTP
--   response that enables website monitoring back end correlation. In case the
--   response already has a Server-Timing header, a value is appended to the
--   existing Server-Timing list.
--
-- Client code should rarely have the need to call this directly. Instead,
-- capture incoming HTTP requests with 'withHttpEntry', which does
-- both of these things automatically.
--
-- Clients should make sure to call this in the context provided above, that is,
-- within 'withHttpEntry_' or between 'startHttpEntry' and 'completeEntry' but
-- outside of blocks that create an exit span, that is, outside of 'withExit',
-- 'withHttpExit' and not between 'startExit' and 'completeExit'.
postProcessHttpResponse ::
  MonadIO m =>
  InstanaContext
  -> Wai.Response
  -> m Wai.Response
postProcessHttpResponse context response = do
  liftIO $ do
    captureHttpStatusUnlifted context response
    captureResponseHeaders context response
    addWebsiteMonitoringBackEndCorrelationUnlifted context response


-- |Captures the status code of the HTTP response and adds it to the currently
-- active span. If the status code is >= 500, the status message is also
-- captured. This function needs be called while the HTTP entry span is still
-- active. It can be used inside a 'withHttpEntry_' block or between
-- 'startHttpEntry' and 'completeEntry'.
--
-- Client code should rarely have the need to call this directly. Instead,
-- capture incoming HTTP requests with 'withHttpEntry', which captures the
-- status code automatically and also adds the Server-Timing header for back end
-- web site monitoring correlation. When not using 'withHttpEntry', the function
-- 'postProcessHttpResponse' should be preferred over this function, because it
-- does both (capture the status code and add the Server-Timing header).
captureHttpStatus ::
  MonadIO m =>
  InstanaContext
  -> Wai.Response
  -> m ()
captureHttpStatus context response = do
  liftIO $ captureHttpStatusUnlifted context response


-- |Captures the status code of the HTTP response and adds it to the currently
-- active span. If the status code is >= 500, the status message is also
-- captured.
captureHttpStatusUnlifted ::
  InstanaContext
  -> Wai.Response
  -> IO ()
captureHttpStatusUnlifted context response = do
  let
    (HTTPTypes.Status statusCode statusMessage) =
      Wai.responseStatus response
  addAnnotationValueToEntrySpanAt context "http.status" $
    SpanData.simpleValue statusCode
  when
    (statusCode >= 500 )
    (addAnnotationValueToEntrySpanAt context "http.message" $
      SpanData.simpleValue $ BSC8.unpack statusMessage
    )


-- |Captures the HTTP headers of the response, if extra headers for capture have
-- been configured. The captured header (if any) will be added to the currently
-- active span. This function needs be called while the HTTP entry span is still
-- active. It can be used inside a 'withHttpEntry_' block or between
-- 'startHttpEntry' and 'completeEntry'.
--
-- Client code should rarely have the need to call this directly. Instead,
-- capture incoming HTTP requests with 'withHttpEntry', which captures the
-- headers automatically. When not using 'withHttpEntry', the function
-- 'postProcessHttpResponse' should be preferred over this function.
captureResponseHeaders ::
  MonadIO m =>
  InstanaContext
  -> Wai.Response
  -> m ()
captureResponseHeaders context response = do
  liftIO $ captureResponseHeadersUnlifted context response


-- |Captures the HTTP headers of the response, if extra headers for capture have
-- been configured. The captured header (if any) will be added to the currently
-- active span.
captureResponseHeadersUnlifted ::
  InstanaContext
  -> Wai.Response
  -> IO ()
captureResponseHeadersUnlifted context response = do
  extraHeadersConfig <- liftIO $ InternalContext.readExtraHeaders context
  let
    capturedHeaders =
      collectHeaders extraHeadersConfig $ Wai.responseHeaders response
  when
    (Maybe.isJust capturedHeaders)
    (do
       let
         Just headers = capturedHeaders
       addAnnotationToEntrySpanAt context "http" $
         SpanData.objectAnnotation "header" headers
    )


-- |Adds an additional HTTP response header (Server-Timing) to the given HTTP
-- response that enables website monitoring back end correlation. In case the
-- response already has a Server-Timing header, a value is appended to the
-- existing Server-Timing list. This function needs be called while the HTTP
-- entry span is still active. It can be used inside a 'withHttpEntry_' block or
-- between 'startHttpEntry' and 'completeEntry'.
--
-- Client code should rarely have the need to call this directly. Instead,
-- capture incoming HTTP requests with 'withHttpEntry', which adds the
-- response header automatically and also captures the HTTP status code of the
-- response. When not using 'withHttpEntry', the function
-- 'postProcessHttpResponse' should be preferred over this function, because
-- it does both (capture the status code and add the Server-Timing header).
addWebsiteMonitoringBackEndCorrelation ::
  MonadIO m =>
  InstanaContext
  -> Wai.Response
  -> m Wai.Response
addWebsiteMonitoringBackEndCorrelation context response = do
  liftIO $ addWebsiteMonitoringBackEndCorrelationUnlifted context response


-- |Adds an additional HTTP response header (Server-Timing) to the given HTTP
-- response that enables website monitoring back end correlation. In case the
-- response already has a Server-Timing header, a value is appended to the
-- existing Server-Timing list.
addWebsiteMonitoringBackEndCorrelationUnlifted ::
  InstanaContext
  -> Wai.Response
  -> IO Wai.Response
addWebsiteMonitoringBackEndCorrelationUnlifted context response = do
  traceIdMaybe <- currentTraceIdInternal context
  suppressed <- isSuppressed context
  case (traceIdMaybe, suppressed) of
    (Just traceId, False) ->
      return $
        Wai.mapResponseHeaders
        (ServerTiming.addTraceIdToServerTiming traceId)
        response
    _ ->
      return response


-- |Creates a preliminary/incomplete exit span, which should later be completed
-- with 'completeExit'.
startExit ::
  MonadIO m =>
  InstanaContext
  -> SpanType
  -> m ()
startExit context spanType = do
  liftIO $ do
    suppressed <- isSuppressed context
    if suppressed then
      return ()
    else do
      entrySpan <- peekSpan context
      case entrySpan of
        Just (Entry parent) -> do
          spanId <- Id.generate
          timestamp <- round . (* 1000) <$> getPOSIXTime
          let
            parentW3cTraceContext = EntrySpan.w3cTraceContext parent
            w3cTraceContext =
              case parentW3cTraceContext of
                Just w3cCtx ->
                  W3CTraceContext.inheritFrom
                    w3cCtx
                    (EntrySpan.traceId parent)
                    spanId
                Nothing ->
                  W3CTraceContext.exitSpanContextFromIds
                    (EntrySpan.traceId parent)
                    spanId
            newSpan =
              ExitSpan
                { ExitSpan.parentSpan      = parent
                , ExitSpan.spanId          = spanId
                , ExitSpan.spanType        = spanType
                , ExitSpan.timestamp       = timestamp
                , ExitSpan.errorCount      = 0
                , ExitSpan.serviceName     = Nothing
                , ExitSpan.spanData        = Span.initialData
                                               ExitKind
                                               spanType
                , ExitSpan.w3cTraceContext = w3cTraceContext
                }
          pushSpan
            context
            (\stack ->
              case stack of
                Nothing        ->
                  -- No entry present, it is invalid to push an exit onto the
                  -- stack without an entry. But we can at least init a stack
                  -- for the current thread.
                  SpanStack.empty
                Just spanStack ->
                  spanStack
                  |> SpanStack.push (Exit newSpan)
            )
        Just (Exit ex) -> do
          warningM instanaLogger $
            "Cannot start exit span \"" ++ show spanType ++
            "\" since there is already an active exit span " ++
            "in progress: " ++ show ex
        Nothing -> do
          warningM instanaLogger $
            "Cannot start exit span \"" ++ show spanType ++
            "\" since there is no active entry span " ++
            "(actually, there is no active span at all)."
          return ()


-- |Creates a preliminary/incomplete http exit span, which should later be
-- completed with 'completeExit'. The Instana tracing headers are added to the
-- request and the modified request value is returned (use the return value of
-- this function to execute your request instead of the request value passed
-- into this function).
startHttpExit ::
  MonadIO m =>
  InstanaContext
  -> HTTP.Request
  -> m HTTP.Request
startHttpExit context request = do
  extraHeadersConfig <- liftIO $ InternalContext.readExtraHeaders context
  secretsMatcher <- liftIO $ InternalContext.readSecretsMatcher context

  let
    originalCheckResponse = HTTP.checkResponse request
    request' =
      request
        -- Inject a checkResponse hook to capture the response status and
        -- response headers.
        { HTTP.checkResponse = (\req res -> do
            let
              status =
                res
                  |> HTTP.responseStatus
                  |> HTTPTypes.statusCode
              capturedResponseHeaders =
                collectHeaders extraHeadersConfig $
                  HTTP.responseHeaders res

            addAnnotationValueAt context "http.status" $
              SpanData.simpleValue status

            when
              (Maybe.isJust capturedResponseHeaders)
              (do
                 let
                   Just headers = capturedResponseHeaders
                 addAnnotationAt context "http" $
                     SpanData.objectAnnotation "header" headers
              )

            originalCheckResponse req res
          )
        }
    port = ":" ++ (show $ HTTP.port request)
    protocol = if HTTP.secure request then "https://" else "http://"
    host = BSC8.unpack $ HTTP.host request
    path = BSC8.unpack $ HTTP.path request
    url = protocol ++ host ++ port ++ path
    capturedRequestHeaders = collectHeaders extraHeadersConfig $ HTTP.requestHeaders request
    httpAnnotations =
      [ SpanData.simpleAnnotation "method" $ BSC8.unpack $ HTTP.method request
      , SpanData.simpleAnnotation "url"    url
      , SpanData.optionalAnnotation "params"
          (processQueryString secretsMatcher (HTTP.queryString request))
      ]
    httpAnnotations' =
      case capturedRequestHeaders of
        Just headers ->
          (SpanData.objectAnnotation "header" headers) : httpAnnotations
        Nothing ->
          httpAnnotations

  startExit context (RegisteredSpan SpanType.HaskellHttpClient)
  request'' <- addHttpTracingHeaders context request'
  addAnnotation context (Object "http" httpAnnotations')
  return request''


processQueryString :: Secrets.SecretsMatcher -> BSC8.ByteString -> Maybe Text
processQueryString secretsMatcher queryString =
  queryString
    |> BSC8.unpack
    |> T.pack
    -- drop leading "?" character
    |> (\t -> if (not . T.null) t && T.head t == '?' then T.tail t else t)
    -- split on "&" delimiter
    |> T.splitOn "&"
    -- splitOn can yield "" elements, drop them
    |> List.filter (not . T.null)
    -- convert to pairs of query string name and value
    |> List.map (T.breakOn "=")
    -- drop leading "=" from value (breakOn includes the delimiter if present)
    |> List.map (\tuple ->
         if T.isPrefixOf "=" (snd tuple)
           then
             (fst tuple, T.tail $ snd tuple)
           else
             tuple
       )
    -- redact secrets
    |> List.map (\tuple ->
         if (Secrets.isSecret secretsMatcher) (fst tuple)
           then
             (fst tuple, "<redacted>")
           else
             tuple
       )
    -- put pairs back together
    |> List.map (\tuple -> T.concat [fst tuple, "=", snd tuple])
    -- concat into one string again
    |> T.intercalate "&"
    -- drop param if there were no query parameters
    |> (\t -> if t == "" then Nothing else Just t)


-- |Completes an entry span, to be called at the last possible moment before the
-- call has been processed completely.
completeEntry ::
  MonadIO m =>
  InstanaContext
  -> m ()
completeEntry context = do
  liftIO $ do
    (poppedSpan, warning) <- popSpan context EntryKind
    case (poppedSpan, warning) of
      (Just (Entry entrySpan), _) ->
        enqueueCommand
          context
          (Command.CompleteEntry entrySpan)
      (_, Just warnMessage) -> do
        warningM instanaLogger $
          "Cannot complete entry span due to a span stack mismatch: " ++
          warnMessage
        return ()
      _ -> do
        warningM instanaLogger $
          "Cannot complete entry span due to a span stack mismatch - there " ++
          "is no active span or the currently active span is not an entry."
        return ()


-- |Completes an exit span, to be called as soon as the remote call has
-- returned.
completeExit ::
  MonadIO m =>
  InstanaContext
  -> m ()
completeExit context = do
  liftIO $ do
    suppressed <- isSuppressed context
    if suppressed then
      return ()
    else do
      (poppedSpan, warning) <- popSpan context ExitKind
      case (poppedSpan, warning) of
        (Just (Exit exitSpan), _) ->
          enqueueCommand
            context
            (Command.CompleteExit exitSpan)
        (_, Just warnMessage) -> do
          warningM instanaLogger $
            "Cannot complete exit span due to a span stack mismatch: " ++
            warnMessage
        _ -> do
          warningM instanaLogger $
            "Cannot complete exit span due to a span stack mismatch - there " ++
            "is no active span or the currently active span is not an exit."
          return ()


-- |Increments the error count for the currently active span by one. Call this
-- between startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to with withEntry/withExit/withRootEntry if an
-- error happens while processing the entry/exit.
--
-- This is an alias for `addToErrorCount instanaContext 1`.
incrementErrorCount :: MonadIO m => InstanaContext -> m ()
incrementErrorCount context =
  addToErrorCount context 1


-- |Increments the error count for the currently active span by one. Call this
-- between startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to with withEntry/withExit/withRootEntry if an
-- error happens while processing the entry/exit.
addToErrorCount :: MonadIO m => InstanaContext -> Int -> m ()
addToErrorCount context increment =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addToErrorCount increment span_)


-- |Override the name of the service for the associated call in Instana.
setServiceName :: MonadIO m => InstanaContext -> Text -> m ()
setServiceName context serviceName_ =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.setServiceName serviceName_ span_)


-- |Set the website monitoring correlation type. This should only be set on
-- root entry spans. It will be silently ignored for other types of spans.
setCorrelationType :: MonadIO m => InstanaContext -> Text -> m ()
setCorrelationType context correlationType_ =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.setCorrelationType correlationType_ span_)


-- |Set the website monitoring correlation ID. This should only be set on
-- root entry spans. It will be silently ignored for other types of spans.
setCorrelationId :: MonadIO m => InstanaContext -> Text -> m ()
setCorrelationId context correlationId_ =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.setCorrelationId correlationId_ span_)


-- |Attaches a W3C trace context to the currently active span.
setW3cTraceContext ::
  MonadIO m =>
  InstanaContext ->
  Maybe W3CTraceContext ->
  m ()
setW3cTraceContext context w3cTraceContext =
  liftIO $ do
    case w3cTraceContext of
      Just w3cCtx ->
        modifyCurrentSpan context
          (\span_ -> Span.setW3cTraceContext w3cCtx span_)
      Nothing ->
        return ()


-- |Sets the tp flag on the current span to mark it as a span that has
-- inherited the trace ID/parent ID from W3C trace context instead of Instana
-- headers.
setSpanTpFlag :: MonadIO m => InstanaContext -> m ()
setSpanTpFlag context =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.setTpFlag span_)


-- |Set the synthetic flag. This should only be set on entry spans. It will be
-- silently ignored for other types of spans.
setSynthetic :: MonadIO m => InstanaContext -> Bool -> m ()
setSynthetic context synthetic =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.setSynthetic synthetic span_)


-- |Adds an annotation to the currently active span. Call this between
-- startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to with withEntry/withExit/withRootEntry. Can be
-- called multiple times, data from multiple calls will be merged.
addAnnotation :: MonadIO m => InstanaContext -> Annotation -> m ()
addAnnotation context annotation =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addAnnotation annotation span_)


-- |Adds an annotation to the currently active span. Call this between
-- startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to withEntry/withExit/withRootEntry.
-- The given path can be a nested path, with path fragments separated by dots,
-- like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": {
--       "key_from_the_provided_annotation_value": ...
--     }
--   },
--   ...
-- }
addAnnotationAt ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> Annotation
  -> m ()
addAnnotationAt context path value =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addAnnotationAt path value span_)


-- |Adds an annotation with the given value to the currently active span. Call
-- this between startEntry/startRootEntry/startExit and
-- completeEntry/completeExit or inside the IO action given to with
-- withEntry/withExit/withRootEntry. The given path can be a nested path, with
-- path fragments separated by dots, like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": "..."
--   },
--   ...
-- }
addAnnotationValueAt ::
  (MonadIO m) =>
  InstanaContext
  -> Text
  -> AnnotationValue
  -> m ()
addAnnotationValueAt context path value =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addAnnotationValueAt path value span_)


-- |Adds a simple value (string, boolean, number) to the currently active span's
-- data section. Should not be used for objects or lists in case you intend to
-- merge them with additional values at the same path later. Call this between
-- startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to withEntry/withExit/withRootEntry.
-- The given path can be a nested path, with path fragments separated by dots,
-- like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": "..."
--   },
--   ...
-- }
addJsonValueAt ::
  (MonadIO m, ToJSON a) =>
  InstanaContext
  -> Text
  -> a
  -> m ()
addJsonValueAt context path value =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addJsonValueAt path value span_)


-- |Adds an additional annotation to the currently active entry span, even if
-- the currently active span is an intermediate or exit child of that entry
-- span. Call this between startEntry/startRootEntry/startExit and
-- completeEntry/completeExit or inside the IO action given to with
-- withEntry/withExit/withRootEntry. Can be called multiple times, data from
-- multiple calls will be merged.
addAnnotationToEntrySpan ::
  MonadIO m =>
  InstanaContext
  -> Annotation
  -> m ()
addAnnotationToEntrySpan context annotation =
  liftIO $ modifyCurrentEntrySpan context
    (\span_ -> Span.addAnnotation annotation span_)


-- |Adds an additional annotation to the currently active entry span, even if
-- the currently active span is an intermediate or exit child of that entry
-- span. Call this between startEntry/startRootEntry/startExit and
-- completeEntry/completeExit or inside the IO action given to with
-- withEntry/withExit/withRootEntry. Can be called multiple times, data from
-- multiple calls will be merged. The given path can be a nested path, with path
-- fragments separated by dots, like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": {
--       "key_from_the_provided_annotation_value": ...
--     }
--   },
--   ...
-- }
addAnnotationToEntrySpanAt ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> Annotation
  -> m ()
addAnnotationToEntrySpanAt context path annotation =
  liftIO $ modifyCurrentEntrySpan context
    (\span_ -> Span.addAnnotationAt path annotation span_)


-- |Adds an additional annotation value to the currently active entry span, even
-- if the currently active span is an intermediate or exit child of that entry
-- span. Call this between startEntry/startRootEntry/startExit and
-- completeEntry/completeExit or inside the IO action given to with
-- withEntry/withExit/withRootEntry. Can be called multiple times, data from
-- multiple calls will be merged. The given path can be a nested path, with path
-- fragments separated by dots, like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": "..."
--   },
--   ...
-- }
addAnnotationValueToEntrySpanAt ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> AnnotationValue
  -> m ()
addAnnotationValueToEntrySpanAt context path value =
  liftIO $ modifyCurrentEntrySpan context
    (\span_ -> Span.addAnnotationValueAt path value span_)


-- |Adds an additional annotation from a simple JSON value (string, number,
-- boolean) to the currently active entry span, even if
-- the currently active span is an intermediate or exit child of that entry
-- span. Should not be used for objects or lists in case you intend to merge
-- them with additional values at the same path later. Call this between
-- startEntry/startRootEntry/startExit and completeEntry/completeExit or inside
-- the IO action given to with withEntry/withExit/withRootEntry. Can be called
-- multiple times, data from  multiple calls will be merged.
-- The given path can be a nested path, with path fragments separated by dots,
-- like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": "..."
--   },
--   ...
-- }
addJsonValueToEntrySpanAt ::
  (MonadIO m, ToJSON a) =>
  InstanaContext
  -> Text
  -> a
  -> m ()
addJsonValueToEntrySpanAt context path value =
  liftIO $ modifyCurrentEntrySpan context
    (\span_ -> Span.addJsonValueAt path value span_)


-- |Adds the Instana tracing headers
-- (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers)
-- from the currently active span to the given HTTP client request.
addHttpTracingHeaders ::
  MonadIO m =>
  InstanaContext
  -> HTTP.Request
  -> m HTTP.Request
addHttpTracingHeaders context request =
  liftIO $ do
    suppressed <- isSuppressed context
    traceId <- currentTraceIdInternal context
    spanId <- currentSpanIdInternal context
    w3cTraceContext <- currentW3cTraceContext context
    let
      originalHeaders = HTTP.requestHeaders request
    case (traceId, spanId, suppressed) of
      (_, _, True) -> do
          suppressedW3cTraceContext <-
            case w3cTraceContext of
              Just w3cCtx -> do
                bogusParentId <- Id.generate
                return $
                  W3CTraceContext.inheritFromForSuppressed
                    w3cCtx
                    bogusParentId
              Nothing -> do
                bogusTraceIdFromStack <- currentTraceIdInternal context
                bogusTraceId <-
                  case bogusTraceIdFromStack of
                    Just tId -> return tId
                    Nothing  -> Id.generate
                bogusParentId <- Id.generate
                return $ W3CTraceContext.createExitContextForSuppressed bogusTraceId bogusParentId
          return $ request {
            HTTP.requestHeaders =
              ((TracingHeaders.levelHeaderName, "0") : originalHeaders)
              ++ (w3cTraceContextToHeaders $ Just suppressedW3cTraceContext)
          }

      (Just tId, Just sId, False) ->
        return $ request {
          HTTP.requestHeaders =
            (originalHeaders ++
              [ (TracingHeaders.traceIdHeaderName, Id.toByteString tId)
              , (TracingHeaders.spanIdHeaderName, Id.toByteString sId)
              ]
              ++ w3cTraceContextToHeaders w3cTraceContext
            )
        }
      (Just tId, Nothing, False) ->
        return $ request {
          HTTP.requestHeaders =
            (originalHeaders ++
              [ (TracingHeaders.traceIdHeaderName, Id.toByteString tId)
              ]
              ++ w3cTraceContextToHeaders w3cTraceContext
            )
        }
      (Nothing, Just sId, False) ->
        return $ request {
          HTTP.requestHeaders =
            (originalHeaders ++
              [ (TracingHeaders.spanIdHeaderName, Id.toByteString sId)
              ]
              ++ w3cTraceContextToHeaders w3cTraceContext
            )
        }
      _ ->
        return request


w3cTraceContextToHeaders :: Maybe W3CTraceContext -> [HTTPTypes.Header]
w3cTraceContextToHeaders w3cTraceContext =
  case w3cTraceContext of
    Just w3cCtx -> W3CTraceContext.toHeaders w3cCtx
    Nothing     -> []


-- |Sends a command to the worker thread.
enqueueCommand :: InstanaContext -> Command -> IO ()
enqueueCommand context command = do
  -- TODO Maybe we better should use a bounded queue and drop stuff if we can't
  -- keep up. For now, this is an unbounded queue that might turn into a memory
  -- leak if a lot of spans are written and the HTTP requests to the agent can't
  -- keep up.
  let
    commandQueue = InternalContext.commandQueue context
  STM.atomically $ STM.writeTQueue commandQueue command


-- |Makes the given span the currently active span.
pushSpan ::
  InstanaContext
  -> (Maybe SpanStack -> SpanStack)
  -> IO ()
pushSpan context fn = do
  threadId <- Concurrent.myThreadId
  STM.atomically $
    STM.modifyTVar'
      (InternalContext.currentSpans context)
      (\currentSpansPerThread ->
        let
          modifiedStack = fn $ Map.lookup threadId currentSpansPerThread
        in
        Map.insert threadId modifiedStack currentSpansPerThread
      )


-- |Yields the currently active span, taking it of the stack. The span below
-- that will become the new active span (if there is any).
popSpan :: InstanaContext -> SpanKind -> IO (Maybe Span, Maybe String)
popSpan context expectedKind = do
  threadId <- Concurrent.myThreadId
  STM.atomically $ popSpanStm context threadId expectedKind


-- |Yields the currently active span, taking it of the stack. The span below
-- that will become the new active span (if there is any).
popSpanStm ::
  InstanaContext
  -> ThreadId
  -> SpanKind
  -> STM (Maybe Span, Maybe String)
popSpanStm context threadId expectedKind = do
  currentSpansPerThread <- STM.readTVar $ InternalContext.currentSpans context
  let
    oldStack = Map.lookup threadId currentSpansPerThread
    (modifiedStack, poppedSpan, warning) =
      case oldStack of
        Nothing        ->
          -- invalid state, there should be a stack with at least one span on it
          ( SpanStack.empty
          , Nothing
          , Just $ "Invalid state: Trying to pop the span stack while there " ++
                   "is no span stack for this thread yet."
          )
        Just spanStack ->
          SpanStack.popWhenMatches expectedKind spanStack
    updatedSpansPerThread =
      Map.insert threadId modifiedStack currentSpansPerThread
  STM.writeTVar (InternalContext.currentSpans context) updatedSpansPerThread
  return (poppedSpan, warning)


-- |Yields the currently active span without modifying the span stack.
peekSpan :: InstanaContext -> IO (Maybe Span)
peekSpan context = do
  spanMaybe <- readFromSpanStack context SpanStack.peek
  return $ join spanMaybe


-- |Checks whether the SDK has a connection to an Instana agent.
isConnected :: InstanaContext -> IO Bool
isConnected =
  InternalContext.isAgentConnectionEstablished


-- |Provides the currently active span in a simple format fit for external use.
currentSpan :: InstanaContext -> IO (Maybe SimpleSpan)
currentSpan context = do
  span_ <- peekSpan context
  return $ SimpleSpan.convert <$> span_


-- |Retrieves the trace ID of the currently active trace in the current thread.
currentTraceId :: InstanaContext -> IO (Maybe String)
currentTraceId context = do
  traceIdMaybe <- currentTraceIdInternal context
  return $ Id.toString <$> traceIdMaybe


-- |Retrieves the trace ID of the currently active trace in the current thread.
currentTraceIdInternal :: InstanaContext -> IO (Maybe Id)
currentTraceIdInternal context = do
  traceIdMaybe <- readFromSpanStack context SpanStack.readTraceId
  return $ join traceIdMaybe


-- |Retrieves the span ID of the currently active span in the current thread.
currentSpanId :: InstanaContext -> IO (Maybe String)
currentSpanId context = do
  spanIdMaybe <- currentSpanIdInternal context
  return $ Id.toString <$> spanIdMaybe


-- |Retrieves the span ID of the currently active span in the current thread.
currentSpanIdInternal :: InstanaContext -> IO (Maybe Id)
currentSpanIdInternal context = do
  span_ <- peekSpan context
  return $ Span.spanId <$> span_


-- |Retrieves the parent ID of the currently active span in the current thread.
currentParentId :: InstanaContext -> IO (Maybe String)
currentParentId context = do
  parentIdMaybe <- currentParentIdInternal context
  return $ Id.toString <$> parentIdMaybe


-- |Retrieves the parent ID of the currently active span in the current thread.
currentParentIdInternal :: InstanaContext -> IO (Maybe Id)
currentParentIdInternal context = do
  span_ <- peekSpan context
  return $ join $ Span.parentId <$> span_


-- |Checks if tracing is suppressed for the current thread.
isSuppressed :: InstanaContext -> IO Bool
isSuppressed context = do
  suppressedMaybe <- readFromSpanStack context SpanStack.isSuppressed
  return $ Maybe.fromMaybe False suppressedMaybe


-- |Retrieves the W3C trace context attached to currently active span in the
-- current thread.
currentW3cTraceContext :: InstanaContext -> IO (Maybe W3CTraceContext)
currentW3cTraceContext context = do
  w3cTraceContextMaybe <-
    readFromSpanStack context SpanStack.readW3cTraceContext
  return $ join w3cTraceContextMaybe


-- |Reads a value from the currently active span stack.
readFromSpanStack :: InstanaContext -> (SpanStack -> a) -> IO (Maybe a)
readFromSpanStack context accessor = do
  threadId <- Concurrent.myThreadId
  STM.atomically $ readFromSpanStackStm context threadId accessor


-- |Reads a value from the currently active span stack in the given thread.
readFromSpanStackStm ::
  InstanaContext
  -> ThreadId
  -> (SpanStack -> a)
  -> STM (Maybe a)
readFromSpanStackStm context threadId accessor = do
  currentSpansPerThread <- STM.readTVar $ InternalContext.currentSpans context
  let
    maybeStack = Map.lookup threadId currentSpansPerThread
  case maybeStack of
    Nothing ->
      return Nothing
    Just stack ->
      return $ Just $ accessor stack


-- |Applies the given function to the currently active span, replacing it in
-- place with the result of the given function.
modifyCurrentSpan ::
  InstanaContext
  -> (Span -> Span)
  -> IO ()
modifyCurrentSpan context fn = do
  threadId <- Concurrent.myThreadId
  STM.atomically $
    STM.modifyTVar' (InternalContext.currentSpans context)
      (\currentSpansPerThread ->
        let
          stack = Map.lookup threadId currentSpansPerThread
          modifiedStack = mapCurrentSpan fn stack
        in
        Map.insert threadId modifiedStack currentSpansPerThread
      )


-- |Applies the given function to the top item on the given span stack.
mapCurrentSpan :: (Span -> Span) -> Maybe SpanStack -> SpanStack
mapCurrentSpan fn stack =
  Maybe.fromMaybe
    SpanStack.empty
    ((SpanStack.mapTop fn) <$> stack)


-- |Applies the given function to the currently active entry span, even if the
-- currently active span is an intermediate or exit child of that entry span.
-- The entry span will be replaced with the result of the given function.
modifyCurrentEntrySpan ::
  InstanaContext
  -> (Span -> Span)
  -> IO ()
modifyCurrentEntrySpan context fn = do
  threadId <- Concurrent.myThreadId
  STM.atomically $
    STM.modifyTVar' (InternalContext.currentSpans context)
      (\currentSpansPerThread ->
        let
          stack = Map.lookup threadId currentSpansPerThread
          modifiedStack = mapCurrentEntrySpan fn stack
        in
        Map.insert threadId modifiedStack currentSpansPerThread
      )


-- |Applies the given function to the entry span (if present) on the given span
-- stack, even if there is already an intermediate or exit span on top of it.
mapCurrentEntrySpan :: (Span -> Span) -> Maybe SpanStack -> SpanStack
mapCurrentEntrySpan fn stack =
  Maybe.fromMaybe
    SpanStack.empty
    ((SpanStack.mapEntry fn) <$> stack)

