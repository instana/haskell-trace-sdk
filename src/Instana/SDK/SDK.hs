{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.SDK
Description : The main API of the Instana Haskell Trace SDK.

Instana.SDK.SDK is the main API of the Instana Haskell Trace SDK. Use one of
initInstana, initConfiguredInstana, withInstana, or withConfiguredInstana to get
an InstanaContext. Then use the context with any of the withRootEntry,
withEntry, withExit functions for tracing.
-}
module Instana.SDK.SDK
    ( Config
    , InstanaContext
    , addData
    , addDataAt
    , addHttpTracingHeaders
    , addToErrorCount
    , agentHost
    , agentName
    , agentPort
    , completeEntry
    , completeExit
    , defaultConfig
    , forceTransmissionAfter
    , forceTransmissionStartingAt
    , incrementErrorCount
    , initConfiguredInstana
    , initInstana
    , maxBufferedSpans
    , readHttpTracingHeaders
    , startEntry
    , startExit
    , startHttpEntry
    , startRootEntry
    , withConfiguredInstana
    , withEntry
    , withExit
    , withHttpEntry
    , withInstana
    , withRootEntry
    ) where


import           Control.Concurrent             (ThreadId)
import qualified Control.Concurrent             as Concurrent
import           Control.Concurrent.STM         (STM)
import qualified Control.Concurrent.STM         as STM
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Aeson                     (Value, (.=))
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Char8          as BSC8
import qualified Data.List                      as List
import qualified Data.Map.Strict                as Map
import qualified Data.Maybe                     as Maybe
import qualified Data.Sequence                  as Seq
import           Data.Text                      (Text)
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import qualified Network.HTTP.Client            as HTTP
import qualified Network.Socket                 as Socket
import qualified Network.Wai                    as Wai
import           System.Log.Logger              (warningM)
import qualified System.Posix.Process           as Process

import           Instana.SDK.Config
import           Instana.SDK.Internal.Command   (Command)
import qualified Instana.SDK.Internal.Command   as Command
import           Instana.SDK.Internal.Config    (FinalConfig)
import qualified Instana.SDK.Internal.Config    as InternalConfig
import           Instana.SDK.Internal.Context   (ConnectionState (..), InternalContext (InternalContext))
import qualified Instana.SDK.Internal.Context   as InternalContext
import qualified Instana.SDK.Internal.Id        as Id
import           Instana.SDK.Internal.Logging   (instanaLogger)
import qualified Instana.SDK.Internal.Logging   as Logging
import           Instana.SDK.Internal.SpanStack (SpanStack)
import qualified Instana.SDK.Internal.SpanStack as SpanStack
import           Instana.SDK.Internal.Util      ((|>))
import qualified Instana.SDK.Internal.Worker    as Worker
import           Instana.SDK.Span.EntrySpan     (EntrySpan (..))
import qualified Instana.SDK.Span.EntrySpan     as EntrySpan
import           Instana.SDK.Span.ExitSpan      (ExitSpan (ExitSpan))
import qualified Instana.SDK.Span.ExitSpan      as ExitSpan
import           Instana.SDK.Span.NonRootEntry  (NonRootEntry (NonRootEntry))
import qualified Instana.SDK.Span.NonRootEntry  as NonRootEntry
import           Instana.SDK.Span.RootEntry     (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry     as RootEntry
import           Instana.SDK.Span.Span          (Span (..), SpanKind (..))
import qualified Instana.SDK.Span.Span          as Span
import           Instana.SDK.TracingHeaders     (TracingHeaders (TracingHeaders))
import qualified Instana.SDK.TracingHeaders     as TracingHeaders


{-| A container for all the things the Instana SDK needs to do its work.
-}
type InstanaContext = InternalContext


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
  pid <- Process.getProcessID
  Logging.initLogger $ show pid
  commandQueue <- STM.newTQueueIO
  spanQueue <- STM.newTVarIO $ Seq.empty
  connectionState <- STM.newTVarIO $ Unconnected
  fileDescriptor <- STM.newTVarIO $ Nothing
  threadId <- Concurrent.myThreadId
  currentSpans <- STM.newTVarIO $ Map.singleton threadId SpanStack.empty
  -- HTTP.newManager is keep-alive by default (10 connections, we set it to 5)
  manager <- HTTP.newManager $
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 5
      , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ 5000 * 1000
      , HTTP.managerRawConnection =
          HTTP.rawConnectionModifySocket
            (\socket -> do
                let
                  fileDescriptorFromSocket = Socket.fdSocket socket
                STM.atomically $
                  STM.writeTVar fileDescriptor (Just fileDescriptorFromSocket)
            )
      }
  let
    context =
      InternalContext
        { InternalContext.config = conf
        , InternalContext.httpManager = manager
        , InternalContext.commandQueue = commandQueue
        , InternalContext.spanQueue = spanQueue
        , InternalContext.connectionState = connectionState
        , InternalContext.fileDescriptor = fileDescriptor
        , InternalContext.currentSpans = currentSpans
        }
  -- The worker thread will also try to establish the connection to the agent
  -- and only start its work when that was successful.
  Worker.spawnWorker context
  return context


-- |Wraps an IO action in 'startRootEntry' and 'completeEntry'.
withRootEntry ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> m a
  -> m a
withRootEntry context spanName io = do
  startRootEntry context spanName
  result <- io
  completeEntry context
  return result


-- |Wraps an IO action in 'startEntry' and 'completeEntry'.
withEntry ::
  MonadIO m =>
  InstanaContext
  -> String
  -> String
  -> Text
  -> m a
  -> m a
withEntry context traceId parentId spanName io = do
  startEntry context traceId parentId spanName
  result <- io
  completeEntry context
  return result


-- |A convenience function that examines the given request for Instana tracing
-- headers (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers)
-- and wraps the given IO action either in 'startRootEntry' or  'startEntry' and
-- 'completeEntry', depending on the presence of absence of these headers.
withHttpEntry ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> Text
  -> m a
  -> m a
withHttpEntry context request spanName io = do
  let
    tracingHeaders = readHttpTracingHeaders request
    traceId = TracingHeaders.traceId tracingHeaders
    spanId = TracingHeaders.spanId tracingHeaders
    level = TracingHeaders.level tracingHeaders
  case level of
    TracingHeaders.Trace -> do
      let
        io' = addDataFromRequest context request io
      case (traceId, spanId) of
        (Just t, Just s) ->
          withEntry context t s spanName io'
        _                ->
          withRootEntry context spanName io'
    TracingHeaders.Suppress -> do
      liftIO $ pushSpan
        context
        (\stack ->
          case stack of
            Nothing ->
              -- We did not initialise the span stack for this thread, do it
              -- now.
              SpanStack.suppress
            Just spanStack ->
              SpanStack.pushSuppress spanStack
        )
      io


-- |Takes an IO action and appends another side effecto to it that will add HTTP
-- data from the given request to the current span.
addDataFromRequest :: MonadIO m => InstanaContext -> Wai.Request -> m a -> m a
addDataFromRequest context request originalIO =
  originalIO >>= addHttpDataInIO context request


addHttpDataInIO :: MonadIO m => InstanaContext -> Wai.Request -> a -> m a
addHttpDataInIO context request ioResult = do
  addHttpData context request
  return ioResult


addHttpData :: MonadIO m => InstanaContext -> Wai.Request -> m ()
addHttpData context request = do
  let
    host :: String
    host =
      Wai.requestHeaderHost request
      |> fmap BSC8.unpack
      |> Maybe.fromMaybe ""
  addData
    context
    (Aeson.object [ "http" .=
      Aeson.object
        [ "method" .= Wai.requestMethod request |> BSC8.unpack
        , "url"    .= Wai.rawPathInfo request |> BSC8.unpack
        , "host"   .= host
        ]
      ]
    )


-- |Wraps an IO action in 'startExit' and 'completeExit'.
withExit ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> m a
  -> m a
withExit context spanName io = do
  startExit context spanName
  result <- io
  completeExit context
  return result


-- |Creates a preliminary/incomplete root entry span, which should later be
-- completed with 'completeEntry'.
startRootEntry ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> m ()
startRootEntry context spanName = do
  liftIO $ do
    timestamp <- round . (* 1000) <$> getPOSIXTime
    traceId <- Id.generate
    let
      newSpan =
        RootEntrySpan $
          RootEntry
            { RootEntry.spanAndTraceId = traceId
            , RootEntry.spanName       = spanName
            , RootEntry.timestamp      = timestamp
            , RootEntry.errorCount     = 0
            , RootEntry.spanData       = emptyValue
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
  -> Text
  -> m ()
startEntry context traceId parentId spanName = do
  liftIO $ do
    timestamp <- round . (* 1000) <$> getPOSIXTime
    spanId <- Id.generate
    let
      newSpan =
        NonRootEntrySpan $
          NonRootEntry
            { NonRootEntry.traceId    = Id.fromString traceId
            , NonRootEntry.spanId     = spanId
            , NonRootEntry.parentId   = Id.fromString parentId
            , NonRootEntry.spanName   = spanName
            , NonRootEntry.timestamp  = timestamp
            , NonRootEntry.errorCount = 0
            , NonRootEntry.spanData   = emptyValue
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
    return ()


-- |A convenience function that examines the given request for Instana tracing
-- headers (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers)
-- and either calls 'startRootEntry' or  'startEntry', depending on the presence
-- of absence of these headers.
startHttpEntry ::
  MonadIO m =>
  InstanaContext
  -> Wai.Request
  -> Text
  -> m ()
startHttpEntry context request spanName = do
  let
    tracingHeaders = readHttpTracingHeaders request
    traceId = TracingHeaders.traceId tracingHeaders
    spanId = TracingHeaders.spanId tracingHeaders
    level = TracingHeaders.level tracingHeaders
  case level of
    TracingHeaders.Trace ->
      case (traceId, spanId) of
        (Just t, Just s) -> do
          startEntry context t s spanName
          addHttpData context request
        _                -> do
          startRootEntry context spanName
          addHttpData context request
    TracingHeaders.Suppress -> do
      liftIO $ pushSpan
        context
        (\stack ->
          case stack of
            Nothing ->
              -- We did not initialise the span stack for this thread, do it now.
              SpanStack.suppress
            Just spanStack ->
              SpanStack.pushSuppress spanStack
        )


-- |Creates a preliminary/incomplete exit span, which should later be completed
-- with 'completeExit'.
startExit ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> m ()
startExit context spanName = do
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
            newSpan =
              ExitSpan
                { ExitSpan.parentSpan  = parent
                , ExitSpan.spanId      = spanId
                , ExitSpan.spanName    = spanName
                , ExitSpan.timestamp   = timestamp
                , ExitSpan.errorCount  = 0
                , ExitSpan.spanData    = emptyValue
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
            "Cannot start exit span \"" ++ show spanName ++
            "\" since there is already an active exit span " ++
            "in progress: " ++ show ex
        Nothing -> do
          warningM instanaLogger $
            "Cannot start exit span \"" ++ show spanName ++
            "\" since there is no active entry span " ++
            "(actually, there is no active span at all)."
          return ()


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


-- |Adds additional custom data to the currently active span. Call this
-- between startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to with withEntry/withExit/withRootEntry.
-- The given path can be a nested path, with path fragments separated by dots,
-- like "http.url". This will result in
-- "data": {
--   ...
--   "http": {
--     "url": "..."
--   },
--   ...
-- }
addDataAt :: (MonadIO m, Aeson.ToJSON a) => InstanaContext -> Text -> a -> m ()
addDataAt context path value =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addDataAt path value span_)


-- |Adds additional custom data to the currently active span. Call this
-- between startEntry/startRootEntry/startExit and completeEntry/completeExit or
-- inside the IO action given to with withEntry/withExit/withRootEntry. Can be
-- called multiple times, data from multiple calls will be merged.
addData :: MonadIO m => InstanaContext -> Value -> m ()
addData context value =
  liftIO $ modifyCurrentSpan context
    (\span_ -> Span.addData value span_)


-- |Reads the Instana tracing headers
-- (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers) from
-- the given request.
readHttpTracingHeaders :: Wai.Request -> TracingHeaders
readHttpTracingHeaders request =
  let
    headers = Wai.requestHeaders request
    -- lookup is automatically case insensitive because
    -- HeaderName = CI ByteString (CI -> Case Insensitive String)
    traceId =
      headers
      |> List.lookup TracingHeaders.traceIdHeaderName
      |> (<$>) BSC8.unpack
    spanId =
      headers
      |> List.lookup TracingHeaders.spanIdHeaderName
      |> (<$>) BSC8.unpack
    level =
      headers
      |> List.lookup TracingHeaders.levelHeaderName
      |> (<$>) BSC8.unpack
  in
  TracingHeaders
    { TracingHeaders.traceId = traceId
    , TracingHeaders.spanId = spanId
    , TracingHeaders.level = TracingHeaders.maybeStringToTracingLevel level
    }


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
    entrySpan <- peekSpan context
    let
      originalHeaders = HTTP.requestHeaders request
      updatedRequest =
        case (entrySpan, suppressed) of
          (_, True) ->
            request {
              HTTP.requestHeaders =
                ((TracingHeaders.levelHeaderName, "0") : originalHeaders)
            }
          (Just (Entry currentEntrySpan), _) ->
            request {
              HTTP.requestHeaders =
                (originalHeaders ++
                  [ (TracingHeaders.traceIdHeaderName, Id.toByteString $
                      EntrySpan.traceId currentEntrySpan)
                  , (TracingHeaders.spanIdHeaderName, Id.toByteString $
                      EntrySpan.spanId currentEntrySpan)
                  ]
                )
            }
          _ ->
            request
    return updatedRequest


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
  threadId <- Concurrent.myThreadId
  STM.atomically $ peekSpanStm context threadId


-- |Yields the currently active span without modifying the span stack.
peekSpanStm :: InstanaContext -> ThreadId -> STM (Maybe Span)
peekSpanStm context threadId = do
  currentSpansPerThread <- STM.readTVar $ InternalContext.currentSpans context
  let
    stack = Map.lookup threadId currentSpansPerThread
  case stack of
    Nothing ->
      return Nothing
    Just s ->
      return $ SpanStack.peek s


-- |Checks if tracing is suppressed for the current thread.
isSuppressed :: InstanaContext -> IO Bool
isSuppressed context = do
  threadId <- Concurrent.myThreadId
  STM.atomically $ isSuppressedStm context threadId


-- |Checks if tracing is suppressed for the current thread.
isSuppressedStm :: InstanaContext -> ThreadId -> STM Bool
isSuppressedStm context threadId = do
  currentSpansPerThread <- STM.readTVar $ InternalContext.currentSpans context
  let
    stack = Map.lookup threadId currentSpansPerThread
  case stack of
    Nothing ->
      return False
    Just s ->
      return $ SpanStack.isSuppressed s


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


-- |Applies the given function to the given span.
mapCurrentSpan :: (Span -> Span) -> Maybe SpanStack -> SpanStack
mapCurrentSpan fn stack =
  Maybe.fromMaybe
    SpanStack.empty
    ((SpanStack.mapTop fn) <$> stack)


-- |Provides an empty Aeson value.
emptyValue :: Value
emptyValue = Aeson.object []

