{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Internal.Worker
Description : Manages the SDKs background worker threads
-}
module Instana.SDK.Internal.Worker
    ( spawnWorker
    ) where


import qualified Control.Concurrent                               as Concurrent
import qualified Control.Concurrent.STM                           as STM
import           Control.Exception                                (SomeException,
                                                                   catch)
import           Control.Monad                                    (forever,
                                                                   when)
import qualified Data.Aeson                                       as Aeson
import           Data.Foldable                                    (toList)
import qualified Data.HashMap.Strict                              as HashMap
import           Data.List                                        (map)
import           Data.Sequence                                    ((|>))
import qualified Data.Sequence                                    as Seq
import           Data.Text                                        (Text)
import           Data.Time.Clock.POSIX                            (getPOSIXTime)
import qualified Network.HTTP.Client                              as HTTP
import qualified Network.HTTP.Types.Status                        as HttpTypes
import           System.Log.Logger                                (debugM,
                                                                   warningM)
import qualified System.Metrics                                   as Metrics
import qualified System.Metrics.Json                              as Metrics.Json

import qualified Instana.SDK.Internal.AgentConnection.ConnectLoop as ConnectLoop
import           Instana.SDK.Internal.AgentConnection.Paths       (haskellEntityDataPathPrefix,
                                                                   haskellTracePluginPath)
import           Instana.SDK.Internal.Command                     (Command (..))
import qualified Instana.SDK.Internal.Config                      as InternalConfig
import           Instana.SDK.Internal.Context                     (ConnectionState (..),
                                                                   InternalContext)
import qualified Instana.SDK.Internal.Context                     as InternalContext
import           Instana.SDK.Internal.FullSpan                    (FullSpan (FullSpan),
                                                                   FullSpanWithPid (FullSpanWithPid),
                                                                   SpanKind (Entry, Exit))
import qualified Instana.SDK.Internal.FullSpan                    as FullSpan
import           Instana.SDK.Internal.Logging                     (instanaLogger)
import qualified Instana.SDK.Internal.Metrics.Collector           as MetricsCollector
import qualified Instana.SDK.Internal.Metrics.Compression         as MetricsCompression
import qualified Instana.SDK.Internal.URL                         as URL
import           Instana.SDK.Span.EntrySpan                       (EntrySpan (..))
import qualified Instana.SDK.Span.EntrySpan                       as EntrySpan
import           Instana.SDK.Span.ExitSpan                        (ExitSpan (..))
import qualified Instana.SDK.Span.ExitSpan                        as ExitSpan


-- |Spawns the SDK's worker. There should only be one worker at any time.
spawnWorker :: InternalContext -> IO()
spawnWorker context = do
  debugM instanaLogger "Spawning the Instana Haskell SDK worker"

  -- The worker starts five threads, which continuously:
  --
  -- 1) Check if the connection to the agent is already/still up. If not, this
  --    thread will start to establish a connection to the agent.
  _ <- Concurrent.forkIO $ ConnectLoop.initConnectLoop context

  -- 2) Read commands (incoming spans) from the command queue and put arriving
  --    spans into the worker's local span buffer. This will happen regardless
  --    of the agent connection state. If a certain amount of spans are in the
  --    local buffer, we'll drain the buffer and, if connected, try to send the
  --    spans to the agent. If not connected, these spans will be dropped. This
  --    avoids excessive memory consumption at the expense of losing spans.
  _ <- Concurrent.forkIO $ initReadLoop context

  -- 3) Drain the local span buffer once every second and try to send the
  --    buffered spans, if any. Again, sending the spans will only be attempted
  --    when connected, see above.
  _ <- Concurrent.forkIO $ initDrainSpanBufferAfterTimeoutLoop context

  -- 4) Collect and send metrics, if connected.
  _ <- Concurrent.forkIO $ collectAndSendMetricsLoop context

  -- 5) Make sure full metrics instad of diffs get send every five minutes
  _ <- Concurrent.forkIO $ resetPreviouslySendMetrics context

  return ()


{-| Read commands (incoming spans) from the command queue and put arriving spans
into the worker's local span buffer. This will happen regardless of the agent
connection state. If a certain amount of spans are in the local buffer, we'll
drain the buffer and, if connected, try to send the spans to the agent. If not
connected, these spans will be dropped. This avoids excessive memory consumption
at the expense of losing spans.
-}
initReadLoop :: InternalContext -> IO()
initReadLoop context =
  forever $ readFromQueue context


readFromQueue :: InternalContext -> IO ()
readFromQueue context =
  catch
    ( do
        let
          commandQueue = InternalContext.commandQueue context
        command <- STM.atomically $ STM.readTQueue commandQueue
        execute command context
    )
    -- exceptions in execute (or reading from queue) must not kill the loop, so
    -- we just catch everything
    (\e -> warningM instanaLogger $ show (e :: SomeException))


execute :: Command -> InternalContext -> IO ()
execute (CompleteEntry entrySpan) =
  queueEntrySpan entrySpan
execute (CompleteExit exitSpan) =
  queueExitSpan exitSpan


queueEntrySpan :: EntrySpan -> InternalContext -> IO ()
queueEntrySpan entrySpan context = do
  now <- round . (* 1000) <$> getPOSIXTime
  let
    timestamp = EntrySpan.timestamp entrySpan
  queueSpan
    context
    FullSpan
      { FullSpan.traceId    = EntrySpan.traceId entrySpan
      , FullSpan.spanId     = EntrySpan.spanId entrySpan
      , FullSpan.parentId   = EntrySpan.parentId entrySpan
      , FullSpan.spanName   = EntrySpan.spanName entrySpan
      , FullSpan.timestamp  = timestamp
      , FullSpan.duration   = now - timestamp
      , FullSpan.kind       = Entry
      , FullSpan.errorCount = EntrySpan.errorCount entrySpan
      , FullSpan.spanData   = EntrySpan.spanData entrySpan
      }


queueExitSpan :: ExitSpan -> InternalContext -> IO ()
queueExitSpan exitSpan context = do
  let
    parentSpan = ExitSpan.parentSpan exitSpan
  now <- round . (* 1000) <$> getPOSIXTime
  queueSpan
    context
    FullSpan
      { FullSpan.traceId    = EntrySpan.traceId parentSpan
      , FullSpan.spanId     = ExitSpan.spanId exitSpan
      , FullSpan.parentId   = Just $ EntrySpan.spanId parentSpan
      , FullSpan.spanName   = ExitSpan.spanName exitSpan
      , FullSpan.timestamp  = ExitSpan.timestamp exitSpan
      , FullSpan.duration   = now - ExitSpan.timestamp exitSpan
      , FullSpan.kind       = Exit
      , FullSpan.errorCount = ExitSpan.errorCount exitSpan
      , FullSpan.spanData   = ExitSpan.spanData exitSpan
      }


queueSpan :: InternalContext -> FullSpan -> IO ()
queueSpan context span_ = do
  currentSpanQueue <-
    STM.atomically $
      STM.readTVar $
        InternalContext.spanQueue context
  let
    bufferedSpans = Seq.length currentSpanQueue
    maxBufferedSpans =
      InternalConfig.maxBufferedSpans $ InternalContext.config context
    forceTransmissionStartingAt =
      InternalConfig.forceTransmissionStartingAt $
        InternalContext.config context
  if bufferedSpans >= maxBufferedSpans
  then do
    -- TODO remove debug log?
    debugM instanaLogger "dropping span, buffer limit reached"
    return ()
  else do
    STM.atomically $
      STM.modifyTVar
        (InternalContext.spanQueue context)
        (\q -> q |> span_)
    when
      (bufferedSpans + 1 >= forceTransmissionStartingAt)
      (drainSpanBuffer context)


{-| Drain the local span buffer once every second and try to send the buffered
spans to the agent, if any. Sending the spans will only be attempted when the
connection to the agent is up, otherwise, the locally buffered spans will be
dropped. This avoids excessive memory consumption at the expense of losing
spans.
-}
initDrainSpanBufferAfterTimeoutLoop :: InternalContext -> IO()
initDrainSpanBufferAfterTimeoutLoop context = do
  let
    delayMilliSeconds =
      InternalConfig.forceTransmissionAfter $ InternalContext.config context
    delayMicroSeconds = delayMilliSeconds * 1000
  forever $ drainSpanBufferAfterTimeoutLoop delayMicroSeconds context


drainSpanBufferAfterTimeoutLoop :: Int -> InternalContext -> IO()
drainSpanBufferAfterTimeoutLoop delayMicroSeconds context = do
  catch
    ( do
        drainSpanBuffer context
    )
    -- exceptions in drainSpanBuffer must not kill the loop, so we just catch
    -- everything
    (\e -> warningM instanaLogger $ show (e :: SomeException))
  Concurrent.threadDelay delayMicroSeconds


drainSpanBuffer :: InternalContext ->  IO ()
drainSpanBuffer context = do
  spansSeq <- STM.atomically $
    STM.swapTVar (InternalContext.spanQueue context) Seq.empty
  let
    spans :: [FullSpan]
    spans = toList spansSeq
  when (not $ null spans) $ do
    InternalContext.whenConnected context $
      sendSpansToAgent context spans


sendSpansToAgent ::
  InternalContext
  -> [FullSpan]
  -> String
  -> Text
  -> Metrics.Store
  -> IO ()
sendSpansToAgent context spans pid _ _ = do
  let
    config = InternalContext.config context
    traceEndpointUrl =
      (show $
        URL.mkHttp
          (InternalConfig.agentHost config)
          (InternalConfig.agentPort config)
          haskellTracePluginPath
      ) ++ "." ++ pid
    -- combine actual span data with static per-process data (e.g. PID)
    spansWithPid = map
      (\fullSpan ->
        FullSpanWithPid {
          FullSpan.fullSpan = fullSpan
        , FullSpan.pid      = pid
        }
      ) spans
  defaultRequestSettings <- HTTP.parseUrlThrow traceEndpointUrl
  let
    request =
      defaultRequestSettings
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode spansWithPid
        , HTTP.requestHeaders =
          [ ("Accept", "application/json")
          , ("Content-Type", "application/json; charset=UTF-8'")
          ]
        }

  -- TODO Would we want to retry the request? Or do we just lose this batch
  -- of spans? Perhaps do 3 quick retries before giving up, maybe excluding a
  -- 404 response status (because that clearly indicates that a new connection
  -- establishment process has to be initiated.

  -- Right now, if sending the spans fails (either in the context of
  -- drainSpanBufferAfterTimeoutLoop or queueSpan), the exception will be
  -- logged and we move on.
  -- That means that if the agent is unavailable for some time, we will try to
  -- send spans at least every second regardless (or even more frequent, if the
  -- monitored application creates more than 1000 spans per second).
  -- We could do something more sophisticated here like trying to send spans
  -- less frequently after a number of failed attempts. Maybe we could use
  -- http://hackage.haskell.org/package/glue-0.2.0/docs/Glue-CircuitBreaker.html
  --
  -- Also, as a small performance improvement, we might want to stop _accepting_
  -- spans until the connection has been reestablished. (To avoid serializing
  -- a lot of spans to JSON if they can not be send due to agent
  -- unavailability.)
  catch
    (do
      _ <- HTTP.httpLbs request $ InternalContext.httpManager context
      return ()
    )
    (\(e :: HTTP.HttpException) -> do
      let
        statusCode =
          case e of
            HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _) ->
              HttpTypes.statusCode (HTTP.responseStatus response)
            _ ->
              0
      if statusCode == 404
        then
          resetToUnconnected context
        else do
          debugM instanaLogger $ show e
          return ()
    )


collectAndSendMetricsLoop :: InternalContext -> IO()
collectAndSendMetricsLoop context = do
  let
    delayMicroSeconds = 1000 * 1000 -- once each second
  -- offset metrics collection from span buffer draining
  Concurrent.threadDelay $ 500 * 1000
  forever $ collectAndSendMetricsSafe delayMicroSeconds context


-- |Resets the previously send metrics to an empty map so we send the full set
-- of metrics the next time (instead of just the diff).
resetPreviouslySendMetrics :: InternalContext -> IO()
resetPreviouslySendMetrics context = do
  let
    delayMicroSeconds = 5 * 60 * 1000 * 1000 -- once every five minutes
  forever $ do
    catch
      (STM.atomically $ STM.writeTVar
         (InternalContext.previousMetrics context)
         HashMap.empty
      )
      (\e -> warningM instanaLogger $ show (e :: SomeException))
    Concurrent.threadDelay delayMicroSeconds


collectAndSendMetricsSafe :: Int -> InternalContext -> IO()
collectAndSendMetricsSafe delayMicroSeconds context = do
  catch
    ( do
        collectAndSendMetricsWhenConnected context
    )
    -- exceptions in collectAndSendMetrics must not kill the loop, so we just catch
    -- everything
    (\e -> warningM instanaLogger $ show (e :: SomeException))
  Concurrent.threadDelay delayMicroSeconds


collectAndSendMetricsWhenConnected :: InternalContext -> IO ()
collectAndSendMetricsWhenConnected context =
  InternalContext.whenConnected context $
    collectAndSendMetrics context


collectAndSendMetrics ::
  InternalContext
  -> String
  -> Text
  -> Metrics.Store
  -> IO ()
collectAndSendMetrics context translatedPidStr _ metricsStore = do
  previousMetrics <-
    STM.atomically $ STM.readTVar $ InternalContext.previousMetrics context
  sampledMetrics <- MetricsCollector.sampleAll metricsStore
  let
    compressedMetrics =
      MetricsCompression.compressSample previousMetrics sampledMetrics
    metricsAsJson = Metrics.Json.Sample compressedMetrics
    config = InternalContext.config context
    metricsEndpointUrl =
      URL.mkHttp
        (InternalConfig.agentHost config)
        (InternalConfig.agentPort config)
        (haskellEntityDataPathPrefix ++ translatedPidStr)
  defaultRequestSettings <- HTTP.parseUrlThrow $ show metricsEndpointUrl
  let
    request =
      defaultRequestSettings
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode metricsAsJson
        , HTTP.requestHeaders =
          [ ("Accept", "application/json")
          , ("Content-Type", "application/json; charset=UTF-8'")
          ]
        }

  -- TODO Would we want to retry the request? Or do we just lose this batch
  -- of data? Perhaps do 3 quick retries before giving up, maybe excluding a
  -- 404 response status (because that clearly indicates that a new connection
  -- establishment process has to be initiated.

  -- Right now, if sending the data fails, the exception will be logged and we
  -- move on.
  -- That means that if the agent is unavailable for some time, we will try to
  -- send data at least every second regardless.
  -- We could do something more sophisticated here like trying to send spans
  -- less frequently after a number of failed attempts. Maybe we could use
  -- http://hackage.haskell.org/package/glue-0.2.0/docs/Glue-CircuitBreaker.html
  --
  -- Also, as a small performance improvement, we might want to stop
  -- _collecting_ metrics until the connection has been reestablished.
  catch
    (do
      _ <- HTTP.httpLbs request $ InternalContext.httpManager context
      _ <- STM.atomically $
             STM.writeTVar
               (InternalContext.previousMetrics context)
               sampledMetrics
      return ()
    )
    (\(e :: HTTP.HttpException) -> do
      let
        statusCode =
          case e of
            HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _) ->
              HttpTypes.statusCode (HTTP.responseStatus response)
            _ ->
              0
      if statusCode == 404
        then
          resetToUnconnected context
        else do
          debugM instanaLogger $ show e
          return ()
    )


-- |Resets the agent connection to unconnected but only if it is currently
-- in state AgentReady (that is, the connection is fully established). This
-- triggers a new sensor agent handshake. The reasoning behind resetting it only
-- when it is in state AgentReady is that we do not want to interfer with any
-- attempts to establish a connection that is currently in flight, we only want
-- to record the fact that we have lost the connection if we thought we were
-- connected but are not.
resetToUnconnected :: InternalContext -> IO ()
resetToUnconnected context = do
  STM.atomically $
    STM.modifyTVar'
      (InternalContext.connectionState context)
      (\state ->
        case state of
          AgentReady _ ->
            Unconnected
          _         ->
            state
      )

