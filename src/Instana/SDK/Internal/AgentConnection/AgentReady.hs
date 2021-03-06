{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.AgentReady
Description : Handles the agent ready phase for establishing the connection to
the agent.
-}
module Instana.SDK.Internal.AgentConnection.AgentReady
    ( waitUntilAgentIsReadyToAcceptData
    ) where


import qualified Control.Concurrent.STM                                     as STM
import           Control.Exception                                          (SomeException,
                                                                             catch)
import           Data.ByteString.Lazy                                       (ByteString)
import qualified Network.HTTP.Client                                        as HTTP
import           System.Log.Logger                                          (debugM,
                                                                             infoM,
                                                                             warningM)

import           Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse (AnnounceResponse)
import qualified Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse as AnnounceResponse
import           Instana.SDK.Internal.AgentConnection.Json.Util             (emptyResponseDecoder)
import           Instana.SDK.Internal.AgentConnection.Paths                 (haskellEntityDataPathPrefix)
import           Instana.SDK.Internal.AgentConnection.ProcessInfo           (ProcessInfo)
import           Instana.SDK.Internal.Context                               (ConnectionState (..),
                                                                             InternalContext)
import qualified Instana.SDK.Internal.Context                               as InternalContext
import           Instana.SDK.Internal.Logging                               (instanaLogger)
import qualified Instana.SDK.Internal.Metrics.Collector                     as MetricsCollector
import qualified Instana.SDK.Internal.Retry                                 as Retry
import qualified Instana.SDK.Internal.URL                                   as URL


-- |Starts the connection establishment phase where we wait for the agent to
-- signal that it is ready to accept data.
waitUntilAgentIsReadyToAcceptData ::
  InternalContext
  -> String
  -> ProcessInfo
  -> AnnounceResponse
  -> IO ()
waitUntilAgentIsReadyToAcceptData
    context
    originalPidStr
    processInfo
    announceResponse = do
  debugM instanaLogger "Waiting until the agent is ready to accept data."
  connectionState <-
      STM.atomically $ STM.readTVar $ InternalContext.connectionState context
  case connectionState of
    Announced hostAndPort ->
      waitForAgent
        context
        hostAndPort
        originalPidStr
        processInfo
        announceResponse
    _ -> do
      warningM instanaLogger $
        "Reached illegal state in agent ready, announce did not " ++
        "yield a host and port. Connection state is " ++ show connectionState ++
        ". Will retry later."
      STM.atomically $ STM.writeTVar
        (InternalContext.connectionState context)
        Unconnected
      return ()


waitForAgent ::
  InternalContext
  -> (String, Int)
  -> String
  -> ProcessInfo
  -> AnnounceResponse
  -> IO ()
waitForAgent
    context
    (host, port)
    originalPidStr
    processInfo
    announceResponse = do
  let
    translatedPidStr = show $ AnnounceResponse.pid announceResponse
    pidTranslationStr =
      if translatedPidStr == originalPidStr
      then translatedPidStr
      else "(" ++ originalPidStr ++ " => " ++ translatedPidStr ++ ")"
    acceptDataUrl =
      URL.mkHttp host port (haskellEntityDataPathPrefix ++ translatedPidStr)
  agentReadyRequestBase <- HTTP.parseUrlThrow $ show acceptDataUrl
  let
    acceptDataRequest = agentReadyRequestBase
       { HTTP.method = "HEAD"
       , HTTP.requestHeaders =
         [ ("Accept", "application/json")
         , ("Content-Type", "application/json; charset=UTF-8'")
         ]
       }
    manager = InternalContext.httpManager context
    acceptDataRequestAction :: IO (HTTP.Response ByteString)
    acceptDataRequestAction = HTTP.httpLbs acceptDataRequest manager

  success <-
    catch
      (Retry.retryRequest
        Retry.acceptDataRetryPolicy
        emptyResponseDecoder
        acceptDataRequestAction
      )
      (\e -> do
        warningM instanaLogger $ show (e :: SomeException)
        return False
      )
    -- if 200 <= statusCode <= 299 then we assume everything is sweet and we
    -- transition to next state

  if success
    then do
      metricsStore <-
        MetricsCollector.registerMetrics
          translatedPidStr
          processInfo
          (InternalContext.sdkStartTime context)
      let
         state =
           InternalContext.mkAgentReadyState
             (host, port)
             announceResponse
             metricsStore
      STM.atomically $
        STM.writeTVar (InternalContext.connectionState context) state
      infoM instanaLogger $
        "Agent connection established for process " ++ pidTranslationStr
      return ()
  else do
    warningM instanaLogger $
      "Could not establish agent connection for process " ++
      pidTranslationStr ++ " (waiting for agent ready state failed), will " ++
      "retry later."
    STM.atomically $ STM.writeTVar
      (InternalContext.connectionState context)
      Unconnected

