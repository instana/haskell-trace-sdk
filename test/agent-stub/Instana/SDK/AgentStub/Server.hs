{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.Server (mainServer) where


import           Control.Monad.IO.Class                  (liftIO)
import           Data.List                               (isPrefixOf)
import qualified Data.List                               as List
import           Data.Maybe                              (fromMaybe)
import           Data.STRef                              (modifySTRef,
                                                          readSTRef)
import           Data.Time.Clock.POSIX                   (getPOSIXTime)
import           Servant                                 (Header, Headers,
                                                          NoContent (NoContent),
                                                          err404, err503,
                                                          (:<|>) (..))
import qualified Servant
import           System.Log.Logger                       (debugM, warningM)
import           Text.Read                               (readMaybe)

import           Instana.SDK.AgentStub.API               (API)
import           Instana.SDK.AgentStub.Config            (AgentStubConfig)
import qualified Instana.SDK.AgentStub.Config            as Config
import           Instana.SDK.AgentStub.DiscoveryRequest  (DiscoveryRequest)
import qualified Instana.SDK.AgentStub.DiscoveryRequest  as DiscoveryRequest
import           Instana.SDK.AgentStub.DiscoveryResponse (DiscoveryResponse (DiscoveryResponse),
                                                          TracingConfig (TracingConfig))
import qualified Instana.SDK.AgentStub.DiscoveryResponse as DiscoveryResponse
import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import           Instana.SDK.AgentStub.Logging           (agentStubLogger)
import           Instana.SDK.AgentStub.Recorders         (Recorders)
import qualified Instana.SDK.AgentStub.Recorders         as Recorders
import qualified Instana.SDK.AgentStub.StubServer        as StubServer
import           Instana.SDK.AgentStub.TraceRequest      (TraceRequest)
import           Instana.SDK.AgentStub.Util              (shouldSimulateConnectionLoss,
                                                          stToServant)


mainServer ::
  AgentStubConfig
  -> Int
  -> Recorders
  -> Servant.Server API
mainServer config startupTime recorders =
      getRoot
 :<|> putDiscovery config startupTime recorders
 :<|> headAgentReady config startupTime recorders
 :<|> postEntityData config startupTime recorders
 :<|> postTrace config startupTime recorders
 :<|> StubServer.stubServer recorders


getRoot :: Servant.Handler (Headers '[Header "Server" String] NoContent)
getRoot =
  return $ Servant.addHeader "Instana Agent" NoContent


putDiscovery ::
  AgentStubConfig
  -> Int
  -> Recorders
  -> DiscoveryRequest
  -> Servant.Handler DiscoveryResponse
putDiscovery config startupTime recorders discoveryRequest = do
  now <- liftIO $ round . (* 1000) <$> getPOSIXTime
  if shouldSimulateConnectionLoss config startupTime now
    then do
      liftIO $ debugM agentStubLogger $
        "Rejecting PUT discovery to simulate connection loss."
      Servant.throwError err503
    else do
      liftIO $
        debugM agentStubLogger $ "PUT discovery " ++ show discoveryRequest
      let
        pidStr = DiscoveryRequest.pid discoveryRequest
        pid = fromMaybe 0 $ readMaybe pidStr
        translatedPid =
          if Config.simulatPidTranslation config
           then pid + 1
           else pid
        translatedDiscoveryRequest =
          discoveryRequest { DiscoveryRequest.pid = show translatedPid }
        tracingConfig =
          if length (Config.extraHeadersViaTracingConfig config) > 0
            then
              Just TracingConfig
                { DiscoveryResponse.extraHttpHeaders =
                    Just $ Config.extraHeadersViaTracingConfig config
                }
            else
              Nothing

        extraHeadersLegacyConfig =
          if length (Config.extraHeadersViaLegacyConfig config) > 0
            then
              Just $ Config.extraHeadersViaLegacyConfig config
            else
              Nothing

      stToServant $
        modifySTRef
          (Recorders.discoveryRecorder recorders)
          ((++) [translatedDiscoveryRequest])

      return $
        DiscoveryResponse
          { DiscoveryResponse.pid = translatedPid
          , DiscoveryResponse.agentUuid = "agent-stub-id"
          , DiscoveryResponse.tracing = tracingConfig
          , DiscoveryResponse.extraHeaders = extraHeadersLegacyConfig
          , DiscoveryResponse.secrets = Config.secretsConfig config
          -- add a bogus attribute to make sure the trace SDK ignores unknown
          -- attributes in the announce response
          , DiscoveryResponse.unknownAttribute = "whatever"
          }


headAgentReady ::
  AgentStubConfig
  -> Int
  -> Recorders
  -> String
  -> Servant.Handler NoContent
headAgentReady
    config
    startupTime
    recorders
    pidString
     = do
  now <- liftIO $ round . (* 1000) <$> getPOSIXTime
  if shouldSimulateConnectionLoss config startupTime now
    then do
      liftIO $ debugM agentStubLogger $
        "Rejecting HEAD agent ready to simulate connection loss."
      Servant.throwError err503
    else do
      liftIO $ debugM agentStubLogger $ "HEAD " ++ pidString
      recordedDiscoveries <-
        stToServant $ readSTRef $ Recorders.discoveryRecorder recorders
      let
        pidStrWithoutPrefix =
          if isPrefixOf "com.instana.plugin.haskell." pidString
            then drop 27 pidString else ""
        knownPids = List.map DiscoveryRequest.pid recordedDiscoveries
        pidHasBeenAnnounced = elem pidStrWithoutPrefix knownPids
      if pidHasBeenAnnounced
        then do
          stToServant $
            modifySTRef
              (Recorders.agentReadyRecorder recorders)
              ((++) [pidStrWithoutPrefix])
          return NoContent
        else do
          liftIO $ warningM agentStubLogger $
            "Rejecting agent ready request for unannounced PID " ++
              pidStrWithoutPrefix
          Servant.throwError err404


postEntityData ::
  AgentStubConfig
  -> Int
  -> Recorders
  -> String
  -> EntityDataRequest
  -> Servant.Handler NoContent
postEntityData
    config
    startupTime
    recorders
    pidString
    entityData
     = do
  now <- liftIO $ round . (* 1000) <$> getPOSIXTime
  if shouldSimulateConnectionLoss config startupTime now
    then do
      liftIO $ debugM agentStubLogger $
        "Rejecting POST entity data to simulate connection loss."
      Servant.throwError err503
    else do
      liftIO $ debugM agentStubLogger $ "POST entity data " ++ pidString
      recordedDiscoveries <-
        stToServant $ readSTRef $ Recorders.discoveryRecorder recorders
      let
        pidStrWithoutPrefix =
          if isPrefixOf "com.instana.plugin.haskell." pidString
            then drop 27 pidString else ""
        knownPids = List.map DiscoveryRequest.pid recordedDiscoveries
        pidHasBeenAnnounced = elem pidStrWithoutPrefix knownPids
      if pidHasBeenAnnounced
        then do
          liftIO $
            debugM agentStubLogger $
              "accepting entity data " ++ show entityData
          stToServant $
            modifySTRef
              (Recorders.entityDataRecorder recorders)
              ((++) [entityData])
          return NoContent
        else do
          liftIO $ warningM agentStubLogger $
            "Rejecting entity data request for unannounced PID " ++
              pidStrWithoutPrefix
          Servant.throwError err404


postTrace ::
  AgentStubConfig
  -> Int
  -> Recorders
  -> String
  -> TraceRequest
  -> Servant.Handler NoContent
postTrace
    config
    startupTime
    recorders
    pidString
    traceRequest = do
  now <- liftIO $ round . (* 1000) <$> getPOSIXTime
  if shouldSimulateConnectionLoss config startupTime now
    then do
      liftIO $ debugM agentStubLogger $
        "Rejecting POST trace to simulate connection loss."
      Servant.throwError err503
    else do
      liftIO $ debugM agentStubLogger $ "POST " ++ pidString ++
        show traceRequest
      recordedDiscoveries <-
        stToServant $ readSTRef $ Recorders.discoveryRecorder recorders
      let
        pidStrWithoutTracesPrefix =
          if isPrefixOf "traces." pidString then drop 7 pidString else ""
        knownPids = List.map DiscoveryRequest.pid recordedDiscoveries
        pidHasBeenAnnounced = elem pidStrWithoutTracesPrefix knownPids
      if pidHasBeenAnnounced
        then do
          stToServant $
            modifySTRef (Recorders.spanRecorder recorders) ((++) traceRequest)
          return NoContent
        else do
          liftIO $ warningM agentStubLogger $
            "Rejecting trace for unannounced PID " ++ pidStrWithoutTracesPrefix
          Servant.throwError err404

