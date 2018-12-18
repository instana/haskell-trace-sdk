{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.StubServer (stubServer) where


import           Control.Monad.IO.Class                  (liftIO)
import           Data.STRef                              (modifySTRef,
                                                          readSTRef)
import           Servant                                 ((:<|>) (..),
                                                          NoContent (NoContent))
import qualified Servant
import qualified System.Exit                             as Exit
import           System.Log.Logger                       (debugM, infoM)
import qualified System.Posix.Process                    as Posix

import           Instana.SDK.AgentStub.DiscoveryRequest  (DiscoveryRequest)
import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import           Instana.SDK.AgentStub.Logging           (agentStubLogger)
import           Instana.SDK.AgentStub.Recorders         (Recorders)
import qualified Instana.SDK.AgentStub.Recorders         as Recorders
import           Instana.SDK.AgentStub.StubAPI           (ResetAPI, StubAPI)
import           Instana.SDK.AgentStub.TraceRequest      (Span)
import           Instana.SDK.AgentStub.Util              (stToServant)


stubServer :: Recorders -> Servant.Server StubAPI
stubServer recorders =
      getPing
 :<|> getRecordedDiscoveries recorders
 :<|> getRecordedAgentReadyRequests recorders
 :<|> getRecordedEntityDataRequests recorders
 :<|> getRecordedSpans recorders
 :<|> postShutdown
 :<|> resetServer recorders


getPing :: Servant.Handler NoContent
getPing = do
  liftIO $ debugM agentStubLogger $ "ping"
  return NoContent


getRecordedDiscoveries :: Recorders -> Servant.Handler [DiscoveryRequest]
getRecordedDiscoveries recorders = do
  recordedDiscoveries <-
    stToServant $ readSTRef $ Recorders.discoveryRecorder recorders
  return recordedDiscoveries


getRecordedAgentReadyRequests :: Recorders -> Servant.Handler [String]
getRecordedAgentReadyRequests recorders = do
  recordedAgentReadyPids <-
    stToServant $ readSTRef $ Recorders.agentReadyRecorder recorders
  return recordedAgentReadyPids


getRecordedEntityDataRequests ::
  Recorders
  -> Servant.Handler [EntityDataRequest]
getRecordedEntityDataRequests recorders = do
  recordedEntityDataRequests <-
    stToServant $ readSTRef $ Recorders.entityDataRecorder recorders
  return recordedEntityDataRequests


getRecordedSpans :: Recorders -> Servant.Handler [Span]
getRecordedSpans recorders = do
  recordedSpans <- stToServant $ readSTRef $ Recorders.spanRecorder recorders
  return recordedSpans


postShutdown :: Servant.Handler NoContent
postShutdown = do
  liftIO $ infoM agentStubLogger $ "AgentStub shutdown requested"
  _ <-liftIO $ Posix.exitImmediately Exit.ExitSuccess
  return NoContent


resetServer ::
  Recorders
  -> Servant.Server ResetAPI
resetServer recorders =
      postResetDiscoveries recorders
 :<|> postResetSpans recorders


postResetDiscoveries :: Recorders -> Servant.Handler NoContent
postResetDiscoveries recorders = do
  liftIO $ debugM agentStubLogger $
    "resetting recorded discoveries, agent ready and entity data requests"
  stToServant $ modifySTRef (Recorders.discoveryRecorder recorders) (\_ -> [])
  stToServant $ modifySTRef (Recorders.agentReadyRecorder recorders) (\_ -> [])
  stToServant $
    modifySTRef (Recorders.entityDataRecorder recorders) (\_ -> [])
  return NoContent


postResetSpans :: Recorders -> Servant.Handler NoContent
postResetSpans recorders = do
  liftIO $ debugM agentStubLogger $ "resetting recorded spans"
  stToServant $ modifySTRef (Recorders.spanRecorder recorders) (\_ -> [])
  return NoContent

