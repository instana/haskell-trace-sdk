{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.StubServer (stubServer) where


import           Control.Monad.IO.Class                 (liftIO)
import           Data.STRef                             (modifySTRef, readSTRef)
import           Servant                                ((:<|>) (..),
                                                         NoContent (NoContent))
import qualified Servant
import           System.Log.Logger                      (debugM)

import           Instana.SDK.AgentStub.DiscoveryRequest (DiscoveryRequest)
import           Instana.SDK.AgentStub.Logging          (agentStubLogger)
import           Instana.SDK.AgentStub.Recorders        (Recorders)
import qualified Instana.SDK.AgentStub.Recorders        as Recorders
import           Instana.SDK.AgentStub.StubAPI          (ResetAPI, StubAPI)
import           Instana.SDK.AgentStub.TraceRequest     (Span)
import           Instana.SDK.AgentStub.Util             (stToServant)


stubServer :: Recorders -> Servant.Server StubAPI
stubServer recorders =
      getPing
 :<|> getRecordedDiscoveries recorders
 :<|> getRecordedAgentReadyRequests recorders
 :<|> getRecordedSpans recorders
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


getRecordedSpans :: Recorders -> Servant.Handler [Span]
getRecordedSpans recorders = do
  recordedSpans <- stToServant $ readSTRef $ Recorders.spanRecorder recorders
  return recordedSpans


resetServer ::
  Recorders
  -> Servant.Server ResetAPI
resetServer recorders =
      postResetDiscoveries recorders
 :<|> postResetSpans recorders


postResetDiscoveries :: Recorders -> Servant.Handler NoContent
postResetDiscoveries recorders = do
  liftIO $ debugM agentStubLogger $ "resetting recorded discoveries"
  stToServant $ modifySTRef (Recorders.discoveryRecorder recorders) (\_ -> [])
  stToServant $ modifySTRef (Recorders.agentReadyRecorder recorders) (\_ -> [])
  return NoContent


postResetSpans :: Recorders -> Servant.Handler NoContent
postResetSpans recorders = do
  liftIO $ debugM agentStubLogger $ "resetting recorded spans"
  stToServant $ modifySTRef (Recorders.spanRecorder recorders) (\_ -> [])
  return NoContent

