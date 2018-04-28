{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.Internal.Context
  ( InternalContext(..)
  , ConnectionState(..)
  , isAgentConnectionEstablished
  , mkAgentReadyState
  , readAgentUuid
  , readPid
  , whenConnected
  ) where


import           Control.Concurrent.STM                                     (STM)
import qualified Control.Concurrent.STM                                     as STM
import           Data.Sequence                                              (Seq)
import           Data.Text                                                  (Text)
import qualified Foreign.C.Types                                            as CTypes
import           GHC.Generics
import           Network.HTTP.Client                                        as HttpClient

import           Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse (AnnounceResponse)
import qualified Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse as AnnounceResponse
import           Instana.SDK.Internal.Command                               (Command)
import           Instana.SDK.Internal.Config                                (FinalConfig)
import           Instana.SDK.Internal.FullSpan                              (FullSpan)


data ConnectionState =
    Unconnected
  | AgentHostLookup
  | Unannounced (String, Int)
  | Announced
  | AgentReady AgentConnection
  deriving (Eq, Show, Generic)


data AgentConnection =
  AgentConnection
    { pid       :: String
    , agentUuid :: Text
    }
  deriving (Eq, Show, Generic)


mkAgentReadyState :: AnnounceResponse -> ConnectionState
mkAgentReadyState announceResponse =
  AgentReady $
    AgentConnection
      { pid       = show $ AnnounceResponse.pid announceResponse
      , agentUuid = AnnounceResponse.agentUuid announceResponse
      }


{-| A container for all the things the Instana SDK needs to do its work.
-}
data InternalContext = InternalContext
  { config          :: FinalConfig
  , httpManager     :: HttpClient.Manager
  , commandQueue    :: STM.TQueue Command
  , spanQueue       :: STM.TVar (Seq FullSpan)
  , connectionState :: STM.TVar ConnectionState
  , fileDescriptor  :: STM.TVar (Maybe CTypes.CInt)
  }


instance Show InternalContext where
  -- hide everything except for config when serializing context to string
  show context = show (config context)


isAgentConnectionEstablishedSTM :: InternalContext -> STM Bool
isAgentConnectionEstablishedSTM context = do
  state <- STM.readTVar $ connectionState context
  return $
    case state of
      AgentReady _ -> True
      _            -> False


isAgentConnectionEstablished :: InternalContext -> IO Bool
isAgentConnectionEstablished context =
  STM.atomically $ isAgentConnectionEstablishedSTM context


readAgentUuidSTM :: InternalContext -> STM (Maybe Text)
readAgentUuidSTM context = do
  state <- STM.readTVar $ connectionState context
  return $ mapConnectionState agentUuid state


readAgentUuid :: InternalContext -> IO (Maybe Text)
readAgentUuid context =
  STM.atomically $ readAgentUuidSTM context


readPidSTM :: InternalContext -> STM (Maybe String)
readPidSTM context = do
  state <- STM.readTVar $ connectionState context
  return $ mapConnectionState pid state


readPid :: InternalContext -> IO (Maybe String)
readPid context =
  STM.atomically $ readPidSTM context


mapConnectionState :: (AgentConnection -> a) -> ConnectionState -> Maybe a
mapConnectionState fn state =
  case state of
    AgentReady agentConnection ->
      Just $ fn agentConnection
    _ ->
      Nothing


whenConnected :: InternalContext -> (String -> Text -> IO ()) -> IO ()
whenConnected context action = do
  state <- STM.atomically $ STM.readTVar $ connectionState context
  whenConnectedState state action


whenConnectedState :: ConnectionState -> (String -> Text -> IO ()) -> IO ()
whenConnectedState state action = do
  case state of
    Unconnected ->
      return ()
    AgentHostLookup ->
      return ()
    Unannounced _ ->
      return ()
    Announced ->
      return ()
    AgentReady agentConnection ->
      action (pid agentConnection) (agentUuid agentConnection)

