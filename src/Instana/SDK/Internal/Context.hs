{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.Context
Description : The Instana context holds everything that the SDK needs in terms of state.
-}
module Instana.SDK.Internal.Context
  ( InternalContext(..)
  , ConnectionState(..)
  , isAgentConnectionEstablished
  , mkAgentReadyState
  , readAgentUuid
  , readPid
  , whenConnected
  ) where


import           Control.Concurrent                                         (ThreadId)
import           Control.Concurrent.STM                                     (STM)
import qualified Control.Concurrent.STM                                     as STM
import           Data.Map.Strict                                            (Map)
import           Data.Sequence                                              (Seq)
import           Data.Text                                                  (Text)
import qualified Foreign.C.Types                                            as CTypes
import           GHC.Generics
import           Network.HTTP.Client                                        as HttpClient
import qualified System.Metrics                                             as Metrics

import           Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse (AnnounceResponse)
import qualified Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse as AnnounceResponse
import           Instana.SDK.Internal.Command                               (Command)
import           Instana.SDK.Internal.Config                                (FinalConfig)
import           Instana.SDK.Internal.FullSpan                              (FullSpan)
import           Instana.SDK.Internal.SpanStack                             (SpanStack)


-- |The current state of the connection to the agent.
data ConnectionState =
    -- |Connection handshake has not been started yet.
    Unconnected
    -- |Phase agent host lookup has been initiated.
  | AgentHostLookup
    -- |Agent host lookup is complete, the process has not been announced yet.
  | Unannounced (String, Int)
    -- |Announce was successful, waiting for the agent to signal readyness.
  | Announced
    -- |Agent has signaled that it is ready to accept data.
  | AgentReady Ready
  deriving (Eq, Show, Generic)


-- |Data to hold after agent ready event.
data Ready =
  Ready
    { connection :: AgentConnection
    , metrics    :: Metrics.Store
    } deriving (Generic)


instance Eq Ready where
  r1 == r2 =
    connection r1 == connection r2


instance Show Ready where
  show r =
     show $ connection r


-- |Meta data about the connection to the agent.
data AgentConnection =
  AgentConnection
    {
      -- |the PID of the monitored process
      pid       :: String
      -- |the agent's UUID
    , agentUuid :: Text
    }
  deriving (Eq, Show, Generic)


-- |Creates a "ready" connection state from an AnnounceResponse.
mkAgentReadyState :: AnnounceResponse -> Metrics.Store -> ConnectionState
mkAgentReadyState announceResponse metricsStore =
  let
    agentConnection = AgentConnection
      { pid          = show $ AnnounceResponse.pid announceResponse
      , agentUuid    = AnnounceResponse.agentUuid announceResponse
      }
  in
  AgentReady $
    Ready
      { connection = agentConnection
      , metrics    = metricsStore
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
  , currentSpans    :: STM.TVar (Map ThreadId SpanStack)
  , previousMetrics :: STM.TVar Metrics.Sample
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


-- |Checks if the connection to the agent has been established.
isAgentConnectionEstablished :: InternalContext -> IO Bool
isAgentConnectionEstablished context =
  STM.atomically $ isAgentConnectionEstablishedSTM context


readAgentUuidSTM :: InternalContext -> STM (Maybe Text)
readAgentUuidSTM context = do
  state <- STM.readTVar $ connectionState context
  return $ mapConnectionState agentUuid state


-- |accessor for the agent UUID
readAgentUuid :: InternalContext -> IO (Maybe Text)
readAgentUuid context =
  STM.atomically $ readAgentUuidSTM context


readPidSTM :: InternalContext -> STM (Maybe String)
readPidSTM context = do
  state <- STM.readTVar $ connectionState context
  return $ mapConnectionState pid state


-- |accessor for the PID of the monitored process
readPid :: InternalContext -> IO (Maybe String)
readPid context =
  STM.atomically $ readPidSTM context


mapConnectionState :: (AgentConnection -> a) -> ConnectionState -> Maybe a
mapConnectionState fn state =
  case state of
    AgentReady (Ready agentConnection _) ->
      Just $ fn agentConnection
    _ ->
      Nothing


-- |Executes an IO action only when the connection to the agent has been
-- established. The action receives the PID, the agent UUID and the internal
-- metrics store as parameters (basically everything that is only available with
-- an established agent connection).
whenConnected ::
  InternalContext
  -> (String -> Text -> Metrics.Store -> IO ())
  -> IO ()
whenConnected context action = do
  state <- STM.atomically $ STM.readTVar $ connectionState context
  whenConnectedState
    state
    (\(Ready agentConnection metricsStore) ->
      action (pid agentConnection) (agentUuid agentConnection) metricsStore
    )


whenConnectedState :: ConnectionState -> (Ready -> IO ()) -> IO ()
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
    AgentReady ready -> do
      action ready

