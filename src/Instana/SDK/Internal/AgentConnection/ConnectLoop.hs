{-|
Module      : Instana.SDK.Internal.AgentConnection.ConnectLoop
Description : Establishes a connection to the agent.
-}
module Instana.SDK.Internal.AgentConnection.ConnectLoop
    ( initConnectLoop
    ) where


import qualified Control.Concurrent                                   as Concurrent
import qualified Control.Concurrent.STM                               as STM
import           Control.Exception                                    (SomeException,
                                                                       catch)
import           Control.Monad                                        (forever)
import qualified System.Environment                                   as Environment
import           System.Log.Logger                                    (debugM,
                                                                       warningM)
import qualified System.Posix.Process                                 as PosixProcess

import qualified Instana.SDK.Internal.AgentConnection.AgentHostLookup as AgentHostLookup
import           Instana.SDK.Internal.AgentConnection.ProcessInfo     (ProcessInfo (ProcessInfo))
import qualified Instana.SDK.Internal.AgentConnection.ProcessInfo     as ProcessInfo
import           Instana.SDK.Internal.Context                         (ConnectionState (..),
                                                                       InternalContext)
import qualified Instana.SDK.Internal.Context                         as InternalContext
import           Instana.SDK.Internal.Logging                         (instanaLogger)


{-| Kick of a thread that loops endlessly and checks once in a while if the
agent connection is still up. If not, a connection attempt will be initiated.
The first attempt is made immediately when calling this.
-}
initConnectLoop :: InternalContext -> IO ()
initConnectLoop context = do
  pid <- PosixProcess.getProcessID
  progName <- Environment.getProgName
  execPath <- Environment.getExecutablePath
  args <- Environment.getArgs
  let
    processInfo =
      ProcessInfo
        { ProcessInfo.pidString   = show pid
        , ProcessInfo.programName = progName
        , ProcessInfo.executablePath = execPath
        , ProcessInfo.arguments   = args
        }
  debugM instanaLogger $ "discovered process info " ++ show processInfo

  -- connection loop works as follows:
  -- - try to connect to an an agent at either the agent host/port received via
  -- configuration, environment variables, default (127.0.0.1:42699) or default
  -- gateway
  -- - establishAgentConnection tries to connect to the agent by issuing
  --   a POST to /com.instana.plugin.haskell.discovery
  -- - establishAgentConnection only ever terminates if it has been successful,
  --   then we have switched to announced state.
  -- - after that, establishAgentConnection is called every 5 seconds,
  -- - if the connection is still up, establishAgentConnection does nothing and
  --   returns immediately,
  -- - should the connection have been lost, the cycle starts again, that is,
  --   establishAgentConnection will retry the POST forever and only terminate
  --   after success.
  forever $ do
    establishAgentConnectionSafe context processInfo
    Concurrent.threadDelay $ 5 * 1000 * 1000


establishAgentConnectionSafe ::
  InternalContext
  -> ProcessInfo
  -> IO ()
establishAgentConnectionSafe context processInfo =
  catch
    (establishAgentConnection context processInfo)
    -- exceptions in establishAgentConnection must not kill the loop, so we just
    -- catch everything
    (\e -> warningM instanaLogger $ show (e :: SomeException))


establishAgentConnection ::
  InternalContext
  -> ProcessInfo
  -> IO ()
establishAgentConnection context processInfo = do
  currentState <- STM.atomically $
    STM.readTVar (InternalContext.connectionState context)
  -- Do nothing if a connection attempt is already in progress or connection has
  -- already been established.
  if currentState /= Unconnected
    then
      return ()
    else do
      STM.atomically $ STM.writeTVar
        (InternalContext.connectionState context)
        AgentHostLookup
      debugM instanaLogger $ "agent connection is not up, attempting reconnect"
      -- Initial status: Unconnected
      -- step 1: do agent host looup (retry forever until an agent has
      --         been found)
      -- New status: Unannounced
      -- step 2: announce request (retry 3 times with 200 ms delay)
      -- New status: Announced
      -- step 3: check whether agent is ready to accept data (retry 10 times
      --         with 10 second delay)
      -- New status: Connected
      -- If anything fails in between, go back to "Unconnected"
      AgentHostLookup.lookupAgentHost context processInfo

