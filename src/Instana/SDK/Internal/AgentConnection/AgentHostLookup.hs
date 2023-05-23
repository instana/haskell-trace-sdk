{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.AgentHostLookup
Description : Handles the agent host lookup phase for establishing the
connection to the agent.
-}
module Instana.SDK.Internal.AgentConnection.AgentHostLookup
    ( lookupAgentHost
    ) where


import qualified Control.Concurrent.STM                                as STM
import           Control.Exception                                     (SomeException,
                                                                        catch)
import qualified Network.HTTP.Client                                   as HTTP
import qualified Network.HTTP.Types.Status                             as Status
import           System.Log.Logger                                     (debugM)

import qualified Instana.SDK.Internal.AgentConnection.Announce         as Announce
import           Instana.SDK.Internal.AgentConnection.DefaultGatewayIp (extractDefaultGatewayIp)
import           Instana.SDK.Internal.AgentConnection.ProcessInfo      (ProcessInfo)
import qualified Instana.SDK.Internal.Config                           as InternalConfig
import           Instana.SDK.Internal.Context                          (ConnectionState (..),
                                                                        InternalContext)
import qualified Instana.SDK.Internal.Context                          as InternalContext
import           Instana.SDK.Internal.Logging                          (instanaLogger)
import qualified Instana.SDK.Internal.Retry                            as Retry
import qualified Instana.SDK.Internal.URL                              as URL


type SuccessfullHost = (String, Int)


-- |Starts the agent host lookup phase.
lookupAgentHost ::
  InternalContext
  -> ProcessInfo
  -> IO ()
lookupAgentHost context processInfo = do
  debugM instanaLogger "Starting agent host lookup."
  result <-
    Retry.retryUntilRight
      Retry.agentHostLookupRetryPolicy
      (tryAll context)
  case result of
    Right successfulHost -> do
      debugM instanaLogger $
        "Found an agent to connect to: " ++ show successfulHost ++
        ", starting sensor-agent handshake."
      STM.atomically $
        STM.writeTVar
          (InternalContext.connectionState context)
          (Unannounced successfulHost)

      -- transition to next phase of sensor-agent handshake
      Announce.announce context processInfo
    Left _ -> do
      -- Actually, this line should never be reached, as the agent host lookup
      -- is retried indefinitely.
      debugM instanaLogger "Could not find an agent to connect to."
      STM.atomically $ STM.writeTVar
        (InternalContext.connectionState context)
        Unconnected


tryAll :: InternalContext -> IO (Either String SuccessfullHost)
tryAll context = do
  resultConfiguredHost <- tryConfiguredHost context
  case resultConfiguredHost of
    Right _ ->
      return resultConfiguredHost
    Left errc -> do
      debugM instanaLogger $ errc ++ ", trying default gateway next."
      resultDefaultGateway <- tryDefaultGateway context
      case resultDefaultGateway of
        Right _ ->
          return resultDefaultGateway
        Left errdg -> do
          debugM instanaLogger errdg
          return resultDefaultGateway


tryConfiguredHost :: InternalContext -> IO (Either String SuccessfullHost)
tryConfiguredHost context = do
  let
    config = InternalContext.config context
    host = InternalConfig.agentHost config
    port = InternalConfig.agentPort config
  tryHost context host port


tryDefaultGateway :: InternalContext -> IO (Either String SuccessfullHost)
tryDefaultGateway context = do
  let
    config = InternalContext.config context
    port = InternalConfig.agentPort config
  defaultGateway <- readDefaultGateway
  case defaultGateway of
    Right gatewayHost ->
      tryHost context gatewayHost port
    Left err ->
      return $ Left err


tryHost ::
  InternalContext
  -> String
  -> Int
  -> IO (Either String SuccessfullHost)
tryHost context host port = do
  let
    manager = InternalContext.httpManager context
    agentRootUrl = URL.mkHttp host port ""
  debugM instanaLogger $ "Trying to reach agent at " ++ show agentRootUrl
  agentRootRequest <- HTTP.parseUrlThrow $ show agentRootUrl
  let
    agentRootAction = HTTP.httpLbs agentRootRequest manager
  catch
    (do
      response <- agentRootAction
      let
        httpStatus = Status.statusCode $ HTTP.responseStatus response
      if httpStatus >= 200 && httpStatus < 300
      then do
        return $ Right (host, port)
      else do
        return $ Left $
          "Host at " ++ show agentRootUrl ++ " did not respond with " ++
          "expected HTTP status but with: " ++ show httpStatus
    )
    (\e -> do
      let
        _ = (e :: SomeException)
      return $ Left $
        "Could not reach agent at " ++ show agentRootUrl
    )


readDefaultGateway :: IO (Either String String)
readDefaultGateway = do
  let
    routeFilePath = "/proc/self/net/route"
  catch
    ( do
        routeFileContent <- readFile routeFilePath
        let
          defaultGatewayIpM = extractDefaultGatewayIp routeFileContent
        case defaultGatewayIpM of
          Just defaultGatewayIp -> do
            debugM instanaLogger $
              "Determined default gateway IP: " ++ defaultGatewayIp
            return $ Right defaultGatewayIp
          Nothing ->
            return $
              Left $
                "Failed to parse default gateway IP from " ++ routeFilePath ++ "."
    )
    (\(e :: SomeException) -> do
      return $
        Left $
          "Failed to read " ++ routeFilePath ++
          " to determine the default gateway IP: " ++
          show e
    )
