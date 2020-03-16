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


import qualified Control.Concurrent.STM                           as STM
import           Control.Exception                                (SomeException,
                                                                   catch)
import           Data.ByteString.Char8                            (unpack)
import           Data.Char                                        (isSpace)
import qualified Data.List                                        as List
import qualified Network.HTTP.Client                              as HTTP
import qualified Network.HTTP.Types.Header                        as Header
import           System.Exit                                      (ExitCode (ExitSuccess))
import           System.Log.Logger                                (debugM)
import           System.Process                                   as Process

import qualified Instana.SDK.Internal.AgentConnection.Announce    as Announce
import           Instana.SDK.Internal.AgentConnection.ProcessInfo (ProcessInfo)
import qualified Instana.SDK.Internal.Config                      as InternalConfig
import           Instana.SDK.Internal.Context                     (ConnectionState (..),
                                                                   InternalContext)
import qualified Instana.SDK.Internal.Context                     as InternalContext
import           Instana.SDK.Internal.Logging                     (instanaLogger)
import qualified Instana.SDK.Internal.Retry                       as Retry
import qualified Instana.SDK.Internal.URL                         as URL


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
    expectedServerHeader = "Instana Agent"
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
        headers = HTTP.responseHeaders response
        serverHeaderTuple = List.find (\(h, _) -> h == Header.hServer) headers
        serverHeaderValue = (unpack . snd) <$> serverHeaderTuple
      if serverHeaderValue == Just expectedServerHeader
      then do
        return $ Right (host, port)
      else do
        return $ Left $
          "Host at " ++ show agentRootUrl ++ " did not respond with " ++
          "expected Server header (" ++ expectedServerHeader ++
          ") but with: " ++ show serverHeaderValue
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
    cmd = "/sbin/ip route | awk '/default/ { print $3 }'"
    stdIn = ""
  (exitCode, stdOut, stdErr) <-
    Process.readCreateProcessWithExitCode (Process.shell cmd) stdIn
  if exitCode /= ExitSuccess || stdErr /= ""
  then
    return $ Left $ "Failed to retrieve default gateway: " ++ stdErr
  else
    return $ Right $ trim stdOut
  where
    trim = List.dropWhileEnd isSpace . dropWhile isSpace

