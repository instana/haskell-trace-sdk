{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.Announce
Description : Handles the announce phase for establishing the connection to the
agent.
-}
module Instana.SDK.Internal.AgentConnection.Announce
    ( announce
    ) where


import qualified Control.Concurrent.STM                                     as STM
import           Control.Exception                                          (SomeException,
                                                                             catch)
import           Data.Aeson                                                 ((.=))
import qualified Data.Aeson                                                 as Aeson
import           Data.ByteString.Lazy                                       (ByteString)
import           Data.Maybe                                                 (isJust)
import qualified Network.HTTP.Client                                        as HTTP
import           System.Log.Logger                                          (debugM,
                                                                             warningM)
import qualified System.Posix.Files                                         as PosixFiles

import           Instana.SDK.Internal.AgentConnection.AgentReady            as AgentReady
import           Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse (AnnounceResponse)
import           Instana.SDK.Internal.AgentConnection.Paths
import           Instana.SDK.Internal.AgentConnection.ProcessInfo           (ProcessInfo)
import qualified Instana.SDK.Internal.AgentConnection.ProcessInfo           as ProcessInfo
import           Instana.SDK.Internal.Context                               (ConnectionState (..),
                                                                             InternalContext)
import qualified Instana.SDK.Internal.Context                               as InternalContext
import           Instana.SDK.Internal.Logging                               (instanaLogger)
import qualified Instana.SDK.Internal.Retry                                 as Retry
import qualified Instana.SDK.Internal.URL                                   as URL


-- |Starts the announce phase.
announce ::
  InternalContext
  -> ProcessInfo
  -> IO ()
announce context processInfo = do
  debugM instanaLogger "Starting to announce process to agent."
  connectionState <-
      STM.atomically $ STM.readTVar $ InternalContext.connectionState context
  case connectionState of
    Unannounced hostAndPort ->
      announceToAgent context hostAndPort processInfo
    _ -> do
      warningM instanaLogger $
        "Reached illegal state in announce, agent host lookup did not " ++
        "yield a host and port. Connection state is " ++ show connectionState ++
        ". Will retry later."
      STM.atomically $ STM.writeTVar
        (InternalContext.connectionState context)
        Unconnected
      return ()


announceToAgent ::
  InternalContext
  -> (String, Int)
  -> ProcessInfo
  -> IO ()
announceToAgent context (host, port) processInfo = do
  fileDescriptor <-
    STM.atomically $ STM.readTVar $ InternalContext.fileDescriptor context
  let
    manager = InternalContext.httpManager context
    discoveryUrl =
      URL.mkHttp host port haskellDiscoveryPath
    nonContainerPid = ProcessInfo.pidString processInfo
    (pidStr, pidFromParentNS) =
       case ProcessInfo.parentNsPid processInfo of
         Just parentPid ->
           (parentPid, parentPid == nonContainerPid)
         Nothing ->
           (nonContainerPid, False)
    maybeFileDescriptorString = show <$> fileDescriptor
    maybeInodeLinkPath =
      (\fdStr -> "/proc/" ++ nonContainerPid ++ "/fd/" ++ fdStr) <$>
        maybeFileDescriptorString
  discoveryRequestBase <- HTTP.parseUrlThrow $ show discoveryUrl
  inode <-
    case maybeInodeLinkPath of
      Just inodeLinkPath ->
        catch
          (do
            i <- PosixFiles.readSymbolicLink inodeLinkPath
            return $ Just i
          )
          (\e -> do
            debugM instanaLogger $
              "Could not obtain inode for process matching from " ++
              inodeLinkPath ++ ": " ++ show (e :: SomeException)
            return Nothing
          )
      Nothing ->
        return Nothing

  let
    haskellProcess =
      Aeson.object
        [ "pid"             .= pidStr
        , "pidFromParentNS" .= pidFromParentNS
        , "progName"        .= ProcessInfo.programName processInfo
        , "execPath"        .= ProcessInfo.executablePath processInfo
        , "args"            .= ProcessInfo.arguments processInfo
        , "fd"              .= maybeFileDescriptorString
        , "inode"           .= inode
        ]

    announceRequest = discoveryRequestBase
       { HTTP.method = "PUT"
       , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode haskellProcess
       , HTTP.requestHeaders =
         [ ("Accept", "application/json")
         , ("Content-Type", "application/json; charset=UTF-8'")
         ]
       }
    announceRequestAction = HTTP.httpLbs announceRequest manager

  maybeAnnounceResponse <-
    catch
      (Retry.retryRequest
         Retry.announceRetryPolicy
         decodeAnnounceResponse
         announceRequestAction
      )
      (\e -> do
        warningM instanaLogger $ show (e :: SomeException)
        return Nothing
      )

  -- maybeAnnounceResponse is guaranteed to be Just _ if request succeeded and
  -- response was parsed succesfully, it is only Nothing when an exception
  -- has been catched, which should actually never happen, because we retry
  -- indefinitely.
  if isJust maybeAnnounceResponse
  then do
    let
      Just announceResponse = maybeAnnounceResponse
    STM.atomically $
      STM.writeTVar (InternalContext.connectionState context) $
        Announced (host, port)
    debugM instanaLogger $
      "Haskell process " ++ pidStr ++
      " has been successfully announced to agent at " ++ show (host, port) ++
      ", now waiting for the agent to be ready to accept data."

    -- transition to next phase of sensor-agent handshake
    AgentReady.waitUntilAgentIsReadyToAcceptData
      context
      pidStr
      processInfo
      announceResponse
  else do
    warningM instanaLogger $
      "Could not establish agent connection for process " ++ pidStr ++
      " (announce failed), will retry later."
    STM.atomically $ STM.writeTVar
      (InternalContext.connectionState context)
      Unconnected


{-| Decodes the JSON response returned by the announce request.
-}
decodeAnnounceResponse ::
  HTTP.Response ByteString
  -> IO (Maybe AnnounceResponse)
decodeAnnounceResponse response = do
  let
    body = HTTP.responseBody response
    maybeParsed :: Maybe AnnounceResponse
    maybeParsed = Aeson.decode body
  case maybeParsed of
    Just _ -> do
      return maybeParsed
    Nothing ->
      fail $ "Can't parse announce response" ++ (show body)

