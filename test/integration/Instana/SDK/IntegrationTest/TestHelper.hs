{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.TestHelper
  ( getSpanByName
  , pingAgentStub
  , resetDiscoveries
  , resetSpans
  , waitForAgentConnection
  , waitForDiscoveryWithMyPid
  , waitForDiscoveryWithPid
  , waitForAgentReadyWithMyPid
  , waitForAgentReadyWithPid
  , waitForSpansMatching
  , withSpanCreation
  ) where


import qualified Data.ByteString.Lazy                   as LBS
import           Data.Either                            (Either)
import qualified Data.List                              as List
import           Data.Text                              (Text)
import qualified Network.HTTP.Client                    as HTTP
import qualified System.Posix.Process                   as PosixProcess

import           Instana.SDK.AgentStub.DiscoveryRequest (DiscoveryRequest)
import qualified Instana.SDK.AgentStub.DiscoveryRequest as DiscoveryRequest
import           Instana.SDK.AgentStub.TraceRequest     (Span)
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.Util       (putStrFlush)


withSpanCreation ::
  IO a
  -> [Text]
  -> IO (a, Either String [Span])
withSpanCreation createSpanAction expectedSpans = do
  result <- createSpanAction
  spansResults <- waitForSpansMatching expectedSpans
  resetSpans
  return (result, spansResults)


pingAgentStub :: IO (HTTP.Response LBS.ByteString)
pingAgentStub = do
  HttpHelper.doRequest "stub/ping" "GET"


waitForAgentConnection :: Bool -> IO (Either String DiscoveryRequest)
waitForAgentConnection pidTranslation = do
  originalPid <- PosixProcess.getProcessID
  let
    pid = if pidTranslation then originalPid + 1 else originalPid
  discoveries  <- waitForDiscoveryWithPid $ show pid
  case discoveries of
    Left message1 -> do
      putStrLn $
        "❗️ Could not establish agent connection " ++
        "(discovery failed): " ++ message1
      return $ Left $
        "❗️ Could not establish agent connection " ++
        "(discovery failed): " ++ message1
    Right _ -> do
      agentReady <- waitForAgentReadyWithPid $ show pid
      case agentReady of
        Left message2 -> do
          putStrLn $
            "❗️ Could not establish agent connection " ++
            "(agent ready failed): " ++ message2
          return $ Left $
            "Could not establish agent connection " ++
            "(agent ready failed): " ++ message2
        Right _ -> do
          putStrLn $ "\n✅ agent connection has been established"
          return discoveries


waitForDiscoveryWithMyPid :: IO (Either String DiscoveryRequest)
waitForDiscoveryWithMyPid = do
  pid <- PosixProcess.getProcessID
  waitForDiscoveryWithPid $ show pid


waitForDiscoveryWithPid :: String -> IO (Either String DiscoveryRequest)
waitForDiscoveryWithPid pidStr = do
  putStrFlush $ "⏱ waiting for discovery request for pid " ++ pidStr
  discoveries <-
    HttpHelper.retryRequest (containsDiscoveryWithPid pidStr) getDiscoveries
  case discoveries of
    Left message -> do
      putStrLn $ "\n❗️ recorded discovery request could not be obtained"
      return $ Left message
    Right ds -> do
      putStrLn "\n✅ recorded discovery request obtained"
      return $ Right $ head ds


getDiscoveries :: IO (Either String [DiscoveryRequest])
getDiscoveries = do
  HttpHelper.requestAndParse "stub/discoveries" "GET"


containsDiscoveryWithPid ::
  String
  -> [DiscoveryRequest]
  -> Bool
containsDiscoveryWithPid pid discoveries =
  length matchingDiscoveries == 1
  where
    matchingDiscoveries =
      List.filter
        (\d -> DiscoveryRequest.pid d == pid)
        discoveries


waitForAgentReadyWithMyPid :: IO (Either String ())
waitForAgentReadyWithMyPid = do
  pid <- PosixProcess.getProcessID
  waitForAgentReadyWithPid $ show pid


waitForAgentReadyWithPid :: String -> IO (Either String ())
waitForAgentReadyWithPid pidStr = do
  putStrFlush $ "⏱ waiting for agent ready request for pid " ++ pidStr
  agentReadyPids <-
    HttpHelper.retryRequest
      (containsAgentReadyWithPid pidStr)
      getAgentReadyPids
  case agentReadyPids of
    Left message -> do
      putStrLn $ "\n❗️ recorded agent ready request could not be obtained"
      return $ Left message
    Right _ -> do
      putStrLn $ "\n✅ recorded agent ready request obtained"
      return $ Right ()


getAgentReadyPids :: IO (Either String [String])
getAgentReadyPids = do
  HttpHelper.requestAndParse "stub/agentReady" "GET"


containsAgentReadyWithPid ::
  String
  -> [String]
  -> Bool
containsAgentReadyWithPid pid pidsFromResponse =
  length matchingPids > 0
  where
    matchingPids =
      List.filter
        (\p -> p == pid)
        pidsFromResponse


waitForSpansMatching :: [Text] -> IO (Either String [Span])
waitForSpansMatching expectedNames = do
  putStrFlush "⏱ waiting for spans to be processed"
  spans <- HttpHelper.retryRequest (hasMatchingSpans expectedNames) getSpans
  putStrLn "\n✅ spans have been processed"
  return spans


hasMatchingSpans :: [Text] -> [Span] -> Bool
hasMatchingSpans expectedNames spans =
  let
     namesFromResponse = List.map TraceRequest.n spans
     intersection = List.intersect namesFromResponse expectedNames
   in
     length intersection == length expectedNames


getSpans :: IO (Either String [Span])
getSpans =
 HttpHelper.requestAndParse "stub/spans" "GET"


getSpanByName :: Text -> [Span] -> Maybe Span
getSpanByName name =
  List.find (\s -> TraceRequest.n s == name)


resetDiscoveries :: IO ()
resetDiscoveries =
  reset "discoveries"


resetSpans :: IO ()
resetSpans =
  reset "spans"


reset :: String -> IO ()
reset what = do
  httpManager <- HTTP.newManager $
    HTTP.defaultManagerSettings { HTTP.managerConnCount = 5 }
  let
    url = HttpHelper.stubUrl $ "stub/reset/" ++ what
  defaultRequestSettings <- HTTP.parseUrlThrow url
  let
    request = defaultRequestSettings
       { HTTP.method = "POST"
       , HTTP.requestHeaders = HttpHelper.defaultHeaders
       }
  _ <- HTTP.httpLbs request httpManager
  return ()

