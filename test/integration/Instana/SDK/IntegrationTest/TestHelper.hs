{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.IntegrationTest.TestHelper
  ( getSpanByName
  , pingAgentStub
  , pingApp
  , resetDiscoveries
  , resetSpans
  , shutDownAgentStub
  , shutDownApp
  , waitForEntityDataWithPid
  , waitForExternalAgentConnection
  , waitForDiscoveryWithPid
  , waitForAgentReadyWithPid
  , waitForSpansMatching
  , withSpanCreation
  ) where


import           Control.Exception                       (catch)
import qualified Data.ByteString.Lazy                    as LBS
import           Data.Either                             (Either)
import qualified Data.List                               as List
import qualified Data.Maybe                              as Maybe
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Network.HTTP.Client                     as HTTP

import           Instana.SDK.AgentStub.DiscoveryRequest  (DiscoveryRequest)
import qualified Instana.SDK.AgentStub.DiscoveryRequest  as DiscoveryRequest
import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import qualified Instana.SDK.AgentStub.EntityDataRequest as EntityDataRequest
import           Instana.SDK.AgentStub.TraceRequest      (Span)
import qualified Instana.SDK.AgentStub.TraceRequest      as TraceRequest
import qualified Instana.SDK.IntegrationTest.HttpHelper  as HttpHelper
import           Instana.SDK.IntegrationTest.Util        (putStrFlush, (|>))


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
  HttpHelper.doAgentStubRequest "stub/ping" "GET"


pingApp :: IO (HTTP.Response LBS.ByteString)
pingApp = do
  HttpHelper.doAppRequest "ping" "GET" []


shutDownAgentStub :: IO ()
shutDownAgentStub = do
  catch
    ( HttpHelper.doAgentStubRequest "stub/shutdown" "POST"
      >> return ()
    )
    -- Ignore all exceptions for the shutdown request. Either the agent stub has
    -- already been shut down (so the request results in a network error) or, if
    -- it is successfull, it results in an HTTP 500 because the agent stub
    -- process terminates before responding.
    (\ (_ :: HTTP.HttpException) -> return ())


shutDownApp :: IO ()
shutDownApp = do
  catch
    ( HttpHelper.doAppRequest "shutdown" "POST" []
      >> return ()
    )
    -- Ignore all exceptions for the shutdown request. Either the app has
    -- already been shut down (so the request results in a network error) or, if
    -- it is successfull, it results in an HTTP 500 because the app process
    -- terminates before responding.
    (\ (_ :: HTTP.HttpException) -> return ())


waitForExternalAgentConnection :: Bool -> Int -> IO (Either String (DiscoveryRequest, String))
waitForExternalAgentConnection =
  waitForAgentConnection


waitForAgentConnection ::
  Bool
  -> Int
  -> IO (Either String (DiscoveryRequest, String))
waitForAgentConnection pidTranslation untranslatedPid = do
  let
    translatedPid =
      if pidTranslation then untranslatedPid + 1 else untranslatedPid
    pid = show translatedPid
  discoveryWithPid <- waitForDiscoveryWithPid pid
  case discoveryWithPid of
    Left message1 -> do
      putStrLn $
        "❗️ Could not establish agent connection " ++
        "(discovery failed): " ++ message1
      return $ Left $
        "❗️ Could not establish agent connection " ++
        "(discovery failed): " ++ message1
    Right _ -> do
      agentReady <- waitForAgentReadyWithPid pid
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
          return discoveryWithPid


waitForDiscoveryWithPid :: String -> IO (Either String (DiscoveryRequest, String))
waitForDiscoveryWithPid pidStr = do
  putStrFlush $ "⏱  waiting for discovery request for pid " ++ pidStr
  discoveries <-
    HttpHelper.retryRequest (containsDiscoveryWithPid pidStr) getDiscoveries
  case discoveries of
    Left message -> do
      putStrLn $ "\n❗️ recorded discovery request could not be obtained"
      return $ Left message
    Right ds -> do
      putStrLn "\n✅ recorded discovery request obtained"
      return $ Right $ (head ds, pidStr)


getDiscoveries :: IO (Either String [DiscoveryRequest])
getDiscoveries = do
  HttpHelper.requestAgentStubAndParse "stub/discoveries" "GET"


containsDiscoveryWithPid ::
  String
  -> [DiscoveryRequest]
  -> Bool
containsDiscoveryWithPid pid discoveries =
  length matchingDiscoveries >= 1
  where
    matchingDiscoveries =
      List.filter
        (\d -> DiscoveryRequest.pid d == pid)
        discoveries


waitForAgentReadyWithPid :: String -> IO (Either String ())
waitForAgentReadyWithPid pidStr = do
  putStrFlush $ "⏱  waiting for agent ready request for pid " ++ pidStr
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
  HttpHelper.requestAgentStubAndParse "stub/agent-ready" "GET"


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


waitForEntityDataWithPid :: String -> IO (Either String [EntityDataRequest])
waitForEntityDataWithPid pidStr = do
  putStrFlush $
    "⏱  waiting for entity data for pid " ++ pidStr ++ " to be collected"
  entityDataRequests <-
    HttpHelper.retryRequest
      (containsEntityDataRequestsWithPid pidStr)
      getEntityDataRequests
  case entityDataRequests of
    Left message -> do
      putStrLn $ "\n❗️ recorded entity data request(s) could not be obtained"
      return $ Left message
    Right _ -> do
      putStrLn $ "\n✅ recorded entity data request(s) have been obtained"
      return entityDataRequests


getEntityDataRequests :: IO (Either String [EntityDataRequest])
getEntityDataRequests = do
  HttpHelper.requestAgentStubAndParse "stub/entity-data" "GET"


containsEntityDataRequestsWithPid ::
  String
  -> [EntityDataRequest]
  -> Bool
containsEntityDataRequestsWithPid pid entityDataRequests =
  length matchingEntityDataRequests >= 1
  where
    matchingEntityDataRequests =
      List.filter
        (\edr ->
          (edr
            |> EntityDataRequest.pid
            |> Maybe.fromMaybe "no PID available"
            |> T.unpack
          ) == pid
        )
        entityDataRequests


waitForSpansMatching :: [Text] -> IO (Either String [Span])
waitForSpansMatching expectedNames = do
  putStrFlush "⏱  waiting for spans to be processed"
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
  HttpHelper.requestAgentStubAndParse "stub/spans" "GET"


getSpanByName :: Text -> [Span] -> Maybe Span
getSpanByName name =
  List.find (\s -> TraceRequest.n s == name)


-- |Will also reset agent ready requests and entity data requests (basically
-- forget that the announce/connection establishment has ever happened)
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
    url = HttpHelper.agentStubUrl $ "stub/reset/" ++ what
  defaultRequestSettings <- HTTP.parseUrlThrow url
  let
    request = defaultRequestSettings
       { HTTP.method = "POST"
       , HTTP.requestHeaders = HttpHelper.defaultHeaders
       }
  _ <- HTTP.httpLbs request httpManager
  return ()

