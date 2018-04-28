{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.IntegrationTest.HttpHelper
  ( agentStubHost
  , agentStubPort
  , defaultHeaders
  , doRequest
  , parseResponse
  , requestAndParse
  , retryRequest
  , retryRequestRecovering
  , stubUrl
  ) where


import           Control.Monad.Catch              (Handler)
import qualified Control.Retry                    as Retry
import qualified Data.Aeson                       as Aeson
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as LBS
import qualified Network.HTTP.Client              as HTTP
import           Network.HTTP.Types.Header        (HeaderName)
import           Network.HTTP.Types.Method        (Method)

import           Instana.SDK.IntegrationTest.Util (putStrFlush)


-- 100 milliseconds
retryDelay :: Int
retryDelay = 100 * 1000


-- 5 seconds
maxRetryDelay :: Int
maxRetryDelay = 5 * 1000 * 1000


agentStubHost :: String
agentStubHost = "127.0.0.1"


agentStubPort :: Int
agentStubPort = 1302


agentStubBaseUrl :: String
agentStubBaseUrl =
  "http://" ++ agentStubHost ++ ":" ++ (show agentStubPort) ++ "/"


stubUrl :: String -> String
stubUrl path =
  agentStubBaseUrl ++ path


defaultHeaders :: [(HeaderName, ByteString)]
defaultHeaders =
  [ ("Accept", "application/json")
  , ("Content-Type", "application/json; charset=UTF-8'")
  ]


requestAndParse ::
  Aeson.FromJSON a =>
  String
  -> ByteString
  -> IO (Either String a)
requestAndParse path method = do
  response <- doRequest path method
  parseResponse response


doRequest ::
  String
  -> Method
  -> IO (HTTP.Response LBS.ByteString)
doRequest path method = do
  httpManager <- HTTP.newManager $
    HTTP.defaultManagerSettings { HTTP.managerConnCount = 5 }
  let
    url = stubUrl path
  defaultRequestSettings <- HTTP.parseUrlThrow url
  let
    request = defaultRequestSettings
       { HTTP.method = method
       , HTTP.requestHeaders = defaultHeaders
       }
  HTTP.httpLbs request httpManager


parseResponse ::
  Aeson.FromJSON a
  => HTTP.Response LBS.ByteString
  -> IO (Either String a)
parseResponse response = do
  let
    body = HTTP.responseBody response
    parsed = Aeson.decode body
    result =
      case parsed of
        Just p  -> Right p
        Nothing -> Left "Could not parse response."
  return result


retryRequestRecovering ::
  IO (HTTP.Response LBS.ByteString)
  -> IO (HTTP.Response LBS.ByteString)
retryRequestRecovering request =
  let
    retryPolicy :: Retry.RetryPolicyM IO =
      Retry.limitRetriesByCumulativeDelay maxRetryDelay $
        Retry.constantDelay retryDelay
    reportHttpError :: Bool -> HTTP.HttpException -> Retry.RetryStatus -> IO ()
    reportHttpError _ _ _ = putStrFlush "."
    retryOnAnyHttpError :: HTTP.HttpException -> IO Bool
    retryOnAnyHttpError _ = return True
    retryOnAnyStatus :: Retry.RetryStatus -> Handler IO Bool
    retryOnAnyStatus = Retry.logRetries retryOnAnyHttpError reportHttpError

  in
    Retry.recovering
       retryPolicy
       [retryOnAnyStatus]
       (const request)


retryRequest ::
  forall a.
  Show a =>
  (a -> Bool)
  -> IO (Either String a)
  -> IO (Either String a)
retryRequest retryUntil executeRequestAndParseResponse = do
  let
    retryPolicy :: Retry.RetryPolicyM IO =
      Retry.limitRetriesByCumulativeDelay maxRetryDelay $
        Retry.constantDelay retryDelay
    retryCheck :: Retry.RetryStatus -> Either String a -> IO Bool
    retryCheck _ decodedResponse = do
      putStrFlush "."
      case decodedResponse of
        Left _ ->
          return True
        Right value ->
          return $ not $ retryUntil value
  lastResponse <- Retry.retrying
     retryPolicy
     retryCheck
     (const executeRequestAndParseResponse)
  case lastResponse of
    Left _ ->
      return lastResponse
    Right lastValue ->
      if retryUntil lastValue
      then
        return lastResponse
      else
        return $
          Left $
            "HTTP request was successful, but the expected value " ++
            "has not been obtained. The last response was: " ++
            show lastValue

