{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.IntegrationTest.HttpHelper
  ( agentStubHost
  , agentStubPort
  , agentStubUrl
  , appUrl
  , defaultHeaders
  , doAgentStubRequest
  , doAppRequest
  , parseResponse
  , requestAgentStubAndParse
  , requestAppAndParse
  , retryRequest
  , retryRequestRecovering
  ) where


import           Control.Monad.Catch               (Handler)
import qualified Control.Retry                     as Retry
import qualified Data.Aeson                        as Aeson
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Lazy              as LBS
import qualified Network.HTTP.Client               as HTTP
import           Network.HTTP.Types.Header         (Header, HeaderName)
import           Network.HTTP.Types.Method         (Method)

import qualified Instana.SDK.IntegrationTest.Suite as Suite
import           Instana.SDK.IntegrationTest.Util  (putStrFlush)


-- 100 milliseconds
retryDelay :: Int
retryDelay = 100 * 1000


-- Time to wait for a particular HTTP response: 7.5 seconds.
--
-- We need > 5 seconds, in particular on Travis. Reason:
-- 1) The agent stub and the app under test are started more or less
--    simultaneously
-- 2) The agent stub might not yet be up when the Instana SDK in the app under
--    test starts to look for an agent (which is a perfectly normal situation
--    and should shouldn't not be a problem as the SDK will retry with to find
--    an agent to connect to with fibonacci backoff).
-- 3) When this happens, the first attempt to talk to 127.0.0.1:1302 fails.
-- 4) Next, the SDK attempts to talk to an agent over the default gateway.
-- 5) This never happens locally (at least not on MacOS), as there is no
--    /sbin/ip executable present
-- 6) On Travis (or more generally on most Linux systems), the SDK actually
--    tries the default gateway because that executable _is_ present. It will
--    also find a default gateway (let's say, for example, 10.20.0.1).
-- 7) That host is reachable so an HTTP request to 10.20.0.1:1302 is attempted
--    (1302 is the configured agent port in the integration tests).
-- 8) If something exists at 10.20.0.1:1302 it never sends a response. Or there
--    is nothing there but somehow the request does not fail immediately.
--    Either way, trying this HTTP request eats up around 5 seconds. Why 5
--    seconds? 5 seconds is:
--    a) the timeout the SDK sets itself (see Instana/SDK/SDK.hs, value for
--       HTTP.managerResponseTimeout),
--    b) also the standard HTTP client timeout (see
--    https://hackage.haskell.org/package/http-client-0.1.0.0/docs/Network-HTTP-Client-Manager.html - managerResponseTimeout).
-- 9) When the HTTP client timeout is up and the HTTP call to 10.20.0.1 has
--    finally failed, the SDK realizes it can't talk to the default gateway. It
--    would probably try the configured host (127.0.0.1:1302) again soon, and
--    then find the agent stub there, unless this timeout here kicks in earlier.
--
-- Thus, we need more than 5 seconds.
maxRetryDelay :: Int
maxRetryDelay = 7500 * 1000


agentStubHost :: String
agentStubHost = "127.0.0.1"


agentStubPort :: Int
agentStubPort = 1302


agentStubBaseUrl :: String
agentStubBaseUrl =
  "http://" ++ agentStubHost ++ ":" ++ (show agentStubPort) ++ "/"


agentStubUrl :: String -> String
agentStubUrl path =
  agentStubBaseUrl ++ path


appHost :: String
appHost = "127.0.0.1"


appUrl :: Int -> String -> String
appUrl port path =
  "http://" ++ appHost ++ ":" ++ (show port) ++ "/" ++ path


defaultHeaders :: [(HeaderName, ByteString)]
defaultHeaders =
  [ ("Accept", "application/json")
  , ("Content-Type", "application/json; charset=UTF-8")
  ]


requestAgentStubAndParse ::
  Aeson.FromJSON a =>
  String
  -> ByteString
  -> IO (Either String a)
requestAgentStubAndParse path method =
  requestAndParse path method [] agentStubUrl


requestAppAndParse ::
  Aeson.FromJSON a =>
  Suite.AppUnderTest
  -> String
  -> ByteString
  -> [Header]
  -> IO (Either String a)
requestAppAndParse appUnderTest path method headers =
  requestAndParse path method headers (appUrl $ Suite.port appUnderTest)


doAgentStubRequest ::
  String
  -> Method
  -> IO (HTTP.Response LBS.ByteString)
doAgentStubRequest path method =
  doRequest path method [] agentStubUrl


doAppRequest ::
  Suite.AppUnderTest
  -> String
  -> Method
  -> [Header]
  -> IO (HTTP.Response LBS.ByteString)
doAppRequest appUnderTest path method headers =
  doRequest path method headers (appUrl $ Suite.port appUnderTest)


requestAndParse ::
  Aeson.FromJSON a =>
  String
  -> Method
  -> [Header]
  -> (String -> String)
  -> IO (Either String a)
requestAndParse path method headers urlCreator = do
  response <- doRequest path method headers urlCreator
  parseResponse response


doRequest ::
  String
  -> Method
  -> [Header]
  -> (String -> String)
  -> IO (HTTP.Response LBS.ByteString)
doRequest path method headers urlCreator = do
  httpManager <- HTTP.newManager $
    HTTP.defaultManagerSettings { HTTP.managerConnCount = 5 }
  let
    url = urlCreator path
  defaultRequestSettings <- HTTP.parseUrlThrow url
  let
    request = defaultRequestSettings
       { HTTP.method = method
       , HTTP.requestHeaders = defaultHeaders ++ headers
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

