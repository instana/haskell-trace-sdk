{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Internal.Retry
Description : Handles retrying IO actions that might fail (mostly HTTP requests)
-}
module Instana.SDK.Internal.Retry
    ( acceptDataRetryPolicy
    , agentHostLookupRetryPolicy
    , announceRetryPolicy
    , retryRequest
    , retryUntil
    , retryUntilRight
    ) where

import           Control.Monad.Catch  (Handler)
import qualified Control.Retry        as Retry
import           Data.ByteString.Lazy (ByteString)
import           Data.Semigroup       ((<>))
import qualified Network.HTTP.Client  as HTTP


{-| A fibonacci retry delay pattern starting with a 1 second delay going up
to 1 minute delay. This retry policy never gives up.
-}
agentHostLookupRetryPolicy :: Retry.RetryPolicyM IO
agentHostLookupRetryPolicy =
  Retry.capDelay maxDelay $
    Retry.fibonacciBackoff minDelay
  where
    -- 1 second
    minDelay = 1 * 1000 * 1000
    -- 60 seconds
    maxDelay = 60 * 1000 * 1000


{-| A constant delay pattern with a 200 ms second delay. This retry policy
gives up after 3 attempts.
-}
announceRetryPolicy :: Retry.RetryPolicyM IO
announceRetryPolicy =
  (Retry.constantDelay delay) <> (Retry.limitRetries retries)
  where
    delay = 200 * 1000
    retries = 3


{-| A constant delay pattern with a 10 second delay. This retry policy
gives up after 10 attempts.
-}
acceptDataRetryPolicy :: Retry.RetryPolicyM IO
acceptDataRetryPolicy =
  (Retry.constantDelay delay) <> (Retry.limitRetries retries)
  where
    delay = 10 * 1000 * 1000
    retries = 10


{-| Retries a given HTTP request according to the given retry policy,
until either the request succeeds or the retry policy mandates to stop retrying.
-}
retryRequest ::
  Retry.RetryPolicyM IO
  -> (HTTP.Response ByteString -> IO a)
  -> IO (HTTP.Response ByteString)
  -> IO a
retryRequest retryPolicy decoder request =
  let
    reportHttpError :: Bool -> HTTP.HttpException -> Retry.RetryStatus -> IO ()
    reportHttpError _ _ _ {- retriedOrCrashed err retryStatus -}  =
      return ()
       -- For a detailed error message, we could use Retry.defaultLogMsg, like
       -- this:
       -- traceM instanaLogger $
       --   Retry.defaultLogMsg retriedOrCrashed err retryStatus
       -- But we generally do not want this level of verbosity here in retry,
       -- instead, we are perfectly fine with swallowing the excetption
       -- completely.
    retryOnAnyHttpError :: HTTP.HttpException -> IO Bool
    retryOnAnyHttpError _ = return True
    retryOnAnyStatus :: Retry.RetryStatus -> Handler IO Bool
    retryOnAnyStatus = Retry.logRetries retryOnAnyHttpError reportHttpError

    executeRequestAndParseResponse = do
      response <- request
      decoded <- decoder response
      return decoded

  in
    Retry.recovering
       retryPolicy
       [retryOnAnyStatus]
       (const executeRequestAndParseResponse)


{-| Retries a given action according to the given retry policy, until it either
yields a result matching Right _ or until the retry policy mandates to stop
retrying, which ever happens first.
-}
retryUntilRight ::
  forall a.
  Show a =>
  Retry.RetryPolicyM IO
  -> IO (Either String a)
  -> IO (Either String a)
retryUntilRight retryPolicy action =
  retryUntil retryPolicy (\_ -> True) action


{-| Retries a given action according to the given retry policy, until either the
given retry check function returns True or the retry policy mandates to stop
retrying.
-}
retryUntil ::
  forall a.
  Show a =>
  Retry.RetryPolicyM IO
  -> (a -> Bool)
  -> IO (Either String a)
  -> IO (Either String a)
retryUntil retryPolicy retryCheck action = do
  let
    check :: Retry.RetryStatus -> Either String a -> IO Bool
    check _ result = do
      case result of
        Left _ ->
          return True
        Right value ->
          return $ not $ retryCheck value
  lastResult <- Retry.retrying
     retryPolicy
     check
     (const action)
  case lastResult of
    Left _ ->
      return lastResult
    Right lastValue ->
      if retryCheck lastValue
      then
        return lastResult
      else
        return $
          Left $
            "The retried action has not yielded the expected result " ++
            "but: " ++ show lastValue

