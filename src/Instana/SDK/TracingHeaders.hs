{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.TracingHeaders
Description : A set of tracing headers
-}
module Instana.SDK.TracingHeaders
  ( TracingHeaders(..)
  , TracingLevel(..)
  , maybeStringToTracingLevel
  , stringToTracingLevel
  , tracingLevelToString
  ) where


import           GHC.Generics


data TracingLevel =
    -- |Record calls.
    Trace
    -- |Don't record calls.
  | Suppress
  deriving (Eq, Generic, Show)


stringToTracingLevel :: String -> TracingLevel
stringToTracingLevel s =
  if s == "0" then Suppress else Trace


maybeStringToTracingLevel :: Maybe String -> TracingLevel
maybeStringToTracingLevel s =
  if s == Just "0" then Suppress else Trace


tracingLevelToString :: TracingLevel -> String
tracingLevelToString l =
  case l of
    Trace    -> "1"
    Suppress -> "0"


-- |A set of tracing headers.
data TracingHeaders  =
  TracingHeaders
    {
      -- |the trace ID
      traceId :: Maybe String
      -- |the span ID
    , spanId  :: Maybe String
      -- |the tracing level (on/off)
    , level   :: TracingLevel
    } deriving (Eq, Generic, Show)

