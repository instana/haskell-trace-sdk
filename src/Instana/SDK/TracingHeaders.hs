{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.TracingHeaders
Description : A set of tracing headers
-}
module Instana.SDK.TracingHeaders
  ( TracingHeaders(..)
  , TracingLevel(..)
  , levelHeaderName
  , maybeStringToTracingLevel
  , spanIdHeaderName
  , stringToTracingLevel
  , traceIdHeaderName
  , tracingLevelToString
  ) where


import           GHC.Generics
import qualified Network.HTTP.Types.Header as HTTPHeader


-- |X-INSTANA-T
traceIdHeaderName :: HTTPHeader.HeaderName
traceIdHeaderName = "X-INSTANA-T"


-- |X-INSTANA-S
spanIdHeaderName :: HTTPHeader.HeaderName
spanIdHeaderName = "X-INSTANA-S"


-- |X-INSTANA-L
levelHeaderName :: HTTPHeader.HeaderName
levelHeaderName = "X-INSTANA-L"


-- |Tracing level.
data TracingLevel =
    -- |Record calls.
    Trace
    -- |Don't record calls.
  | Suppress
  deriving (Eq, Generic, Show)


-- |Converts a string into the tracing level.
stringToTracingLevel :: String -> TracingLevel
stringToTracingLevel s =
  if s == "0" then Suppress else Trace


-- |Converts a string into the tracing level.
maybeStringToTracingLevel :: Maybe String -> TracingLevel
maybeStringToTracingLevel s =
  if s == Just "0" then Suppress else Trace


-- |Converts tracing level into a string.
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

