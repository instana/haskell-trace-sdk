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
  , parseXInstanaL
  , spanIdHeaderName
  , stringToTracingLevel
  , syntheticHeaderName
  , traceIdHeaderName
  , tracingLevelToString
  ) where


import           GHC.Generics
import qualified Network.HTTP.Types.Header as HTTPHeader
import           Text.Regex.PCRE           ((=~))


-- |X-INSTANA-T
traceIdHeaderName :: HTTPHeader.HeaderName
traceIdHeaderName = "X-INSTANA-T"


-- |X-INSTANA-S
spanIdHeaderName :: HTTPHeader.HeaderName
spanIdHeaderName = "X-INSTANA-S"


-- |X-INSTANA-L
levelHeaderName :: HTTPHeader.HeaderName
levelHeaderName = "X-INSTANA-L"


-- |X-INSTANA-SYNTHETIC
syntheticHeaderName :: HTTPHeader.HeaderName
syntheticHeaderName = "X-INSTANA-SYNTHETIC"


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


-- |Parses the X-INSTANA-L value to determine the tracing level, and optionally
-- the correlation type and correlation ID.
parseXInstanaL :: Maybe String -> (TracingLevel, Maybe String, Maybe String)
parseXInstanaL xInstanaLValueMaybe =
  case xInstanaLValueMaybe of
  Nothing ->
    (Trace, Nothing, Nothing)
  Just xInstanaLValue ->
    let
      (_, _, _, groups) =
        xInstanaLValue =~ xInstanaLRegex :: (String, String, String, [String])
    in
    case groups of
      [] ->
        (Trace, Nothing, Nothing)
      ["", "", ""] ->
        (Trace, Nothing, Nothing)
      ["0", _, _] ->
        (Suppress, Nothing, Nothing)
      ["1", "", ""] ->
        (Trace, Nothing, Nothing)
      ["1", _correlationType, _correlationId] ->
        (Trace, Just _correlationType, Just _correlationId)
      _ ->
        (Trace, Nothing, Nothing)


xInstanaLRegex :: String
xInstanaLRegex =
  -- example "1,correlationType=web;correlationId=1234567890abcdef"
  "^\\s*([01])\\s*(?:,\\s*correlationType\\s*=\\s*([^ ;]*)\\s*;\\s*correlationId\\s*=\\s*([^ ;]*)\\s*)?$"


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
      traceId         :: Maybe String
      -- |the span ID
    , spanId          :: Maybe String
      -- |the tracing level (on/off)
    , level           :: TracingLevel
      -- |eum correlation type
    , correlationType :: Maybe String
      -- |eum correlation ID
    , correlationId   :: Maybe String
      -- |synthetic flag
    , synthetic       :: Bool
    } deriving (Eq, Generic, Show)

