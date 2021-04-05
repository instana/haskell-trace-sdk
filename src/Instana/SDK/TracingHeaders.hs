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
  , readHttpTracingHeaders
  , spanIdHeaderName
  , stringToTracingLevel
  , syntheticHeaderName
  , traceIdHeaderName
  , traceparentHeaderName
  , tracestateHeaderName
  , tracingLevelToString
  ) where


import qualified Data.ByteString.Char8     as BSC8
import qualified Data.List                 as List
import           GHC.Generics
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.Wai               as Wai
import           Text.Regex.PCRE           ((=~))

import           Instana.SDK.Internal.Util ((|>))


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


-- |traceparent
traceparentHeaderName :: HTTPHeader.HeaderName
traceparentHeaderName = "traceparent"


-- |tracestate
tracestateHeaderName :: HTTPHeader.HeaderName
tracestateHeaderName = "tracestate"


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
      -- |W3C Trace Context traceparent
    , traceparent     :: Maybe String
      -- |W3C Trace Context tracestate
    , tracestate      :: Maybe String
    } deriving (Eq, Generic, Show)


-- |Reads the Instana tracing headers
-- (https://docs.instana.io/core_concepts/tracing/#http-tracing-headers) from
-- the given request.
readHttpTracingHeaders :: Wai.Request -> TracingHeaders
readHttpTracingHeaders request =
  let
    headers = Wai.requestHeaders request
    -- lookup is automatically case insensitive because
    -- HeaderName = CI ByteString (CI -> Case Insensitive String)
    tId =
      headers
      |> List.lookup traceIdHeaderName
      |> (<$>) BSC8.unpack
    sId =
      headers
      |> List.lookup spanIdHeaderName
      |> (<$>) BSC8.unpack
    xInstanaLValue =
      headers
      |> List.lookup levelHeaderName
      |> (<$>) BSC8.unpack
    (lvl, crtp, crid) =
      parseXInstanaL xInstanaLValue
    sy =
      headers
      |> List.lookup syntheticHeaderName
      |> (<$>) BSC8.unpack
    tp =
      headers
      |> List.lookup traceparentHeaderName
      |> (<$>) BSC8.unpack
    ts =
      headers
      |> List.lookup tracestateHeaderName
      |> (<$>) BSC8.unpack
  in
  TracingHeaders
    { traceId = tId
    , spanId = sId
    , level = lvl
    , correlationType = crtp
    , correlationId = crid
    , synthetic = sy == (Just "1")
    , traceparent = tp
    , tracestate = ts
    }

