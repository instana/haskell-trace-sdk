{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.ServerTiming
Description : Add/update the Server-Timing header
-}
module Instana.SDK.Internal.ServerTiming
  ( addTraceIdToServerTiming
  ) where


import qualified Data.ByteString.Char8   as BSC8
import qualified Network.HTTP.Types      as HTTPTypes
import           Text.Regex              (Regex)
import qualified Text.Regex              as Regex

import           Instana.SDK.Internal.Id (Id)
import qualified Instana.SDK.Internal.Id as Id


addTraceIdToServerTiming ::
  Id
  -> HTTPTypes.ResponseHeaders
  -> HTTPTypes.ResponseHeaders
addTraceIdToServerTiming traceId headers =
  let
    existingValue = lookup "Server-Timing" headers
    result =
      case existingValue of
        Nothing ->
          addServerTimingHeader traceId headers
        Just existingMetrics ->
          appendInTIdToServerTimingHeader traceId existingMetrics headers
  in
  result


addServerTimingHeader ::
  Id
  -> HTTPTypes.ResponseHeaders
  -> HTTPTypes.ResponseHeaders
addServerTimingHeader traceId headers =
  let
    newValue = (BSC8.pack "intid;desc=") <> Id.toByteString traceId
  in
  headers ++ [("Server-Timing", newValue)]


appendInTIdToServerTimingHeader ::
  Id
  -> BSC8.ByteString
  -> HTTPTypes.ResponseHeaders
  -> HTTPTypes.ResponseHeaders
appendInTIdToServerTimingHeader traceId existingMetrics headers =
  if (BSC8.isInfixOf "intid;desc=" existingMetrics)
    then
      replaceExistingInTIdMetric traceId existingMetrics headers
    else
      appendInTIdMetricAtEnd traceId existingMetrics headers


appendInTIdMetricAtEnd ::
  Id
  -> BSC8.ByteString
  -> HTTPTypes.ResponseHeaders
  -> HTTPTypes.ResponseHeaders
appendInTIdMetricAtEnd traceId existingMetrics headers =
  let
    newServerTimingValue =
      existingMetrics <>
      (BSC8.pack ", intid;desc=") <>
      Id.toByteString traceId
    headersWithoutServerTiming =
      filter (\(k, _) -> k /= "Server-Timing")  headers
  in
  headersWithoutServerTiming ++ [("Server-Timing", newServerTimingValue)]


replaceExistingInTIdMetric ::
  Id
  -> BSC8.ByteString
  -> HTTPTypes.ResponseHeaders
  -> HTTPTypes.ResponseHeaders
replaceExistingInTIdMetric traceId existingMetrics headers =
  let
    current = BSC8.unpack existingMetrics
    replaced =
      Regex.subRegex
        replaceExistingRegex
        current
        ("intid;desc=" ++ Id.toString traceId)
    newServerTimingValue = BSC8.pack replaced
    headersWithoutServerTiming =
      filter (\(k, _) -> k /= "Server-Timing")  headers
  in
  headersWithoutServerTiming ++ [("Server-Timing", newServerTimingValue)]


replaceExistingRegex :: Regex
replaceExistingRegex =
  Regex.mkRegex "intid;desc=[^,]*"

