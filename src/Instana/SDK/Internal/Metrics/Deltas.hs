{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Delta
Description : Computes deltas between two metrics samples.
-}
module Instana.SDK.Internal.Metrics.Deltas
  ( enrichWithDeltas
  ) where


import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Text                           (Text)
import qualified Data.Text                           as T

import           Instana.SDK.Internal.Metrics.Sample (InstanaMetricValue,
                                                      TimedSample)
import qualified Instana.SDK.Internal.Metrics.Sample as Sample


whitelist :: [Text]
whitelist =
  [ "rts.gc.bytes_allocated"
  , "rts.gc.num_gcs"
  , "rts.gc.num_bytes_usage_samples"
  , "rts.gc.cumulative_bytes_used"
  , "rts.gc.bytes_copied"
  , "rts.gc.init_cpu_ms"
  , "rts.gc.init_wall_ms"
  , "rts.gc.mutator_cpu_ms"
  , "rts.gc.mutator_wall_ms"
  , "rts.gc.gc_cpu_ms"
  , "rts.gc.gc_wall_ms"
  , "rts.gc.cpu_ms"
  , "rts.gc.wall_ms"
  ]


enrichWithDeltas :: TimedSample -> TimedSample -> TimedSample
enrichWithDeltas previousSample currentSample =
  let
    currentTimestamp = Sample.timestamp currentSample
    previousTimestamp = Sample.timestamp previousSample
    deltaT = currentTimestamp - previousTimestamp

    previousMetrics = Sample.sample previousSample
    currentMetrics = Sample.sample currentSample

    metricsWithDeltas =
      HashMap.foldrWithKey
        (addDeltaToSample deltaT previousMetrics)
        currentMetrics
        currentMetrics
  in
    Sample.mkTimedSample metricsWithDeltas currentTimestamp


addDeltaToSample ::
  Int
  -> HashMap Text InstanaMetricValue
  -> Text
  -> InstanaMetricValue
  -> HashMap Text InstanaMetricValue
  -> HashMap Text InstanaMetricValue
addDeltaToSample deltaT previousMetrics metricKey currentMetricValue currentMetrics  =
  if not (elem metricKey whitelist) then
    currentMetrics
  else
    let
      previousMetricValue = HashMap.lookup metricKey previousMetrics
    in
    case (previousMetricValue, currentMetricValue) of
      -- We are only interested in integer metrics here. Reason: The ekg package
      -- only emits integer values and only the deltas are fractional values. We
      -- never want to compute a delta of two deltas, that wouldn't make sense.
      (Just (Sample.IntegralValue previousValue),
        Sample.IntegralValue currentValue) ->
        addNormalizedDelta
          metricKey
          deltaT
          (currentValue - previousValue)
          currentMetrics
      -- The metric is present in the current sample but was not present in the
      -- previous sample, that must be the first sample taken ever. Assume zero
      -- for the previous value.
      (Nothing, Sample.IntegralValue currentValue) ->
        addNormalizedDelta
          metricKey
          deltaT
          currentValue
          currentMetrics
      _ ->
        currentMetrics


addNormalizedDelta ::
  Text
  -> Int
  -> Int
  -> HashMap Text InstanaMetricValue
  -> HashMap Text InstanaMetricValue
addNormalizedDelta metricKey deltaT deltaV metrics =
  let
    deltaKey = T.append metricKey "_delta"
    -- Normalize difference to a one second timespan, no matter how much time
    -- elapsed between taking the two samples. The provided timestamps (and thus
    -- deltaT) are in milliseconds.
    normalizedDeltaV =
      Sample.FractionalValue $
        (fromIntegral deltaV / fromIntegral deltaT) * 1000
  in
  HashMap.insert
    deltaKey
    normalizedDeltaV
    metrics

