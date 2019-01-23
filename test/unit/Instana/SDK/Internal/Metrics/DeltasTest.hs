{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.Internal.Metrics.DeltasTest (allTests) where


import qualified Data.HashMap.Strict                 as HashMap
import           Data.Maybe                          (isJust)
import           Data.Text                           (Text)
import           Test.HUnit

import qualified Instana.SDK.Internal.Metrics.Deltas as Deltas
import           Instana.SDK.Internal.Metrics.Sample (InstanaMetricValue (..),
                                                      InstanaSample,
                                                      TimedSample)
import qualified Instana.SDK.Internal.Metrics.Sample as Sample
import           Instana.SDK.Internal.Util           ((|>))


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldComputeDelta" shouldComputeDelta
    , TestLabel
        "shouldLeaveOriginalValuesUntouched"
        shouldLeaveOriginalValuesUntouched
    , TestLabel "shouldComputeCorrectDelta" shouldComputeCorrectDelta
    , TestLabel "shouldComputeAllDeltas" shouldComputeAllDeltas
    , TestLabel
        "shouldComputeDeltasWhenPreviousValueIsMissing"
        shouldComputeDeltasWhenPreviousValueIsMissing
    ]


shouldComputeDelta :: Test
shouldComputeDelta =
  let
    previous = justOneValue 500 10000
    current = justOneValue 600 12000
    withDeltas = Deltas.enrichWithDeltas previous current |> Sample.sample
    delta = HashMap.lookup "rts.gc.num_gcs_delta" withDeltas
  in
  TestCase $ assertBool "delta is present" $ isJust delta


shouldLeaveOriginalValuesUntouched :: Test
shouldLeaveOriginalValuesUntouched =
  let
    previous = justOneValue 500 10000
    current = justOneValue 600 12000
    withDeltas = Deltas.enrichWithDeltas previous current |> Sample.sample
    original = HashMap.lookup "rts.gc.num_gcs" withDeltas
  in
  TestCase $ assertBool "original value is present" $ isJust original


shouldComputeCorrectDelta :: Test
shouldComputeCorrectDelta =
  let
    previous = justOneValue 500 10000
    current = justOneValue 600 12000
    withDeltas = Deltas.enrichWithDeltas previous current |> Sample.sample
    deltaValue = getValue "rts.gc.num_gcs_delta" withDeltas
  in
  TestCase $
    assertEqual
      "delta is correct"
      50.0
      deltaValue


shouldComputeAllDeltas :: Test
shouldComputeAllDeltas =
  let
    previous = completeSample 1100 20000
    current = completeSample 1150 23000
    withDeltas = Deltas.enrichWithDeltas previous current |> Sample.sample
  in
  TestCase $ assertBool "all deltas are present" $
    (isJust $ HashMap.lookup "rts.gc.num_gcs_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.bytes_allocated_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.num_gcs_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.num_bytes_usage_samples_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.cumulative_bytes_used_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.bytes_copied_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.init_cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.init_wall_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.mutator_cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.mutator_wall_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.gc_cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.gc_wall_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.wall_ms_delta" withDeltas)


shouldComputeDeltasWhenPreviousValueIsMissing :: Test
shouldComputeDeltasWhenPreviousValueIsMissing =
  let
    previous = incompleteSample 1100 20000
    current = completeSample 1150 23000
    withDeltas = Deltas.enrichWithDeltas previous current |> Sample.sample
  in
  TestCase $ assertBool "all deltas are present" $
    (isJust $ HashMap.lookup "rts.gc.num_gcs_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.bytes_allocated_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.num_gcs_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.num_bytes_usage_samples_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.cumulative_bytes_used_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.bytes_copied_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.init_cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.init_wall_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.mutator_cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.mutator_wall_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.gc_cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.gc_wall_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.cpu_ms_delta" withDeltas) &&
    (isJust $ HashMap.lookup "rts.gc.wall_ms_delta" withDeltas)




justOneValue :: Int -> Int -> TimedSample
justOneValue metricValue timestamp =
  let
    sample =
      HashMap.singleton "rts.gc.num_gcs" (IntegralValue metricValue)
  in
  Sample.mkTimedSample sample timestamp


completeSample :: Int -> Int -> TimedSample
completeSample offset timestamp =
  let
    sample =
      HashMap.empty
        |> HashMap.insert "rts.gc.bytes_allocated"         (value offset 100)
        |> HashMap.insert "rts.gc.num_gcs"                 (value offset 110)
        |> HashMap.insert "rts.gc.num_bytes_usage_samples" (value offset 120)
        |> HashMap.insert "rts.gc.cumulative_bytes_used"   (value offset 130)
        |> HashMap.insert "rts.gc.bytes_copied"            (value offset 140)
        |> HashMap.insert "rts.gc.init_cpu_ms"             (value offset 150)
        |> HashMap.insert "rts.gc.init_wall_ms"            (value offset 160)
        |> HashMap.insert "rts.gc.mutator_cpu_ms"          (value offset 170)
        |> HashMap.insert "rts.gc.mutator_wall_ms"         (value offset 180)
        |> HashMap.insert "rts.gc.gc_cpu_ms"               (value offset 190)
        |> HashMap.insert "rts.gc.gc_wall_ms"              (value offset 200)
        |> HashMap.insert "rts.gc.cpu_ms"                  (value offset 210)
        |> HashMap.insert "rts.gc.wall_ms"                 (value offset 210)
  in
  Sample.mkTimedSample sample timestamp


incompleteSample :: Int -> Int -> TimedSample
incompleteSample offset timestamp =
  let
    sample =
      HashMap.empty
        |> HashMap.insert "rts.gc.bytes_allocated"         (value offset 100)
        |> HashMap.insert "rts.gc.num_bytes_usage_samples" (value offset 120)
        |> HashMap.insert "rts.gc.cumulative_bytes_used"   (value offset 130)
        |> HashMap.insert "rts.gc.init_cpu_ms"             (value offset 150)
        |> HashMap.insert "rts.gc.init_wall_ms"            (value offset 160)
        |> HashMap.insert "rts.gc.mutator_wall_ms"         (value offset 180)
        |> HashMap.insert "rts.gc.gc_wall_ms"              (value offset 200)
        |> HashMap.insert "rts.gc.wall_ms"                 (value offset 210)
  in
  Sample.mkTimedSample sample timestamp


value :: Int -> Int -> InstanaMetricValue
value offset val =
  IntegralValue $ offset + val


getValue :: Text -> InstanaSample -> Double
getValue key sample =
  let
    maybeValue = HashMap.lookup key sample
  in
  case maybeValue of
    Just (FractionalValue v) -> v
    -- arbitrary value that won't match any expectation
    _                        -> -1000

