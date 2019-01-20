{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.IntegrationTest.Metrics
  ( shouldReportMetrics
  ) where


import qualified Data.Aeson                              as Aeson
import qualified Data.HashMap.Strict                     as HashMap
import qualified Data.List                               as List
import           Data.Maybe                              (isJust)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Test.HUnit

import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import qualified Instana.SDK.AgentStub.EntityDataRequest as EntityDataRequest

import           Instana.SDK.IntegrationTest.HUnitExtra  (applyLabel,
                                                          assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.TestHelper  as TestHelper


shouldReportMetrics :: String -> IO Test
shouldReportMetrics pid =
  applyLabel "shouldReportMetrics" $ do
    entityDataRequestsResult <- TestHelper.waitForEntityDataWithPid pid
    case entityDataRequestsResult of
      Left failure ->
        failIO $
          "Could not load recorded entity data requests from agent stub: " ++
          failure
      Right [] -> do
        failIO $
          "Could not load recorded entity data requests from agent stub " ++
          "- received empty list."
      Right [entityData] -> do
        assertAllIO
          [ assertLabelIs "pid" pid (EntityDataRequest.pid entityData)
          , assertLabelContains
              "executable path"
              "instana-haskell-test-wai-server"
              (EntityDataRequest.executablePath entityData)
          , assertLabelIs
              "program name"
              "instana-haskell-test-wai-server"
              (EntityDataRequest.programName entityData)
          , assertLabelIs
              "arguments"
              ""
              (EntityDataRequest.arguments entityData)
          , assertLabelIs
              "sensorVersion"
              "0.1.0.0"
              (EntityDataRequest.sensorVersion entityData)
          , assertCounterSatisfies
              "startTime"
              (1545570995405 <)
              (EntityDataRequest.startTime entityData)

          , assertMetricsArePresent
              "RTS GC metrics does not contain all expected metric keys"
              -- The list of metrics might depend on GHC version and ekg-core
              -- version, so this test is potentially fragile. If it starts to
              -- make trouble, we could also just check for one well known
              -- metric that is always present, should be good enough.
              [ "gc_cpu_ms"
              , "mutator_wall_ms"
              , "mutator_cpu_ms"
              , "gc_wall_ms"
              , "wall_ms"
              , "bytes_copied"
              , "max_bytes_used"
              , "max_bytes_slop"
              , "num_bytes_usage_samples"
              , "peak_megabytes_allocated"
              , "cpu_ms"
              , "current_bytes_used"
              , "bytes_allocated"
              , "par_max_bytes_copied"
              , "current_bytes_slop"
              , "cumulative_bytes_used"
              , "num_gcs"
              , "par_tot_bytes_copied"
              , "par_avg_bytes_copied"
              ]
              entityData
          ]
      _ -> do
        failIO $
          "Unexpectedly received multiple recorded entity data requests " ++
          "from agent stub "


assertLabelIs ::
  String
  -> String
  -> Maybe Text
  -> Assertion
assertLabelIs testLabel expectedValue labelMetric =
  assertEqual testLabel (label expectedValue) labelMetric


assertLabelContains ::
  String
  -> String
  -> Maybe Text
  -> Assertion
assertLabelContains testLabel expectedValue labelMetric =
  if isJust labelMetric then
    let
      Just labelContent = labelMetric
    in
    assertBool testLabel (T.isInfixOf (T.pack expectedValue) labelContent)
  else
    assertFailure $ testLabel ++ " - label metric is Nothing"


label :: String -> Maybe Text
label s =
  Just $ T.pack s


assertCounterSatisfies ::
  String
  -> (Int -> Bool)
  -> Maybe Int
  -> Assertion
assertCounterSatisfies testLabel predicate counterMetric =
  case counterMetric of
    Just metric ->
      assertBool testLabel (predicate $ metric)
    Nothing ->
      assertFailure $ testLabel ++ " - counter metric is Nothing"


assertMetricsArePresent :: String -> [String] -> EntityDataRequest -> Assertion
assertMetricsArePresent testLabel expectedMetrics entityData =
  if isJust $ EntityDataRequest.rts entityData then
    let
      Just rtsData = EntityDataRequest.rts entityData
      gcMetricsJson = EntityDataRequest.gc rtsData
      (decodingResult :: Aeson.Result Aeson.Object) = Aeson.fromJSON gcMetricsJson
    in
    case decodingResult of
      Aeson.Error e ->
        assertFailure $ testLabel ++ " - unparseable GC metrics - " ++ e
      Aeson.Success decoded ->
        let
          containsAllMetrics =
            List.foldl
              (\allTrue metricKey ->
                allTrue && HashMap.member (T.pack metricKey) decoded
              )
            True
            expectedMetrics
        in
        assertBool testLabel containsAllMetrics
  else
    assertFailure $ testLabel ++ " - no rts data"

