{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.Internal.Metrics.CompressionTest (allTests) where


import qualified Data.HashMap.Strict                      as HashMap
import           Test.HUnit

import qualified Instana.SDK.Internal.Metrics.Compression as MetricsCompression
import           Instana.SDK.Internal.Metrics.Sample      (InstanaMetricValue (..))


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldNotRemoveNewMetrics" shouldNotRemoveNewMetrics
    , TestLabel "shouldNotRemoveChangedMetrics" shouldNotRemoveChangedMetrics
    , TestLabel "shouldRemoveUnchangedMetrics" shouldRemoveUnchangedMetrics
    , TestLabel "shouldLeaveChangedUntouched" shouldLeaveChangedUntouched
    ]


shouldNotRemoveNewMetrics :: Test
shouldNotRemoveNewMetrics =
  let
    previous =
      HashMap.empty
    next =
      HashMap.singleton "key" (StringValue "value")
    compressed =
      MetricsCompression.compressSample previous next
  in
  TestCase $
    assertEqual
      "don't remove new"
      (Just $ StringValue "value")
      (HashMap.lookup "key" compressed)


shouldNotRemoveChangedMetrics :: Test
shouldNotRemoveChangedMetrics =
  let
    previous =
      HashMap.singleton "key" (StringValue "value 1")
    next =
      HashMap.singleton "key" (StringValue "value 2")
    compressed =
      MetricsCompression.compressSample previous next
  in
  TestCase $
    assertEqual
      "don't remove changed"
      (Just $ StringValue "value 2")
      (HashMap.lookup "key" compressed)


shouldRemoveUnchangedMetrics :: Test
shouldRemoveUnchangedMetrics =
  let
    previous =
      HashMap.insert "key 2" (StringValue "value 2") $
        HashMap.singleton "key 1" (StringValue "value 1")
    next =
      HashMap.insert "key 2" (StringValue "value 2 changed") $
        HashMap.singleton "key 1" (StringValue "value 1")
    compressed =
      MetricsCompression.compressSample previous next
  in
  TestCase $
    assertEqual
      "remove unchanged"
      Nothing
      (HashMap.lookup "key 1" compressed)


shouldLeaveChangedUntouched :: Test
shouldLeaveChangedUntouched =
  let
    previous =
      HashMap.insert "key 2" (StringValue "value 2") $
        HashMap.singleton "key 1" (StringValue "value 1")
    next =
      HashMap.insert "key 2" (StringValue "value 2 changed") $
        HashMap.singleton "key 1" (StringValue "value 1")
    compressed =
      MetricsCompression.compressSample previous next
  in
  TestCase $
    assertEqual
      "leave changed"
      (Just $ StringValue "value 2 changed")
      (HashMap.lookup "key 2" compressed)

