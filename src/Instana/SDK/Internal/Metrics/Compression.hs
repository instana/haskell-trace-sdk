{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Compression
Description : Removes unchanged metrics from the sampled metrics to save
              bandwidth/CPU (JSON seralization/deserialization).
-}
module Instana.SDK.Internal.Metrics.Compression
  ( compressSample
  ) where


import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import qualified Data.List                           as List
import           Data.Text                           (Text)

import           Instana.SDK.Internal.Metrics.Deltas (deltaKeyList)
import           Instana.SDK.Internal.Metrics.Sample (InstanaMetricValue (IntegralValue),
                                                      InstanaSample)


dummyMapWithKeysEligibleForDeltaComputation :: HashMap Text InstanaMetricValue
dummyMapWithKeysEligibleForDeltaComputation =
  List.foldl
    (\dummyMap key -> HashMap.insert key dummyValue dummyMap)
    HashMap.empty
    deltaKeyList
  where
     dummyValue = IntegralValue 0


-- |Removes metric values that are identical in the both samples, also removes
-- original values for which deltas have been computed.
compressSample :: InstanaSample -> InstanaSample -> InstanaSample
compressSample previous next =
  let
     -- step 1: remove the original metrics from which we have computed deltas,
     -- those are not used in the back end (we are only interested in the delta
     -- values of those metrics)
    deltaSourcesRemoved =
      HashMap.difference
        next
        dummyMapWithKeysEligibleForDeltaComputation
  in
  -- step 2: remove all values for which the value has not changed since the
  -- last sent metric payload
  HashMap.differenceWith dropUnchanged deltaSourcesRemoved previous


dropUnchanged ::
  InstanaMetricValue
  -> InstanaMetricValue
  -> Maybe InstanaMetricValue
dropUnchanged nextValue previousValue =
  if nextValue == previousValue then
    Nothing
  else
    Just nextValue

