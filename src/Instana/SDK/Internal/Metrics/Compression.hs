{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Compression
Description : Removes unchanged metrics from the sampled metrics to save
              bandwidth/CPU (JSON seralization/deserialization).
-}
module Instana.SDK.Internal.Metrics.Compression
  ( compressSample
  ) where


import qualified Data.HashMap.Strict                 as HashMap

import           Instana.SDK.Internal.Metrics.Sample (InstanaMetricValue,
                                                      InstanaSample)


compressSample :: InstanaSample -> InstanaSample -> InstanaSample
compressSample previous next =
  HashMap.differenceWith dropUnchanged next previous


dropUnchanged ::
  InstanaMetricValue
  -> InstanaMetricValue
  -> Maybe InstanaMetricValue
dropUnchanged nextValue previousValue =
  if nextValue == previousValue then
    Nothing
  else
    Just nextValue

