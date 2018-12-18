{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Compression
Description : Removes unchanged metrics from the sampled metrics to save
              bandwidth/CPU (JSON seralization/deserialization).
-}
module Instana.SDK.Internal.Metrics.Compression
  ( compressSample
  ) where


import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics      as Metrics


compressSample :: Metrics.Sample -> Metrics.Sample -> Metrics.Sample
compressSample previous next =
  HashMap.differenceWith dropUnchanged next previous


dropUnchanged :: Metrics.Value -> Metrics.Value -> Maybe Metrics.Value
dropUnchanged nextValue previousValue =
  if nextValue == previousValue then
    Nothing
  else
    Just nextValue

