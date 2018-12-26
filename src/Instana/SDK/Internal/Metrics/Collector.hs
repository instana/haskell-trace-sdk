{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Collector
Description : Initializes the collection of metrics and samples them at
              regular intervals.
-}
module Instana.SDK.Internal.Metrics.Collector
  ( registerMetrics
  , sampleAll
  ) where


import qualified Data.List                                        as List
import           Data.Text                                        (Text)
import qualified Data.Text                                        as T
import qualified System.Metrics                                   as Metrics

import           Instana.SDK.Internal.AgentConnection.ProcessInfo (ProcessInfo)
import qualified Instana.SDK.Internal.AgentConnection.ProcessInfo as ProcessInfo
import           Instana.SDK.Internal.Util                        ((|>))


registerMetrics :: String -> ProcessInfo -> Int -> IO Metrics.Store
registerMetrics translatedPid processInfo sdkStartTime = do
  instanaMetricsStore <- Metrics.newStore
  -- register Instana specific metrics (mostly snapshot data)
  registerCustomMetrics
    instanaMetricsStore
    translatedPid
    processInfo
    sdkStartTime
  -- register all predefined GC metrics provided by ekg
  Metrics.registerGcMetrics instanaMetricsStore
  return instanaMetricsStore


sampleAll :: Metrics.Store -> IO Metrics.Sample
sampleAll = Metrics.sampleAll


registerCustomMetrics ::
  Metrics.Store
  -> String
  -> ProcessInfo
  -> Int
  -> IO ()
registerCustomMetrics
    instanaMetricsStore
    translatedPid
    processInfo
    _ = do
  registerConstantMetric
    instanaMetricsStore
    "pid"
    translatedPid
  registerConstantMetric
    instanaMetricsStore
    "programName"
    (ProcessInfo.programName processInfo)
  registerConstantMetric
    instanaMetricsStore
    "executablePath"
    (ProcessInfo.executablePath processInfo)
  registerConstantMetric
    instanaMetricsStore
    "arguments"
    (ProcessInfo.arguments processInfo |> List.intercalate " ")


registerConstantMetric :: Metrics.Store -> Text -> String -> IO ()
registerConstantMetric instanaMetricsStore label value = do
  Metrics.registerLabel label (return $ T.pack value) instanaMetricsStore

