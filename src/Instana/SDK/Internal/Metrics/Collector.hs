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
import           Data.Time.Clock.POSIX                            (getPOSIXTime)
import qualified Data.Version                                     as Version
import           Paths_instana_haskell_trace_sdk                  (version)
import qualified System.Metrics                                   as Metrics
import qualified System.SysInfo                                   as SysInfo

import           Instana.SDK.Internal.AgentConnection.ProcessInfo (ProcessInfo)
import qualified Instana.SDK.Internal.AgentConnection.ProcessInfo as ProcessInfo
import           Instana.SDK.Internal.Util                        ((|>))


{-| Creates the ekg metric store and registers all relevant metrics for regular
collection.
-}
registerMetrics :: String -> ProcessInfo -> Int -> IO Metrics.Store
registerMetrics translatedPid processInfo sdkStartTime = do
  -- registerMetrics is executed once more after each connection loss/reconnect.
  -- It should not be an actual problem, as the previous metrics store should
  -- have been garbage collected.

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


{-| Collects the current value for all registered metrics.
-}
sampleAll :: Metrics.Store -> IO Metrics.Sample
sampleAll = Metrics.sampleAll


{-| Registers custom metrics (not included in the ekg default metrics).
-}
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
    sdkStartTime = do
  startTime <- calcStartTime sdkStartTime
  registerConstantLabelMetric
    instanaMetricsStore
    "pid"
    translatedPid
  registerConstantLabelMetric
    instanaMetricsStore
    "programName"
    (ProcessInfo.programName processInfo)
  registerConstantLabelMetric
    instanaMetricsStore
    "executablePath"
    (ProcessInfo.executablePath processInfo)
  registerConstantLabelMetric
    instanaMetricsStore
    "arguments"
    (ProcessInfo.arguments processInfo |> List.intercalate " ")
  registerConstantLabelMetric
    instanaMetricsStore
    "sensorVersion"
    (Version.showVersion version)
  registerConstantCounterMetric
    instanaMetricsStore
    "startTime"
    startTime


calcStartTime :: Int -> IO Int
calcStartTime sdkStartTime = do
  sysInfoOrError <- SysInfo.sysInfo
  now <- round . (* 1000) <$> getPOSIXTime
  case sysInfoOrError of
    Right sysInfo -> do
      return $ now - (fromIntegral $ SysInfo.uptime sysInfo)
    Left _ ->
      -- System.SysInfo is not available on non-Linux systems, we use the time
      -- when the SDK has been initialized as a fallback.
      return sdkStartTime


registerConstantLabelMetric :: Metrics.Store -> Text -> String -> IO ()
registerConstantLabelMetric instanaMetricsStore label value = do
  Metrics.registerLabel label (return $ T.pack value) instanaMetricsStore


registerConstantCounterMetric :: Metrics.Store -> Text -> Int -> IO ()
registerConstantCounterMetric instanaMetricsStore label value = do
  Metrics.registerCounter
    label
    (return $ fromIntegral value)
    instanaMetricsStore

