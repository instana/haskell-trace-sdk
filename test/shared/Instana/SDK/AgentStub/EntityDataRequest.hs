{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.EntityDataRequest where


import           Data.Aeson
import qualified Data.Aeson   as Aeson
import           Data.Text    (Text)
import           GHC.Generics


-- Example entity data JSON:
-- {
--   "executablePath": "/opt/very/important/service/bin/exec",
--   "arguments": ""
--   "programName": "instana-haskell-trace-sdk-integration-tests"
--   "pid": "30114"
--   "sensorVersion": "0.1.0.0"
--   "startTime": 1545569786526
--   "rts": {
--     "gc": {
--       "gc_cpu_ms": 35
--       "mutator_wall_ms": 8743
--       "mutator_cpu_ms": 155
--       "gc_wall_ms": 8
--       "wall_ms": 8751
--       "bytes_copied": 2225552
--       "max_bytes_used": 2279960
--       "max_bytes_slop": 110376
--       "num_bytes_usage_samples": 3
--       "peak_megabytes_allocated": 12
--       "cpu_ms": 190
--       "current_bytes_used": 3151432
--       "bytes_allocated": 122542896
--       "par_max_bytes_copied": 1311424
--       "current_bytes_slop": 96696
--       "cumulative_bytes_used": 3993064
--       "num_gcs": 31
--       "par_tot_bytes_copied": 2225552
--       "par_avg_bytes_copied": 2225552
--     }
--   }
-- }


data EntityDataRequest =
  EntityDataRequest
    { pid            :: Maybe Text
    , executablePath :: Maybe Text
    -- arguments are concatenated, separate by one space character
    , arguments      :: Maybe Text
    , programName    :: Maybe Text
    , startTime      :: Maybe Int
    , sensorVersion  :: Maybe Text
    , rts            :: Maybe RtsData
    } deriving (Eq, Show, Generic)


instance FromJSON EntityDataRequest
instance ToJSON EntityDataRequest


data RtsData =
  RtsData
    { gc :: Aeson.Value
    } deriving (Eq, Show, Generic)


instance FromJSON RtsData
instance ToJSON RtsData

