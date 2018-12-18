{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.EntityDataRequest where


import           Data.Aeson
import qualified Data.Aeson                        as Aeson
import           GHC.Generics

import           Instana.SDK.AgentStub.LabelMetric (LabelMetric)


-- Example entity data JSON:
-- {
--   "executablePath": {
--     "type": "l",
--     "val": "/opt/very/important/service/bin/exec"
--   },
--   "arguments": {
--     "type": "l",
--     "val": ""
--   },
--   "programName": {
--     "type": "l",
--     "val": "instana-haskell-trace-sdk-integration-tests"
--   },
--   "pid": {
--     "type": "l",
--     "val": "30114"
--   },
--   "rts": {
--     "gc": {
--       "gc_cpu_ms": {
--         "type": "c",
--         "val": 35
--       },
--       "mutator_wall_ms": {
--         "type": "c",
--         "val": 8743
--       },
--       "mutator_cpu_ms": {
--         "type": "c",
--         "val": 155
--       },
--       "gc_wall_ms": {
--         "type": "c",
--         "val": 8
--       },
--       "wall_ms": {
--         "type": "c",
--         "val": 8751
--       },
--       "bytes_copied": {
--         "type": "c",
--         "val": 2225552
--       },
--       "max_bytes_used": {
--         "type": "g",
--         "val": 2279960
--       },
--       "max_bytes_slop": {
--         "type": "g",
--         "val": 110376
--       },
--       "num_bytes_usage_samples": {
--         "type": "c",
--         "val": 3
--       },
--       "peak_megabytes_allocated": {
--         "type": "g",
--         "val": 12
--       },
--       "cpu_ms": {
--         "type": "c",
--         "val": 190
--       },
--       "current_bytes_used": {
--         "type": "g",
--         "val": 3151432
--       },
--       "bytes_allocated": {
--         "type": "c",
--         "val": 122542896
--       },
--       "par_max_bytes_copied": {
--         "type": "g",
--         "val": 1311424
--       },
--       "current_bytes_slop": {
--         "type": "g",
--         "val": 96696
--       },
--       "cumulative_bytes_used": {
--         "type": "c",
--         "val": 3993064
--       },
--       "num_gcs": {
--         "type": "c",
--         "val": 31
--       },
--       "par_tot_bytes_copied": {
--         "type": "g",
--         "val": 2225552
--       },
--       "par_avg_bytes_copied": {
--         "type": "g",
--         "val": 2225552
--       }
--     }
--   }
-- }


data EntityDataRequest =
  EntityDataRequest
    { pid            :: Maybe LabelMetric
    , executablePath :: Maybe LabelMetric
    , arguments      :: Maybe LabelMetric -- arguments are concatenated
    , programName    :: Maybe LabelMetric
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

