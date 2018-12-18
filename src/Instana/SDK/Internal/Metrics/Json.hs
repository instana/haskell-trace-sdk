{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Json
Description : ToJSON instances for System.Metrics values
-}
module Instana.SDK.Internal.Metrics.Json where


import           System.Metrics (Value (..))
import qualified System.Metrics as Metrics


