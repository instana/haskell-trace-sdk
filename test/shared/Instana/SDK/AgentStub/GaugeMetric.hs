{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.GaugeMetric where


import           Data.Aeson
import           GHC.Generics


data GaugeMetric =
  GaugeMetric
    { val :: Int
    } deriving (Eq, Show, Generic)


instance FromJSON GaugeMetric
instance ToJSON GaugeMetric

