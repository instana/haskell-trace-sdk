{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.CounterMetric where


import           Data.Aeson
import           GHC.Generics


data CounterMetric =
  CounterMetric
    { val :: Int
    } deriving (Eq, Show, Generic)


instance FromJSON CounterMetric
instance ToJSON CounterMetric

