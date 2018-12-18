{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.LabelMetric where


import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data LabelMetric =
  LabelMetric
    { val :: Text
    } deriving (Eq, Show, Generic)


instance FromJSON LabelMetric
instance ToJSON LabelMetric

