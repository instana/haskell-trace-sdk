{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.DiscoveryResponse where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data DiscoveryResponse =
  DiscoveryResponse
   { pid       :: Int
   , agentUuid :: Text
   } deriving (Eq, Show, Generic)


instance ToJSON DiscoveryResponse

