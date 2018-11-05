{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.DiscoveryRequest where


import           Data.Aeson
import           GHC.Generics


data DiscoveryRequest =
  DiscoveryRequest
    { pid      :: String
    , progName :: String
    , execPath :: String
    , args     :: [String]
    } deriving (Eq, Show, Generic)


instance FromJSON DiscoveryRequest
instance ToJSON DiscoveryRequest

