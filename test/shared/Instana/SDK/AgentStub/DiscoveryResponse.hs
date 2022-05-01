{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.DiscoveryResponse where

import           Data.Aeson
import           Data.Aeson.Casing as AesonCasing
import           Data.Text         (Text)
import           GHC.Generics


data DiscoveryResponse =
  DiscoveryResponse
   { pid              :: Int
   , agentUuid        :: Text
   , tracing          :: Maybe TracingConfig
   , extraHeaders     :: Maybe [Text]
   , secrets          :: SecretsConfig
   , unknownAttribute :: String
   } deriving (Eq, Show, Generic)

instance ToJSON DiscoveryResponse


data TracingConfig = TracingConfig
  { extraHttpHeaders :: Maybe [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON TracingConfig where
   toJSON = genericToJSON $ AesonCasing.aesonDrop 0 AesonCasing.trainCase


data SecretsConfig = SecretsConfig
  { matcher :: Text
  , list    :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON SecretsConfig

