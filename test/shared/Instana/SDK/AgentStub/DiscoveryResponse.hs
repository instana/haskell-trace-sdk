{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.AgentStub.DiscoveryResponse where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data DiscoveryResponse =
  DiscoveryResponse
   { pid          :: Int
   , agentUuid    :: Text
   , extraHeaders :: Maybe [Text]
   , secrets      :: SecretsConfig
   } deriving (Eq, Show, Generic)

instance ToJSON DiscoveryResponse


data SecretsConfig = SecretsConfig
  { matcher :: Text
  , list    :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON SecretsConfig

