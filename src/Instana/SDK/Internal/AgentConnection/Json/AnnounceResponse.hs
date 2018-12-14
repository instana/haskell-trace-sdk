{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
Description : Aeson type for the agent's announce response
-}
module Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
    ( AnnounceResponse(..)
    , SecretsConfig(..)
    ) where


import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)
import           GHC.Generics


-- |Holds the agent's response to the announce request.
data AnnounceResponse = AnnounceResponse
  { pid          :: Int
  , agentUuid    :: Text
  , extraHeaders :: Maybe [Text]
  , secrets      :: SecretsConfig
  } deriving (Eq, Show, Generic)

instance FromJSON AnnounceResponse


data SecretsConfig = SecretsConfig
  { matcher :: Text
  , list    :: [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON SecretsConfig

