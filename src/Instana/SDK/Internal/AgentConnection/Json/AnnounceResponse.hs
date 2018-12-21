{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
Description : Aeson type for the agent's announce response
-}
module Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
    ( AnnounceResponse(..)
    ) where


import           Data.Aeson                   (FromJSON)
import           Data.Text                    (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Secrets (SecretsMatcher)


-- |Holds the agent's response to the announce request.
data AnnounceResponse = AnnounceResponse
  { pid          :: Int
  , agentUuid    :: Text
  , extraHeaders :: Maybe [Text]
  , secrets      :: SecretsMatcher
  } deriving (Eq, Show, Generic)

instance FromJSON AnnounceResponse

