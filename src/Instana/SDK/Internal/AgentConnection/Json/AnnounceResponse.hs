{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
Description : Aeson type for the agent's announce response
-}
module Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
    ( AnnounceResponse(..)
    , TracingConfig(..)
    ) where


import           Data.Aeson                   (FromJSON)
import qualified Data.Aeson                   as Aeson
import           Data.Aeson.Casing            as AesonCasing
import           Data.Text                    (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Secrets (SecretsMatcher)


-- |Holds the agent's response to the announce request.
data AnnounceResponse = AnnounceResponse
  { pid          :: Int
  , agentUuid    :: Text
  , tracing      :: Maybe TracingConfig
  , extraHeaders :: Maybe [String]
  , secrets      :: SecretsMatcher
  } deriving (Eq, Show, Generic)

instance FromJSON AnnounceResponse


data TracingConfig = TracingConfig
  { extraHttpHeaders :: Maybe [String]
  } deriving (Eq, Show, Generic)

instance FromJSON TracingConfig where
   parseJSON = Aeson.genericParseJSON $
     AesonCasing.aesonDrop 0 AesonCasing.trainCase
