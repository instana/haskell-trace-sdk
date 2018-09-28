{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
Description : Aeson type for the agent's announce response
-}
module Instana.SDK.Internal.AgentConnection.Json.AnnounceResponse
    ( AnnounceResponse(..)
    ) where


import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)
import           GHC.Generics


data AnnounceResponse = AnnounceResponse
    { pid       :: Int
    , agentUuid :: Text
    } deriving (Eq, Show, Generic)


instance FromJSON AnnounceResponse

