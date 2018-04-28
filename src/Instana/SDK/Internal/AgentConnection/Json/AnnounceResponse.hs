{-# LANGUAGE DeriveGeneric #-}
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

