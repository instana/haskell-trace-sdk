{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.Config
  -- Maintenance note: accessor functions need to be reexported in SDK.hs
  ( Config
  , agentHost
  , agentPort
  , agentName
  , defaultConfig
  , forceTransmissionAfter
  , forceTransmissionStartingAt
  , maxBufferedSpans
  ) where


import           GHC.Generics


{-| Configuration for a the Instana SDK. Please use the 'defaultConfig'
function and then modify individual settings via record syntax For more
information, see <http://www.yesodweb.com/book/settings-types>.
-}
data Config = Config
  { agentHost                   :: Maybe String
  , agentPort                   :: Maybe Int
  , agentName                   :: Maybe String
  , forceTransmissionAfter      :: Maybe Int
  , forceTransmissionStartingAt :: Maybe Int
  , maxBufferedSpans            :: Maybe Int
  } deriving (Eq, Generic)


{-| Populates all config values as Nothing, so that the Instana SDK relies on
environment variables or on its default config values (in this order)
internally.
-}
defaultConfig :: Config
defaultConfig =
  Config
    { agentHost = Nothing
    , agentPort = Nothing
    , agentName = Nothing
    , forceTransmissionAfter = Nothing
    , forceTransmissionStartingAt = Nothing
    , maxBufferedSpans = Nothing
    }

