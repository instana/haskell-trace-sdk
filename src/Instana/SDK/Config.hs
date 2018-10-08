{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Config
Description : Provides configuration records that can be used to control the initialization of the SDK
-}
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
  { -- | IP or host name of the Instana agent
    agentHost                   :: Maybe String
    -- | Port of the Instana agent
  , agentPort                   :: Maybe Int
    -- | When establishing a connection to the Instana agent, the SDK validates
    -- the Instana agent's `Server` HTTP response header. Should you have
    -- changed the Server name on the agent side, you can use parameter to
    -- provide the name to match that header against.
  , agentName                   :: Maybe String
    -- | Spans are usually buffered before being transmitted to the agent. This
    -- setting forces the transmission of all buffered spans after the given
    -- amount of milliseconds. Default: 1000.
  , forceTransmissionAfter      :: Maybe Int
    -- | This setting forces the transmission of all buffered spans when the
    -- given number of spans has been buffered.
  , forceTransmissionStartingAt :: Maybe Int
    -- | Limits the number of spans to buffer. When the limit is reached, spans
    -- will be dropped. This setting is a safe guard against memory leaks from
    -- buffering excessive amounts of spans. It must be larger than
    -- forceTransmissionStartingAt.
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

