{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.Config
Description : Internal representation of the configuration.

This internal module is not supposed to be used by client code. It's API can
change between releases, no guarantee for any kind of compatibility between
releases exists for this module.
-}
module Instana.SDK.Internal.Config
  ( FinalConfig(..)
  , mergeConfigs
  , mkFinalConfig
  , readConfigFromEnvironment
  , readConfigFromEnvironmentAndApplyDefaults
  ) where


import           Control.Applicative ((<|>))
import           Data.Maybe          (fromMaybe)
import           GHC.Generics
import           System.Environment  (lookupEnv)
import           Text.Read           (readMaybe)

import           Instana.SDK.Config  (Config)
import qualified Instana.SDK.Config  as Config


-- |Environment variable for the agent host
agentHostKey :: String
agentHostKey = "INSTANA_AGENT_HOST"


-- |Environment variable for the agent port
agentPortKey :: String
agentPortKey = "INSTANA_AGENT_PORT"


-- |Environment variable for the service name override.
serviceNameKey :: String
serviceNameKey = "INSTANA_SERVICE_NAME"


-- |Environment variable for the force-transmision-afeter setting
forceTransmissionAfterKey :: String
forceTransmissionAfterKey = "INSTANA_FORCE_TRANSMISSION_STARTING_AFTER"


-- |Environment variable for the force-transmision-at setting
forceTransmissionStartingAtKey :: String
forceTransmissionStartingAtKey = "INSTANA_FORCE_TRANSMISSION_STARTING_AT"


-- |Environment variable for the max-buffered-spans setting
maxBufferedSpansKey :: String
maxBufferedSpansKey = "INSTANA_MAX_BUFFERED_SPANS"


-- |Default agent host/IP
defaultAgentHost :: String
defaultAgentHost = "127.0.0.1"


-- |Default agent port
defaultAgentPort :: Int
defaultAgentPort = 42699


-- |Default force-transmission-after setting
defaultForceTransmissionAfter :: Int
defaultForceTransmissionAfter = 1000


-- |Default force-transmission-at setting
defaultForceTransmissionStartingAt :: Int
defaultForceTransmissionStartingAt = 500


-- |Default max-buffered-spans setting
defaultMaxBufferedSpans :: Int
defaultMaxBufferedSpans = 1000


-- |The config after evaluating and merging user provided config, environment
-- variables and default values.
data FinalConfig = FinalConfig
  { agentHost                   :: String
  , agentPort                   :: Int
  , serviceName                 :: Maybe String
  , forceTransmissionAfter      :: Int
  , forceTransmissionStartingAt :: Int
  , maxBufferedSpans            :: Int
  } deriving (Eq, Generic, Show)


-- |Creates the FinalConfig.
mkFinalConfig ::
  String
  -> Int
  -> Maybe String
  -> Int
  -> Int
  -> Int
  -> FinalConfig
mkFinalConfig
  agentHost_
  agentPort_
  serviceName_
  forceTransmissionAfter_
  forceTransmissionStartingAt_
  maxBufferedSpans_ =
  FinalConfig
    { agentHost = agentHost_
    , agentPort = agentPort_
    , serviceName = serviceName_
    , forceTransmissionAfter = forceTransmissionAfter_
    , forceTransmissionStartingAt = forceTransmissionStartingAt_
    , maxBufferedSpans = maxBufferedSpans_
    }


-- |Reads all provided config related environment variables.
readConfigFromEnvironment :: IO Config
readConfigFromEnvironment = do
  agentHostEnv <- lookupEnv agentHostKey
  agentPortEnv <- lookupEnv agentPortKey
  serviceNameEnv <- lookupEnv serviceNameKey
  forceTransmissionAfterEnv <- lookupEnv forceTransmissionAfterKey
  forceTransmissionStartingAtEnv <- lookupEnv forceTransmissionStartingAtKey
  maxBufferedSpansEnv <- lookupEnv maxBufferedSpansKey
  let
    -- parse numeric config values via readMaybe to Int if they were set,
    -- otherwise they remain Nothing
    agentPortParsed = agentPortEnv >>= readMaybe
    forceTransmissionAfterParsed =
      forceTransmissionAfterEnv >>= readMaybe
    forceTransmissionStartingAtParsed =
      forceTransmissionStartingAtEnv >>= readMaybe
    maxBufferedSpansParsed = maxBufferedSpansEnv >>= readMaybe
  return $
    Config.defaultConfig
      { Config.agentHost = agentHostEnv
      , Config.agentPort = agentPortParsed
      , Config.serviceName = serviceNameEnv
      , Config.forceTransmissionAfter = forceTransmissionAfterParsed
      , Config.forceTransmissionStartingAt = forceTransmissionStartingAtParsed
      , Config.maxBufferedSpans = maxBufferedSpansParsed
      }


-- |Reads all provided config related environment variables and applies default
-- values for absent settings.
readConfigFromEnvironmentAndApplyDefaults :: IO FinalConfig
readConfigFromEnvironmentAndApplyDefaults = do
  configFromEnv <- readConfigFromEnvironment
  return $ applyDefaults configFromEnv


-- |Merges the user provided config with default values.
applyDefaults :: Config -> FinalConfig
applyDefaults config =
  FinalConfig
   { agentHost =
       fromMaybe defaultAgentHost (Config.agentHost config)
   , agentPort =
       fromMaybe defaultAgentPort (Config.agentPort config)
   , serviceName = Config.serviceName config
   , forceTransmissionAfter =
       fromMaybe
         defaultForceTransmissionAfter
         (Config.forceTransmissionAfter config)
   , forceTransmissionStartingAt =
       fromMaybe
         defaultForceTransmissionStartingAt
         (Config.forceTransmissionStartingAt config)
   , maxBufferedSpans =
       fromMaybe
         defaultMaxBufferedSpans
         (Config.maxBufferedSpans config)
   }


-- |Merges two configs into a FinalConfig.
mergeConfigs :: Config -> Config -> FinalConfig
mergeConfigs userConfig configFromEnv =
  let
    merged = userConfig
      { Config.agentHost =
          (Config.agentHost userConfig) <|>
          (Config.agentHost configFromEnv)
      , Config.agentPort =
          (Config.agentPort userConfig) <|>
          (Config.agentPort configFromEnv)
      , Config.serviceName =
          (Config.serviceName userConfig) <|>
          (Config.serviceName configFromEnv)
      , Config.forceTransmissionAfter =
          (Config.forceTransmissionAfter userConfig) <|>
          (Config.forceTransmissionAfter configFromEnv)
      , Config.forceTransmissionStartingAt =
          (Config.forceTransmissionStartingAt userConfig) <|>
          (Config.forceTransmissionStartingAt configFromEnv)
      , Config.maxBufferedSpans =
          (Config.maxBufferedSpans userConfig) <|>
          (Config.maxBufferedSpans configFromEnv)
      }
  in
    applyDefaults merged
