{-# LANGUAGE DeriveGeneric #-}


{-| This internal module is not supposed to be used by client code. It's API can
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


agentHostKey :: String
agentHostKey = "INSTANA_AGENT_HOST"


agentPortKey :: String
agentPortKey = "INSTANA_AGENT_PORT"


agentNameKey :: String
agentNameKey = "INSTANA_AGENT_NAME"


forceTransmissionAfterKey :: String
forceTransmissionAfterKey = "INSTANA_FORCE_TRANSMISSION_STARTING_AFTER"


forceTransmissionStartingAtKey :: String
forceTransmissionStartingAtKey = "INSTANA_FORCE_TRANSMISSION_STARTING_AT"


maxBufferedSpansKey :: String
maxBufferedSpansKey = "INSTANA_MAX_BUFFERED_SPANS"


defaultAgentHost :: String
defaultAgentHost = "127.0.0.1"


defaultAgentPort :: Int
defaultAgentPort = 42699


defaultAgentName :: String
defaultAgentName = "Instana Agent"


defaultForceTransmissionAfter :: Int
defaultForceTransmissionAfter = 1000


defaultForceTransmissionStartingAt :: Int
defaultForceTransmissionStartingAt = 500

defaultMaxBufferedSpans :: Int
defaultMaxBufferedSpans = 1000


data FinalConfig = FinalConfig
  { agentHost                   :: String
  , agentPort                   :: Int
  , agentName                   :: String
  , forceTransmissionAfter      :: Int
  , forceTransmissionStartingAt :: Int
  , maxBufferedSpans            :: Int
  } deriving (Eq, Generic, Show)


mkFinalConfig ::
  String
  -> Int
  -> String
  -> Int
  -> Int
  -> Int
  -> FinalConfig
mkFinalConfig
  agentHost_
  agentPort_
  agentName_
  forceTransmissionAfter_
  forceTransmissionStartingAt_
  maxBufferedSpans_ =
  FinalConfig
    { agentHost = agentHost_
    , agentPort = agentPort_
    , agentName = agentName_
    , forceTransmissionAfter = forceTransmissionAfter_
    , forceTransmissionStartingAt = forceTransmissionStartingAt_
    , maxBufferedSpans = maxBufferedSpans_
    }


readConfigFromEnvironment :: IO Config
readConfigFromEnvironment = do
  agentHostEnv <- lookupEnv agentHostKey
  agentPortEnv <- lookupEnv agentPortKey
  agentNameEnv <- lookupEnv agentNameKey
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
      , Config.agentName = agentNameEnv
      , Config.forceTransmissionAfter = forceTransmissionAfterParsed
      , Config.forceTransmissionStartingAt = forceTransmissionStartingAtParsed
      , Config.maxBufferedSpans = maxBufferedSpansParsed
      }


readConfigFromEnvironmentAndApplyDefaults :: IO FinalConfig
readConfigFromEnvironmentAndApplyDefaults = do
  configFromEnv <- readConfigFromEnvironment
  return $ applyDefaults configFromEnv


applyDefaults :: Config -> FinalConfig
applyDefaults config =
  FinalConfig
   { agentHost =
       fromMaybe defaultAgentHost (Config.agentHost config)
   , agentPort =
       fromMaybe defaultAgentPort (Config.agentPort config)
   , agentName =
       fromMaybe defaultAgentName (Config.agentName config)
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
      , Config.agentName =
          (Config.agentName userConfig) <|>
          (Config.agentName configFromEnv)
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
