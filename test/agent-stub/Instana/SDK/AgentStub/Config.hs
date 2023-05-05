{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.Config
  ( AgentStubConfig(..)
  , readConfig
  ) where


import           Data.Maybe                              (fromMaybe)
import           Data.String                             (fromString)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           GHC.Generics
import           Instana.SDK.AgentStub.DiscoveryResponse (SecretsConfig (SecretsConfig))
import qualified Instana.SDK.AgentStub.DiscoveryResponse as DiscoveryResponse
import qualified Network.Wai.Handler.Warp                as Warp
import           System.Environment                      (lookupEnv)
import           Text.Read                               (readMaybe)



data AgentStubConfig = AgentStubConfig
  { bindHost                     :: Warp.HostPreference
  , bindPort                     :: Int
  , secretsConfig                :: SecretsConfig
  , extraHeadersViaTracingConfig :: [Text]
  , extraHeadersViaLegacyConfig  :: [Text]
  , startupDelay                 :: Int
  , simulateConnectionLoss       :: Bool
  , simulatPidTranslation        :: Bool
  } deriving (Eq, Show, Generic)


defaultSecretsConfig :: SecretsConfig
defaultSecretsConfig =
  SecretsConfig
    { DiscoveryResponse.matcher = "contains-ignore-case"
    , DiscoveryResponse.list = ["key", "pass", "secret"]
    }


readConfig :: IO AgentStubConfig
readConfig = do
  -- Since the stub is only used locally, binding to host "127.0.0.1" is safer
  -- and also more convenient than binding to something like "*4"
  -- (see https://hackage.haskell.org/package/warp-3.2.9/docs/Network-Wai-Handler-Warp.html#t:HostPreference),
  -- as it prevents the MacOS firewall from asking if it is okay to accept
  -- incoming connections every time the app is recompiled and restarted.
  hostString          <- lookupEnvWithDefault    "HOST" "127.0.0.1"
  port                <- lookupEnvIntWithDefault "PORT" 1302
  secrets             <- parseSecretsConfig
  extraHeadersTracing <- parseExtraHeadersConfig "EXTRA_HEADERS_VIA_TRACING_CONFIG"
  extraHeadersLegacy  <- parseExtraHeadersConfig "EXTRA_HEADERS_VIA_LEGACY_CONFIG"
  delay               <- lookupEnvIntWithDefault "STARTUP_DELAY" 0
  connectionLoss      <- lookupFlag              "SIMULATE_CONNECTION_LOSS"
  pidTranslation      <- lookupFlag              "SIMULATE_PID_TRANSLATION"
  let
    hostPreference :: Warp.HostPreference
    hostPreference = fromString hostString
  return
    AgentStubConfig
      { bindHost                     = hostPreference
      , bindPort                     = port
      , secretsConfig                = secrets
      , extraHeadersViaTracingConfig = extraHeadersTracing
      , extraHeadersViaLegacyConfig  = extraHeadersLegacy
      , startupDelay                 = delay
      , simulateConnectionLoss       = connectionLoss
      , simulatPidTranslation        = pidTranslation
      }


lookupEnvWithDefault :: String -> String -> IO String
lookupEnvWithDefault key defaultValue = do
  maybeValue <- lookupEnv key
  return $ fromMaybe defaultValue maybeValue


lookupEnvIntWithDefault :: String -> Int -> IO Int
lookupEnvIntWithDefault key defaultValue = do
  maybeValue <- lookupEnv key
  case maybeValue of
    Just string -> do
      let
        parsed :: Maybe Int
        parsed = readMaybe string
      case parsed of
        Just integer ->
          return integer
        Nothing -> do
          _ <- error
            ("Error: You provided the string \"" ++ string ++
             "\" as the value for configuration option " ++ key ++
             " but this option requires an integer value and I could" ++
             " not convert \"" ++ string ++ "\" to an integer.")
          return defaultValue
    Nothing ->
      return defaultValue


lookupFlag :: String -> IO Bool
lookupFlag key = do
  maybeValue <- lookupEnv key
  return $ case maybeValue of
             Just _ -> True
             _      -> False


parseSecretsConfig :: IO SecretsConfig
parseSecretsConfig = do
  maybeSecretsConfigString <- lookupEnv "SECRETS_CONFIG"
  case maybeSecretsConfigString of
    Just secretsString -> do
      let
        matcherAndList = T.splitOn ":" (T.pack secretsString)
        matcher = head matcherAndList
        listAsString = head $ tail matcherAndList
        list = T.splitOn "," listAsString
      return SecretsConfig
        { DiscoveryResponse.matcher = matcher
        , DiscoveryResponse.list = list
        }
    Nothing -> do
      return defaultSecretsConfig


parseExtraHeadersConfig :: String -> IO [Text]
parseExtraHeadersConfig key = do
  maybeExtraHeadersConfigString <- lookupEnv key
  case maybeExtraHeadersConfigString of
    Just extraHeadersString -> do
      return $ T.splitOn "," (T.pack extraHeadersString)
    Nothing -> do
      return []

