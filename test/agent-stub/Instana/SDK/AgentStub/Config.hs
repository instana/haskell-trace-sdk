{-# LANGUAGE DeriveGeneric #-}

module Instana.SDK.AgentStub.Config
  ( AgentStubConfig(..)
  , readConfig
  ) where


import           Data.Maybe               (fromMaybe)
import           Data.String              (fromString)
import           GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)
import           Text.Read                (readMaybe)


data AgentStubConfig = AgentStubConfig
  { bindHost               :: Warp.HostPreference
  , bindPort               :: Int
  , agentName              :: String
  , startupDelay           :: Int
  , simulateConnectionLoss :: Bool
  , simulatPidTranslation  :: Bool
  } deriving (Eq, Show, Generic)


readConfig :: IO AgentStubConfig
readConfig = do
  -- Since the stub is only used locally, binding to host "127.0.0.1" is safer
  -- and also more convenient than binding to something like "*4"
  -- (see https://hackage.haskell.org/package/warp-3.2.9/docs/Network-Wai-Handler-Warp.html#t:HostPreference),
  -- as it prevents the MacOS firewall from asking if it is okay to accept
  -- incoming connections every time the app is recompiled and restarted.
  hostString     <- lookupEnvWithDefault    "HOST" "127.0.0.1"
  port           <- lookupEnvIntWithDefault "PORT" 1302
  name           <- lookupEnvWithDefault    "AGENT_NAME" "Instana Agent"
  delay          <- lookupEnvIntWithDefault "STARTUP_DELAY" 0
  connectionLoss <- lookupFlag              "SIMULATE_CONNECTION_LOSS"
  pidTranslation <- lookupFlag              "SIMULATE_PID_TRANSLATION"
  let
    hostPreference :: Warp.HostPreference
    hostPreference = fromString hostString
  return
    AgentStubConfig
      { bindHost               = hostPreference
      , bindPort               = port
      , agentName              = name
      , startupDelay           = delay
      , simulateConnectionLoss = connectionLoss
      , simulatPidTranslation  = pidTranslation
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

