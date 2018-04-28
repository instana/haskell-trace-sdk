module Instana.SDK.Internal.ConfigTest (allTests) where


import           Data.Maybe                  (isNothing)
import           System.Environment          (setEnv, unsetEnv)
import           Test.HUnit

import           Instana.SDK.Config          (Config)
import qualified Instana.SDK.Config          as Config
import qualified Instana.SDK.Internal.Config as InternalConfig


allTests :: Test
allTests =
  TestList
    [ TestLabel "mergeShouldApplyDefaults" mergeShouldApplyDefaults
    , TestLabel "mergeShouldPreferFirstArg" mergeShouldPreferFirstArg
    , TestLabel
      "shouldReadNonExistingEnvironmentVariablesAsNothing"
      shouldReadNonExistingEnvironmentVariablesAsNothing
    , TestLabel
      "shouldReadExistingEnvironmentVariables"
      shouldReadExistingEnvironmentVariables
    , TestLabel
      "shouldReadNonNumericPortAsNothing"
      shouldReadNonNumericPortAsNothing
    , TestLabel
      "shouldReadNonExistingEnvVarsAndApplyDefaults"
      shouldReadNonExistingEnvVarsAndApplyDefaults
    ]


mergeShouldApplyDefaults :: Test
mergeShouldApplyDefaults =
  let
    merged = InternalConfig.mergeConfigs emptyConfig emptyConfig
  in
    TestList $
      [ TestCase $
          assertEqual
            "agent host" "127.0.0.1" (InternalConfig.agentHost merged)
      , TestCase $
          assertEqual "agent port" 42699 (InternalConfig.agentPort merged)
      , TestCase $
          assertEqual
            "agent name"
            "Instana Agent"
            (InternalConfig.agentName merged)
      ]


mergeShouldPreferFirstArg :: Test
mergeShouldPreferFirstArg =
  let
    first =
      Config.defaultConfig
        { Config.agentHost = Just "horst"
        , Config.agentPort = Just 12345
        , Config.agentName = Just "Hans"
        }
    second =
      Config.defaultConfig
        { Config.agentHost = Just "wurst"
        , Config.agentPort = Just 98765
        , Config.agentName = Just "Franz"
        }
    merged = InternalConfig.mergeConfigs first second
  in
    TestList $
      [ TestCase $
          assertEqual
            "agent host" "horst" (InternalConfig.agentHost merged)
      , TestCase $
          assertEqual "agent port" 12345 (InternalConfig.agentPort merged)
      , TestCase $
          assertEqual
            "agent name"
            "Hans"
            (InternalConfig.agentName merged)
      ]


shouldReadNonExistingEnvironmentVariablesAsNothing :: Test
shouldReadNonExistingEnvironmentVariablesAsNothing =
  TestCase $
    do
      conf <- InternalConfig.readConfigFromEnvironment
      assertBool "agent host" (isNothing $ Config.agentHost conf)
      assertBool "agent port" (isNothing $ Config.agentPort conf)
      assertBool "agent name" (isNothing $ Config.agentName conf)


shouldReadExistingEnvironmentVariables :: Test
shouldReadExistingEnvironmentVariables =
  TestCase $
    do
      setEnv "INSTANA_AGENT_HOST" "agenthost.com"
      setEnv "INSTANA_AGENT_PORT" "12345"
      setEnv "INSTANA_AGENT_NAME" "Say my name, say my name"
      conf <- InternalConfig.readConfigFromEnvironment
      assertEqual "agent host" (Just "agenthost.com") (Config.agentHost conf)
      assertEqual "agent port" (Just 12345) (Config.agentPort conf)
      assertEqual
        "agent name"
        (Just "Say my name, say my name")
        (Config.agentName conf)
      unsetEnv  "INSTANA_AGENT_HOST"
      unsetEnv "INSTANA_AGENT_PORT"
      unsetEnv "INSTANA_AGENT_NAME"


shouldReadNonNumericPortAsNothing :: Test
shouldReadNonNumericPortAsNothing =
  TestCase $
    do
      setEnv "INSTANA_AGENT_PORT" "12x45"
      conf <- InternalConfig.readConfigFromEnvironment
      assertBool "agent port" $ isNothing $ Config.agentPort conf
      unsetEnv "INSTANA_AGENT_PORT"


shouldReadNonExistingEnvVarsAndApplyDefaults :: Test
shouldReadNonExistingEnvVarsAndApplyDefaults =
  TestCase $
    do
      conf <- InternalConfig.readConfigFromEnvironmentAndApplyDefaults
      assertEqual "agent host" "127.0.0.1" (InternalConfig.agentHost conf)
      assertEqual "agent port" 42699 (InternalConfig.agentPort conf)
      assertEqual
        "agent name"
        "Instana Agent"
        (InternalConfig.agentName conf)


emptyConfig :: Config
emptyConfig =
  Config.defaultConfig

