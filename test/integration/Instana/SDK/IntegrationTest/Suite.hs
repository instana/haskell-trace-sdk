module Instana.SDK.IntegrationTest.Suite
  ( ExternalAppSuites
  , Suite(..)
  , SuiteGenerator(..)
  , SuiteGeneratorInternal
  , SuiteOpts(..)
  , defaultOpts
  , suiteOpts
  , withCustomAgentName
  , withPidTranslation
  ) where


import           Test.HUnit

import           Instana.SDK.SDK (InstanaContext)


data Suite =
  Suite
    { label :: String
    , tests :: String -> [IO Test]
    }


data SuiteOpts =
  SuiteOpts
    { suiteLabel        :: String
    , usePidTranslation :: Bool
    , customAgentName   :: Maybe String
    }


defaultOpts :: String -> SuiteOpts
defaultOpts suiteLabel_ =
  SuiteOpts
    { suiteLabel        = suiteLabel_
    , usePidTranslation = False
    , customAgentName   = Nothing
    }


withPidTranslation :: String -> SuiteOpts
withPidTranslation suiteLabel_ =
  (defaultOpts suiteLabel_) { usePidTranslation = True }


withCustomAgentName :: String -> String -> SuiteOpts
withCustomAgentName suiteLabel_ agentName =
  (defaultOpts suiteLabel_) { customAgentName = Just agentName }


-- |Describes a test suite that connects the integration test process to the
-- agent stub and creates spans directly from the integration test process.
type SuiteGeneratorInternal = (InstanaContext -> [Suite], SuiteOpts)


-- |Describes a test suite that starts an external app which then conn the integration test process to the
type ExternalAppSuites = ([Suite], SuiteOpts)


data SuiteGenerator =
    Internal SuiteGeneratorInternal
  | External ExternalAppSuites


suiteOpts :: SuiteGenerator -> SuiteOpts
suiteOpts gen =
  case gen of
    Internal g ->
      let
        (_, opts) = g
      in
      opts
    External g ->
      let
        (_, opts) = g
      in
      opts

