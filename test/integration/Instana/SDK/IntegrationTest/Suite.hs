module Instana.SDK.IntegrationTest.Suite
  ( ExternalAppSuites
  , Suite(..)
  , SuiteGenerator(..)
  , SuiteGeneratorInternal
  , SuiteOpts(..)
  , defaultOpts
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
    { usePidTranslation :: Bool
    , customAgentName   :: Maybe String
    }


defaultOpts :: SuiteOpts
defaultOpts =
  SuiteOpts
    { usePidTranslation = False
    , customAgentName   = Nothing
    }


withPidTranslation :: SuiteOpts
withPidTranslation =
  defaultOpts { usePidTranslation = True }


withCustomAgentName :: String -> SuiteOpts
withCustomAgentName name =
  defaultOpts { customAgentName = Just name }


-- |Describes a test suite that connects the integration test process to the
-- agent stub and creates spans directly from the integration test process.
type SuiteGeneratorInternal = (InstanaContext -> [Suite], SuiteOpts)


-- |Describes a test suite that starts an external app which then conn the integration test process to the
type ExternalAppSuites = ([Suite], SuiteOpts)


data SuiteGenerator =
    Internal SuiteGeneratorInternal
  | External ExternalAppSuites

