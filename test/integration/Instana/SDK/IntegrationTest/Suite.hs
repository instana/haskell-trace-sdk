module Instana.SDK.IntegrationTest.Suite
  ( Suite(..)
  , SuiteOpts(..)
  , SuiteGenerator
  , defaultOpts
  , withPidTranslation
  , withCustomAgentName
  ) where


import           Test.HUnit

import           Instana.SDK.SDK (InstanaContext)


data Suite =
  Suite
    { label :: String
    , tests :: [IO Test]
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


type SuiteGenerator = (InstanaContext -> [Suite], SuiteOpts)

