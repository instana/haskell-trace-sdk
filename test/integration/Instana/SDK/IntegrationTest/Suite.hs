module Instana.SDK.IntegrationTest.Suite
  ( ConditionalSuite(..)
  , Suite(..)
  , SuiteOptions(..)
  , defaultOptions
  , isExclusive
  , withConnectionLoss
  , withCustomAgentName
  , withPidTranslation
  , withStartupDelay
  ) where


import           Test.HUnit


-- |Describes a collection of cohesive tests that share the same suite options.
data Suite =
  Suite
    { label   :: String
    , tests   :: String -> [IO Test]
    , options :: SuiteOptions
    }


-- |Describes options for running a test suite.
data SuiteOptions =
  SuiteOptions
    { usePidTranslation      :: Bool
    , customAgentName        :: Maybe String
    , startupDelay           :: Bool
    , simulateConnectionLoss :: Bool
    , appUnderTest           :: String
    }


defaultOptions :: SuiteOptions
defaultOptions =
  SuiteOptions
    { usePidTranslation      = False
    , customAgentName        = Nothing
    , startupDelay           = False
    , simulateConnectionLoss = False
    , appUnderTest           = "instana-haskell-test-wai-server"
    }


withPidTranslation :: SuiteOptions
withPidTranslation =
  defaultOptions { usePidTranslation = True }


withCustomAgentName :: String -> SuiteOptions
withCustomAgentName agentName =
  defaultOptions { customAgentName = Just agentName }


withStartupDelay :: SuiteOptions
withStartupDelay =
  defaultOptions { startupDelay = True }


withConnectionLoss :: SuiteOptions
withConnectionLoss =
  defaultOptions { simulateConnectionLoss = True }


data ConditionalSuite =
    Run Suite
  | Exclusive Suite
  | Skip Suite


isExclusive :: ConditionalSuite -> Bool
isExclusive conditionalSuite =
  case conditionalSuite of
    Exclusive _ -> True
    _           -> False

