module Instana.SDK.IntegrationTest.Suite
  ( ConditionalSuite(..)
  , Suite(..)
  , SuiteOptions(..)
  , defaultOptions
  , isExclusive
  , withConnectionLoss
  , withCustomServiceName
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
    , startupDelay           :: Bool
    , simulateConnectionLoss :: Bool
    , appUnderTest           :: String
    , customServiceName      :: Maybe String
    }


defaultOptions :: SuiteOptions
defaultOptions =
  SuiteOptions
    { usePidTranslation      = False
    , startupDelay           = False
    , simulateConnectionLoss = False
    , appUnderTest           = "instana-haskell-test-wai-server"
    , customServiceName      = Nothing
    }


withPidTranslation :: SuiteOptions
withPidTranslation =
  defaultOptions { usePidTranslation = True }


withStartupDelay :: SuiteOptions
withStartupDelay =
  defaultOptions { startupDelay = True }


withConnectionLoss :: SuiteOptions
withConnectionLoss =
  defaultOptions { simulateConnectionLoss = True }


withCustomServiceName :: String -> SuiteOptions
withCustomServiceName serviceName =
  defaultOptions { customServiceName = Just serviceName }


data ConditionalSuite =
    Run Suite
  | Exclusive Suite
  | Skip Suite


isExclusive :: ConditionalSuite -> Bool
isExclusive conditionalSuite =
  case conditionalSuite of
    Exclusive _ -> True
    _           -> False

