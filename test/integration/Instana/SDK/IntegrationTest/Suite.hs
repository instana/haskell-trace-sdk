module Instana.SDK.IntegrationTest.Suite
  ( AppUnderTest(..)
  , ConditionalSuite(..)
  , Suite(..)
  , SuiteOptions(..)
  , defaultOptions
  , downstreamTarget
  , isExclusive
  , testServer
  , testServerWithMiddleware
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
    , customServiceName      :: Maybe String
    , appsUnderTest          :: [AppUnderTest]
    }


-- |Describes options for running a test suite.
data AppUnderTest =
  AppUnderTest
    { executable      :: String
    , port            :: Int
    , connectsToAgent :: Bool
    }


defaultOptions :: SuiteOptions
defaultOptions =
  SuiteOptions
    { usePidTranslation      = False
    , startupDelay           = False
    , simulateConnectionLoss = False
    , appsUnderTest          = [testServer]
    , customServiceName      = Nothing
    }


testServer :: AppUnderTest
testServer =
  AppUnderTest
    { executable      = "instana-haskell-test-wai-server"
    , port            = 1207
    , connectsToAgent = True
    }


testServerWithMiddleware :: AppUnderTest
testServerWithMiddleware =
  AppUnderTest
    { executable      = "instana-haskell-test-wai-with-middleware-server"
    , port            = 1207
    , connectsToAgent = True
    }


downstreamTarget :: AppUnderTest
downstreamTarget =
  AppUnderTest
    { executable      = "downstream-target"
    , port            = 1208
    , connectsToAgent = True
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

