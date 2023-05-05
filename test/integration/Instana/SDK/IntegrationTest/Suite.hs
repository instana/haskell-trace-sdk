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
  , withCustomSecretsConfig
  , withCustomServiceName
  , withLegacyConfigForExtraHeaders
  , withPidTranslation
  , withStartupDelay
  , withTracingConfigForExtraHeaders
  , withW3cTraceCorrelationDisabled
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
    { usePidTranslation            :: Bool
    , startupDelay                 :: Bool
    , simulateConnectionLoss       :: Bool
    , customServiceName            :: Maybe String
    , customSecretsConfig          :: Maybe String
    , tracingConfigForExtraHeaders :: [String]
    , legacyConfigForExtraHeaders  :: [String]
    , disableW3cTraceCorrelation   :: Bool
    , appsUnderTest                :: [AppUnderTest]
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
    { usePidTranslation            = False
    , startupDelay                 = False
    , simulateConnectionLoss       = False
    , customServiceName            = Nothing
    , customSecretsConfig          = Nothing
    , tracingConfigForExtraHeaders = []
    , legacyConfigForExtraHeaders  = []
    , disableW3cTraceCorrelation   = False
    , appsUnderTest                = [testServer]
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


withCustomSecretsConfig :: String -> SuiteOptions
withCustomSecretsConfig secretsConfig =
  defaultOptions { customSecretsConfig = Just secretsConfig }


withTracingConfigForExtraHeaders :: SuiteOptions
withTracingConfigForExtraHeaders =
  defaultOptions {
    tracingConfigForExtraHeaders =
      [ "X-Request-Header-On-Entry"
      , "X-Response-Header-On-Entry"
      , "X-Request-Header-On-Exit"
      , "X-Response-Header-On-Exit"
      ]
  }


withLegacyConfigForExtraHeaders :: SuiteOptions
withLegacyConfigForExtraHeaders =
  defaultOptions {
    legacyConfigForExtraHeaders =
      [ "X-Request-Header-On-Entry"
      , "X-Response-Header-On-Entry"
      , "X-Request-Header-On-Exit"
      , "X-Response-Header-On-Exit"
      ]
  }


withW3cTraceCorrelationDisabled :: SuiteOptions
withW3cTraceCorrelationDisabled =
  defaultOptions { disableW3cTraceCorrelation = True }


data ConditionalSuite =
    Run Suite
  | Exclusive Suite
  | Skip Suite


isExclusive :: ConditionalSuite -> Bool
isExclusive conditionalSuite =
  case conditionalSuite of
    Exclusive _ -> True
    _           -> False

