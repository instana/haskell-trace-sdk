module Instana.SDK.IntegrationTest.TestSuites (allSuites) where


import qualified Instana.SDK.IntegrationTest.BracketApi    as BracketApi
import qualified Instana.SDK.IntegrationTest.Connection    as Connection
import qualified Instana.SDK.IntegrationTest.HttpTracing   as HttpTracing
import qualified Instana.SDK.IntegrationTest.LowLevelApi   as LowLevelApi
import qualified Instana.SDK.IntegrationTest.Metrics       as Metrics
import qualified Instana.SDK.IntegrationTest.ServiceName   as ServiceName
import           Instana.SDK.IntegrationTest.Suite         (ConditionalSuite (..),
                                                            Suite (..))
import qualified Instana.SDK.IntegrationTest.Suite         as Suite
import qualified Instana.SDK.IntegrationTest.WaiMiddleware as WaiMiddleware


allSuites :: [ConditionalSuite]
allSuites =
  [ testBracketApi
  , testLowLevelApi
  , testConnectionEstablishment
  , testConnectionLoss
  , testAgentRestart
  , testPidTranslation
  , testServiceName
  , testHttpTracing
  , testWaiMiddleware
  , testMetrics
  ]


testBracketApi :: ConditionalSuite
testBracketApi =
  Run $
    Suite
      { Suite.label = "Bracket API"
      , Suite.tests = BracketApi.allTests
      , Suite.options = Suite.defaultOptions
      }


testLowLevelApi :: ConditionalSuite
testLowLevelApi =
  Run $
    Suite
      { Suite.label = "Low Level API"
      , Suite.tests = LowLevelApi.allTests
      , Suite.options = Suite.defaultOptions
      }


testConnectionEstablishment :: ConditionalSuite
testConnectionEstablishment =
  Run $
    Suite
      { Suite.label = "Initial Connection Establishment"
      , Suite.tests = (\pid -> [
          Connection.shouldRetryInitialConnectionEstablishment pid
        ])
      , Suite.options = Suite.withStartupDelay
      }


testConnectionLoss :: ConditionalSuite
testConnectionLoss =
  Run $
    Suite
      { Suite.label = "Connection Loss"
      , Suite.tests = (\pid -> [
          Connection.shouldReestablishLostConnection pid
        ])
      , Suite.options = Suite.withConnectionLoss
      }


testAgentRestart :: ConditionalSuite
testAgentRestart =
  Run $
    Suite
      { Suite.label = "Agent Restart"
      , Suite.tests = (\pid -> [
          Connection.shouldReconnectAfterAgentRestart pid
        ])
      , Suite.options = Suite.defaultOptions
      }


testPidTranslation :: ConditionalSuite
testPidTranslation =
  Run $
    Suite
      { Suite.label =  "PID translation"
      , Suite.tests = (\pid -> [
          Connection.shouldUseTranslatedPid pid
        ])
      , Suite.options = Suite.withPidTranslation
      }


testServiceName :: ConditionalSuite
testServiceName =
  Run $
    Suite
      { Suite.label = "INSTANA_SERVICE_NAME env var"
      , Suite.tests = ServiceName.allTests
      , Suite.options = Suite.withCustomServiceName "Custom Service Name"
      }


testHttpTracing :: ConditionalSuite
testHttpTracing =
  Run $
    Suite
      { Suite.label = "HTTP Tracing"
      , Suite.tests = HttpTracing.allTests
      , Suite.options = Suite.defaultOptions {
            Suite.appsUnderTest =
              [ Suite.testServer
              , Suite.downstreamTarget
              ]
          }
      }


testWaiMiddleware :: ConditionalSuite
testWaiMiddleware =
  Run $
    Suite
      { Suite.label = "WAI Middleware"
      , Suite.tests = WaiMiddleware.allTests
      , Suite.options = Suite.defaultOptions {
            Suite.appsUnderTest =
              [ Suite.testServerWithMiddleware
              , Suite.downstreamTarget
              ]
          }
      }


testMetrics :: ConditionalSuite
testMetrics =
  Run $
    Suite
      { Suite.label =  "Metrics"
      , Suite.tests = (\pid -> [
          Metrics.shouldReportMetrics pid
        ])
      , Suite.options = Suite.withPidTranslation
      }

