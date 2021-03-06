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
      , Suite.tests = (\pid ->
         [ BracketApi.shouldRecordSpans pid
         , BracketApi.shouldRecordNonRootEntry pid
         , BracketApi.shouldMergeTags pid
         , BracketApi.shouldSetServiceName pid
         ])
      , Suite.options = Suite.defaultOptions
      }


testLowLevelApi :: ConditionalSuite
testLowLevelApi =
  Run $
    Suite
      { Suite.label = "Low Level API"
      , Suite.tests = (\pid ->
         [ LowLevelApi.shouldRecordSpans pid
         , LowLevelApi.shouldRecordNonRootEntry pid
         , LowLevelApi.shouldMergeTags pid
         ])
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
      , Suite.tests = (\pid ->
         [ ServiceName.shouldUseServiceNameEnvVar pid
         ])
      , Suite.options = Suite.withCustomServiceName "Custom Service Name"
      }


testHttpTracing :: ConditionalSuite
testHttpTracing =
  Run $
    Suite
      { Suite.label = "HTTP Tracing"
      , Suite.tests = (\pid -> [
          HttpTracing.shouldCreateRootEntryWithBracketApi pid
        , HttpTracing.shouldCreateNonRootEntryWithBracketApi pid
        , HttpTracing.shouldAddWebsiteMonitoringCorrelationWithBracketApi pid
        , HttpTracing.shouldSuppressWithBracketApi
        , HttpTracing.shouldCreateRootEntryWithLowLevelApi pid
        , HttpTracing.shouldAddWebsiteMonitoringCorrelationWithLowLevelApi pid
        , HttpTracing.shouldCreateNonRootEntryWithLowLevelApi pid
        , HttpTracing.shouldSuppressWithLowLevelApi
        ])
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
      , Suite.tests = (\pid -> [
          WaiMiddleware.shouldCreateRootEntry pid
        , WaiMiddleware.shouldCreateNonRootEntry pid
        , WaiMiddleware.shouldAddWebsiteMonitoringCorrelation pid
        , WaiMiddleware.shouldSuppress
        ])
      , Suite.options =
          Suite.defaultOptions {
            Suite.appsUnderTest = [Suite.testServerWithMiddleware]
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

