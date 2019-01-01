module Instana.SDK.IntegrationTest.TestSuites (allSuites) where


import qualified Instana.SDK.IntegrationTest.BracketApi  as BracketApi
import qualified Instana.SDK.IntegrationTest.Connection  as Connection
import qualified Instana.SDK.IntegrationTest.HttpTracing as HttpTracing
import qualified Instana.SDK.IntegrationTest.LowLevelApi as LowLevelApi
import qualified Instana.SDK.IntegrationTest.Metrics     as Metrics
import           Instana.SDK.IntegrationTest.Suite       (ConditionalSuite (..),
                                                          Suite (..))
import qualified Instana.SDK.IntegrationTest.Suite       as Suite


allSuites :: [ConditionalSuite]
allSuites =
  [ testBracketApi
  , testLowLevelApi
  , testConnectionEstablishment
  , testConnectionLoss
  , testAgentRestart
  , testPidTranslation
  , testCustomAgentName
  , testHttpTracing
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
         , BracketApi.shouldMergeData pid
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
         , LowLevelApi.shouldMergeData pid
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


testCustomAgentName :: ConditionalSuite
testCustomAgentName =
  Run $
    Suite
      { Suite.label = "Custom Agent Name"
      , Suite.tests = (\pid -> [
          Connection.shouldUseCustomAgentName pid
        ])
      , Suite.options = Suite.withCustomAgentName "Devil in Disguise"
      }


testHttpTracing :: ConditionalSuite
testHttpTracing =
  Run $
    Suite
      { Suite.label = "HTTP Tracing"
      , Suite.tests = (\pid -> [
          HttpTracing.shouldCreateRootEntryWithBracketApi pid
        , HttpTracing.shouldCreateNonRootEntryWithBracketApi pid
        , HttpTracing.shouldSuppressWithBracketApi
        , HttpTracing.shouldCreateRootEntryWithLowLevelApi pid
        , HttpTracing.shouldCreateNonRootEntryWithLowLevelApi pid
        , HttpTracing.shouldSuppressWithLowLevelApi
        ])
      , Suite.options = Suite.defaultOptions
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

