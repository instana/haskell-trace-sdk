module Instana.SDK.IntegrationTest.TestSuites (allSuites) where


import qualified Data.Aeson                                   as Aeson
import qualified Instana.SDK.IntegrationTest.BracketApi       as BracketApi
import qualified Instana.SDK.IntegrationTest.Connection       as Connection
import qualified Instana.SDK.IntegrationTest.CustomSecrets    as CustomSecrets
import qualified Instana.SDK.IntegrationTest.ExtraHttpHeaders as ExtraHttpHeaders
import qualified Instana.SDK.IntegrationTest.HttpTracing      as HttpTracing
import qualified Instana.SDK.IntegrationTest.LowLevelApi      as LowLevelApi
import qualified Instana.SDK.IntegrationTest.Metrics          as Metrics
import qualified Instana.SDK.IntegrationTest.ServiceName      as ServiceName
import qualified Instana.SDK.IntegrationTest.SpecCompliance   as SpecCompliance
import           Instana.SDK.IntegrationTest.Suite            (ConditionalSuite (..),
                                                               Suite (..))
import qualified Instana.SDK.IntegrationTest.Suite            as Suite
import qualified Instana.SDK.IntegrationTest.WaiMiddleware    as WaiMiddleware


allSuites :: Aeson.Array -> [ConditionalSuite]
allSuites specificationComplianceTestCases =
  [ testBracketApi
  , testLowLevelApi
  , testConnectionEstablishment
  , testConnectionLoss
  , testAgentRestart
  , testPidTranslation
  , testServiceName
  , testHttpTracing
  , testCustomSecrets
  , testExtraHttpHeaders
  , testExtraHttpHeadersLegacyConfig
  , testWaiMiddleware
  , testSpecComplianceW3cOn
      specificationComplianceTestCases
      "Low Level API"
      "http/low/level/api"
  , testSpecComplianceW3cOff
      specificationComplianceTestCases
      "Low Level API"
      "http/low/level/api"
  , testSpecComplianceW3cOn
      specificationComplianceTestCases
      "Bracket API"
      "http/bracket/api"
  , testSpecComplianceW3cOff
      specificationComplianceTestCases
      "Bracket API"
      "http/bracket/api"
  , testSpecComplianceW3cOnWaiMiddleware specificationComplianceTestCases "api"
  , testSpecComplianceW3cOffWaiMiddleware specificationComplianceTestCases "api"
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


testCustomSecrets :: ConditionalSuite
testCustomSecrets =
  Run $
    Suite
      { Suite.label = "Custom Secrets"
      , Suite.tests = CustomSecrets.allTests
      , Suite.options = (Suite.withCustomSecretsConfig "regex:.*obscured.*,hidden.*") {
            Suite.appsUnderTest =
              [ Suite.testServer
              , Suite.downstreamTarget
              ]
          }
      }


testExtraHttpHeaders :: ConditionalSuite
testExtraHttpHeaders =
  Run $
    Suite
      { Suite.label = "Extra HTTP Headers"
      , Suite.tests = (\pid -> [
          ExtraHttpHeaders.shouldApplyExtraHeadersFromCommonTracerConfig pid
        ])
      , Suite.options = Suite.withTracingConfigForExtraHeaders {
            Suite.appsUnderTest =
              [ Suite.testServer
              , Suite.downstreamTarget
              ]
          }
      }


testExtraHttpHeadersLegacyConfig :: ConditionalSuite
testExtraHttpHeadersLegacyConfig =
  Run $
    Suite
      { Suite.label = "Extra HTTP Headers (legacy config)"
      , Suite.tests = (\pid -> [
          ExtraHttpHeaders.shouldApplyExtraHeadersFromLegacyConfig pid
        ])
      , Suite.options = Suite.withLegacyConfigForExtraHeaders {
            Suite.appsUnderTest =
              [ Suite.testServer
              , Suite.downstreamTarget
              ]
          }
      }


testSpecComplianceW3cOn :: Aeson.Array -> String -> String -> ConditionalSuite
testSpecComplianceW3cOn specificationComplianceTestCases apiLabel route =
  Run $
    Suite
      { Suite.label =
          "Specification Compliance (WC3 Trace Correlation On/" ++
          apiLabel ++ ")"
      , Suite.tests =
          SpecCompliance.allTestsW3cCorrelationOn
            specificationComplianceTestCases
            Suite.testServer
            route
      , Suite.options = (Suite.withCustomSecretsConfig "contains-ignore-case:password,secret,token") {
            Suite.appsUnderTest =
              [ Suite.testServer
              , Suite.downstreamTarget
              ]
          }
      }


testSpecComplianceW3cOff :: Aeson.Array -> String -> String -> ConditionalSuite
testSpecComplianceW3cOff specificationComplianceTestCases apiLabel route =
  Run $
    Suite
      { Suite.label =
          "Specification Compliance (WC3 Trace Correlation Off/" ++
          apiLabel ++ ")"
      , Suite.tests =
          SpecCompliance.allTestsW3cCorrelationOff
            specificationComplianceTestCases
            Suite.testServer
            route
      , Suite.options = Suite.defaultOptions {
            Suite.customSecretsConfig = Just "contains-ignore-case:password,secret,token"
          , Suite.disableW3cTraceCorrelation = True
          , Suite.appsUnderTest =
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


testSpecComplianceW3cOnWaiMiddleware ::
  Aeson.Array
  -> String
  -> ConditionalSuite
testSpecComplianceW3cOnWaiMiddleware specificationComplianceTestCases route =
  Run $
    Suite
      { Suite.label =
          "Specification Compliance (WC3 Trace Correlation On/WAI Middleware)"
      , Suite.tests =
          SpecCompliance.allTestsW3cCorrelationOn
            specificationComplianceTestCases
            Suite.testServerWithMiddleware
            route
      , Suite.options = (Suite.withCustomSecretsConfig "contains-ignore-case:password,secret,token") {
            Suite.appsUnderTest =
              [ Suite.testServerWithMiddleware
              , Suite.downstreamTarget
              ]
          }
      }


testSpecComplianceW3cOffWaiMiddleware ::
  Aeson.Array
  -> String
  -> ConditionalSuite
testSpecComplianceW3cOffWaiMiddleware specificationComplianceTestCases route =
  Run $
    Suite
      { Suite.label =
          "Specification Compliance (WC3 Trace Correlation Off/WAI Middleware)"
      , Suite.tests =
          SpecCompliance.allTestsW3cCorrelationOff
            specificationComplianceTestCases
            Suite.testServerWithMiddleware
            route
      , Suite.options = Suite.defaultOptions {
            Suite.customSecretsConfig = Just "contains-ignore-case:password,secret,token"
          , Suite.disableW3cTraceCorrelation = True
          , Suite.appsUnderTest =
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

