module Main where


import qualified Instana.SDK.Internal.AgentConnection.DefaultGatewayIpTest as DefaultGatewayIpTest
import qualified Instana.SDK.Internal.AgentConnection.SchedFileTest        as SchedFileTest
import qualified Instana.SDK.Internal.ConfigTest                           as ConfigTest
import qualified Instana.SDK.Internal.IdTest                               as IdTest
import qualified Instana.SDK.Internal.LoggingTest                          as LoggingTest
import qualified Instana.SDK.Internal.Metrics.CompressionTest              as MetricsCompressionTest
import qualified Instana.SDK.Internal.Metrics.DeltasTest                   as MetricsDeltasTest
import qualified Instana.SDK.Internal.SecretsTest                          as SecretsTest
import qualified Instana.SDK.Internal.ServerTimingTest                     as ServerTimingTest
import qualified Instana.SDK.Internal.SpanStackTest                        as SpanStackTest
import qualified Instana.SDK.Internal.W3CTraceContextTest                  as W3CTraceContextTest
import qualified Instana.SDK.SpanDataTest                                  as SpanDataTest
import qualified Instana.SDK.SpanTest                                      as SpanTest
import qualified Instana.SDK.TracingHeadersTest                            as TracingHeadersTest

import           Test.HUnit


main :: IO Counts
main = do
  runTestTT allTests


allTests :: Test
allTests =
  TestList
    [ ConfigTest.allTests
    , DefaultGatewayIpTest.allTests
    , IdTest.allTests
    , LoggingTest.allTests
    , MetricsCompressionTest.allTests
    , MetricsDeltasTest.allTests
    , SchedFileTest.allTests
    , SecretsTest.allTests
    , ServerTimingTest.allTests
    , SpanDataTest.allTests
    , SpanStackTest.allTests
    , SpanTest.allTests
    , TracingHeadersTest.allTests
    , W3CTraceContextTest.allTests
    ]

