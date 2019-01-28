module Main where


import qualified Instana.SDK.Internal.AgentConnection.SchedFileTest as SchedFileTest
import qualified Instana.SDK.Internal.ConfigTest                    as ConfigTest
import qualified Instana.SDK.Internal.IdTest                        as IdTest
import qualified Instana.SDK.Internal.LoggingTest                   as LoggingTest
import qualified Instana.SDK.Internal.Metrics.CompressionTest       as MetricsCompressionTest
import qualified Instana.SDK.Internal.Metrics.DeltasTest            as MetricsDeltasTest
import qualified Instana.SDK.Internal.SecretsTest                   as SecretsTest
import qualified Instana.SDK.Internal.SpanStackTest                 as SpanStackTest
import qualified Instana.SDK.Internal.SpanTest                      as SpanTest

import           Test.HUnit


main :: IO Counts
main = do
  runTestTT allTests


allTests :: Test
allTests =
  TestList
    [ ConfigTest.allTests
    , IdTest.allTests
    , LoggingTest.allTests
    , MetricsCompressionTest.allTests
    , MetricsDeltasTest.allTests
    , SecretsTest.allTests
    , SpanTest.allTests
    , SpanStackTest.allTests
    , SchedFileTest.allTests
    ]

