module Main where


import qualified Instana.SDK.Internal.ConfigTest  as ConfigTest
import qualified Instana.SDK.Internal.IdTest      as IdTest
import qualified Instana.SDK.Internal.LoggingTest as LoggingTest

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
    ]

