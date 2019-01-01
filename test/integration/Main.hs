module Main where


import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.Runner     as TestRunner
import qualified Instana.SDK.IntegrationTest.TestSuites as TestSuites


main :: IO Counts
main =
  TestRunner.runSuites TestSuites.allSuites

