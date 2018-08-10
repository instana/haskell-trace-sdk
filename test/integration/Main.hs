module Main where


import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.TestSuites as TestSuites


main :: IO Counts
main =
  TestSuites.allTests

