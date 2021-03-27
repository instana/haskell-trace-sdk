module Instana.SDK.IntegrationTest.HUnitExtra
  ( SuiteResult
  , applyLabel
  , assertAllIO
  , failIO
  , mergeTestResults
  , passIO
  , skip
  ) where


import qualified Data.List                           as List
import           System.Log.Logger                   (infoM)
import           Test.HUnit                          (Assertion, Counts (..),
                                                      Test (..), assertBool,
                                                      assertFailure)

import           Instana.SDK.IntegrationTest.Logging (testLogger)


failIO :: String -> IO Test
failIO message =
  return $ TestCase $ assertFailure message


passIO :: IO Test
passIO =
  return $ TestCase $ assertBool "" True


assertAllIO :: [Assertion] -> IO Test
assertAllIO assertions =
  return $ TestList $ List.map TestCase assertions


applyLabel :: String -> IO Test -> IO Test
applyLabel label testIO = do
  t <- testIO
  return $ TestLabel label t


skip :: String -> IO Test -> IO Test
skip label _ = do
  infoM testLogger $ "Skipped: " ++ label
  return $ TestLabel label $ TestList []


type SuiteResult = (Counts, [String])


mergeTestResults :: [SuiteResult] -> SuiteResult
mergeTestResults allResults =
  let
    (counts_, fullReport) =
      List.foldl
        merge
        ( (Counts
            { cases = 0
            , tried = 0
            , errors = 0
            , failures = 0
            }
          )
        , []
        )
        allResults
  in
  (counts_, fullReport)


merge :: SuiteResult -> SuiteResult -> SuiteResult
merge (counts1, report1) (counts2, report2) =
  ( Counts
      { cases = cases counts1 + cases counts2
      , tried = tried counts1 + tried counts2
      , errors = errors counts1 + errors counts2
      , failures = failures counts1 + failures counts2
      }
  , report1 ++ report2
  )



