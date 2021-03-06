module Instana.SDK.IntegrationTest.HUnitExtra
  ( applyLabel
  , assertAllIO
  , failIO
  , mergeCounts
  , passIO
  , skip
  ) where


import qualified Data.List                           as List
import           System.Log.Logger                   (infoM)
import           Test.HUnit

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


mergeCounts :: [Counts] -> Counts
mergeCounts =
  List.foldl
    addCounts
    (Counts { cases = 0, tried = 0, errors = 0, failures = 0 })


addCounts :: Counts -> Counts -> Counts
addCounts c1 c2 =
  Counts
    { cases = cases c1 + cases c2
    , tried = tried c1 + tried c2
    , errors = errors c1 + errors c2
    , failures = failures c1 + failures c2
    }



