module Main where


import           Data.Aeson                             (Array)
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Lazy                   as LBS
import qualified System.Directory                       as Directory
import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.Logging    as Logging
import qualified Instana.SDK.IntegrationTest.Runner     as TestRunner
import qualified Instana.SDK.IntegrationTest.TestSuites as TestSuites
import qualified System.Exit                            as Exit


main :: IO Counts
main = do
  Logging.initLogging
  specificationComplianceTestCases <- parseSpecificationTestCasesJson
  case specificationComplianceTestCases of
    Just specTestCases ->
      TestRunner.runSuites $ TestSuites.allSuites specTestCases
    Nothing ->
      Exit.die $
        "ERROR: Could not parse specification test cases JSON file. " ++
          "See above for details."


specificationComplianceJsonFile :: FilePath
specificationComplianceJsonFile =
  "test/integration/Instana/SDK/IntegrationTest/" ++
    "tracer_compliance_test_cases.json"


parseSpecificationTestCasesJson :: IO (Maybe Array)
parseSpecificationTestCasesJson = do
  cwd <- Directory.getCurrentDirectory
  putStrLn $ "Running integration tests in directory " ++ show cwd
  putStrLn $
    "Parsing " ++ cwd ++ "/" ++ specificationComplianceJsonFile ++ "..."
  content <- LBS.readFile specificationComplianceJsonFile
  let
    decodedEither = Aeson.eitherDecode' content :: Either String Array
  case decodedEither of
    Left errorMessage -> do
      putStrLn $
        "Could not parse " ++ specificationComplianceJsonFile ++
        " - is it valid JSON?"
      putStrLn errorMessage
      return Nothing
    Right result ->
      return $ Just result

