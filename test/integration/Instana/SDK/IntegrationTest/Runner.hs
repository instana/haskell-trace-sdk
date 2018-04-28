{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.Runner (runTestsIgnoringHandles) where


import           Data.List                              as List
import           GHC.IO.Handle                          (Handle)
import           System.Process                         as Process
import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.Suite      (Suite, SuiteGenerator)
import qualified Instana.SDK.IntegrationTest.Suite      as Suite
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import           Instana.SDK.IntegrationTest.Util       (putStrFlush)
import           Instana.SDK.SDK                        (InstanaContext)
import qualified Instana.SDK.SDK                        as InstanaSDK


{-| Solely exists to consume the handles yielded by withCreateProcess, which we
do not need.
-}
runTestsIgnoringHandles ::
  SuiteGenerator
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> ProcessHandle
  -> IO Counts
runTestsIgnoringHandles suiteGenerator _ _ _ _ =
  runTests suiteGenerator


runTests :: SuiteGenerator -> IO Counts
runTests suiteGenerator = do
  putStrFlush "⏱k waiting for agent stub to come up"
  _ <- HttpHelper.retryRequestRecovering TestHelper.pingAgentStub
  putStrLn "\n✅ agent stub is up"
  let
    (_, opts) = suiteGenerator
    config =
      InstanaSDK.defaultConfig
        { InstanaSDK.agentHost = Just HttpHelper.agentStubHost
        , InstanaSDK.agentPort = Just HttpHelper.agentStubPort
        , InstanaSDK.agentName = Suite.customAgentName opts
        }
  InstanaSDK.withConfiguredInstana config $
    waitForAgentConnectionAndRun suiteGenerator


waitForAgentConnectionAndRun :: SuiteGenerator -> InstanaContext -> IO Counts
waitForAgentConnectionAndRun suiteGenerator instana = do
  let
    (generatorFn, opts) = suiteGenerator
    suites = generatorFn instana
  discoveries <-
    TestHelper.waitForAgentConnection (Suite.usePidTranslation opts)
  case discoveries of
    Left message -> do
      let
        suiteLabels = List.map Suite.label suites
        allLabels = List.intercalate ", " suiteLabels
      assertFailure $
        "Could not start test suites [" ++ allLabels ++ "]. The agent " ++
        "connection could not be established: " ++ message
    Right _ ->
      runTestSuites suites


runTestSuites :: [Suite] -> IO Counts
runTestSuites suites = do
  let
    allIntegrationTestsIO = List.map applyLabelToSuite suites
  allIntegrationTests <- sequence allIntegrationTestsIO
  runTestTT $ TestLabel "Span Recording" $ TestList allIntegrationTests


applyLabelToSuite :: Suite -> IO Test
applyLabelToSuite suite = do
  let
    label   = Suite.label suite
    testsIO = Suite.tests suite
  tests <- sequence testsIO
  return $ TestLabel label $ TestList tests

