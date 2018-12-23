{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.Runner (runTestsIgnoringHandles) where


import qualified Data.ByteString.Lazy.Char8             as LBSC8
import           Data.List                              as List
import           GHC.IO.Handle                          (Handle)
import qualified Network.HTTP.Client                    as HTTP
import           System.Process                         as Process
import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.Suite      (ExternalAppSuites,
                                                         Suite,
                                                         SuiteGenerator (..),
                                                         SuiteGeneratorInternal)
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
  let
    suiteLabel = Suite.suiteLabel $ Suite.suiteOpts suiteGenerator
  putStrFlush $ "\nRunning Test Suite: " ++ suiteLabel ++ "\n"
  putStrFlush "⏱  waiting for agent stub to come up"
  _ <- HttpHelper.retryRequestRecovering TestHelper.pingAgentStub
  putStrLn "\n✅ agent stub is up"
  case suiteGenerator of
    Internal suiteGeneratorInternal ->
      runInternalTests suiteGeneratorInternal
    External externalAppSuites ->
      runExternalTestsWithExternalApp externalAppSuites


runInternalTests :: SuiteGeneratorInternal -> IO Counts
runInternalTests suiteGeneratorInternal = do
  let
    (_, opts) = suiteGeneratorInternal
    config =
      InstanaSDK.defaultConfig
        { InstanaSDK.agentHost = Just HttpHelper.agentStubHost
        , InstanaSDK.agentPort = Just HttpHelper.agentStubPort
        , InstanaSDK.agentName = Suite.customAgentName opts
        }
  results <- InstanaSDK.withConfiguredInstana config $
    waitForInternalAgentConnectionAndRun suiteGeneratorInternal
  -- The withProcess call that starts the agent stub should also terminate it
  -- when the test suite is done or when an error occurs while running the test
  -- suite. On MacOS, this works. On Linux, the agent stub does not get
  -- terminated, for the following reasons: The agent stub is started via
  -- "/bin/sh -c \"stack exec ...\"" and Process.createWith will send a TERM
  -- signal to terminate the started process. On Linux, this results only in the
  -- "bin/sh" process to be terminated, but the "stack exec" not. Thus, the
  -- first started agent stub instance would never be shut down. To make sure
  -- the agent stub instance gets shut down, we send an extra HTTP request to
  -- ask the agent stub to terminate itself.
  _ <- TestHelper.shutDownAgentStub
  return results


runExternalTestsWithExternalApp :: ExternalAppSuites -> IO Counts
runExternalTestsWithExternalApp externalAppSuites = do
  putStrFlush "⏱  waiting for app to come up"
  appPingResponse <- HttpHelper.retryRequestRecovering TestHelper.pingApp
  let
    appPingBody = HTTP.responseBody appPingResponse
    appPid = LBSC8.unpack appPingBody
  putStrLn $ "\n✅ app is up, PID: " ++ appPid
  results <-
    waitForExternalAgentConnectionAndRun externalAppSuites appPid
  -- The withProcess calls that starts the agent stub and the external app
  -- should also terminate them when the test suite is done or when an error
  -- occurs while running the test suite. On MacOS, this works. On Linux, these
  -- processes do not get terminated, for the following reasons: They are
  -- started via
  -- "/bin/sh -c \"stack exec ...\"" and Process.createWith will send a TERM
  -- signal to terminate the started process. On Linux, this results only in the
  -- "bin/sh" process to be terminated, but the "stack exec" not. Thus, the
  -- first started agent stub/app instance would never be shut down. To make
  -- sure the process instances get shut down, we send an extra HTTP request to
  -- ask the processes to terminate themselves.
  _ <- TestHelper.shutDownAgentStub
  _ <- TestHelper.shutDownApp
  return results


-- |Waits for an the integration test process to establish a connection to the
-- agent, then runs the tests.
waitForInternalAgentConnectionAndRun ::
  SuiteGeneratorInternal
  -> InstanaContext
  -> IO Counts
waitForInternalAgentConnectionAndRun suiteGeneratorInternal instana = do
  let
    (generatorFn, opts) = suiteGeneratorInternal
    suites = generatorFn instana
  discoveries <-
    TestHelper.waitForInternalAgentConnection (Suite.usePidTranslation opts)
  case discoveries of
    Left message -> do
      let
        suiteLabels = List.map Suite.label suites
        allLabels = List.intercalate ", " suiteLabels
      assertFailure $
        "Could not start test suites [" ++ allLabels ++ "]. The agent " ++
        "connection could not be established: " ++ message
    Right (_, pid) ->
      runTestSuites pid suites


-- |Waits for an external app to establish a connection to the agent, then runs
-- the tests.
waitForExternalAgentConnectionAndRun :: ExternalAppSuites -> String -> IO Counts
waitForExternalAgentConnectionAndRun externalAppSuites appPid = do
  let
    (suites, _) = externalAppSuites
  discoveries <-
    TestHelper.waitForExternalAgentConnection appPid
  case discoveries of
    Left message -> do
      let
        suiteLabels = List.map Suite.label suites
        allLabels = List.intercalate ", " suiteLabels
      assertFailure $
        "Could not start test suites [" ++ allLabels ++ "]. The agent " ++
        "connection could not be established: " ++ message
    Right (_, pid) ->
      runTestSuites pid suites


runTestSuites :: String -> [Suite] -> IO Counts
runTestSuites pid suites = do
  let
    allIntegrationTestsIO = List.map (wrapSuite pid) suites
  allIntegrationTests <- sequence allIntegrationTestsIO
  runTestTT $ TestList allIntegrationTests


wrapSuite :: String -> Suite -> IO Test
wrapSuite pid suite = do
  let
    label   = Suite.label suite
    testsIO = (Suite.tests suite) pid
    -- Reset the agent stub's recordeds spans after each test, but do not reset
    -- the discoveries, as all tests of one suite share the connection
    -- establishment process.
    testsWithReset =
      List.map
        (\testIO ->
          testIO >>=
            (\testResult -> TestHelper.resetSpans >> return testResult)
        )
        testsIO
  -- sequence all tests into one action
  tests <- sequence testsWithReset
  return $ TestLabel label $ TestList tests

