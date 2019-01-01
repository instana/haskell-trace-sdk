{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.Runner (runSuites) where


import qualified Data.ByteString.Lazy.Char8             as LBSC8
import           Data.List                              as List
import qualified Data.Maybe                             as Maybe
import qualified Network.HTTP.Client                    as HTTP
import           System.Exit                            as Exit
import           System.Process                         as Process
import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.HUnitExtra (mergeCounts)
import           Instana.SDK.IntegrationTest.Suite      (ConditionalSuite (..),
                                                         Suite)
import qualified Instana.SDK.IntegrationTest.Suite      as Suite
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import           Instana.SDK.IntegrationTest.Util       (putStrFlush)


{-| Runs a collection of test suites.
-}
runSuites :: [ConditionalSuite] -> IO Counts
runSuites allSuites = do
  let
    exlusiveSuites =
      List.filter Suite.isExclusive allSuites
    (actions, skippedDueToExlusive) =
      if not (null exlusiveSuites) then
        ( List.map runConditionalSuite exlusiveSuites
        , length allSuites - length exlusiveSuites
        )
      else
        (List.map runConditionalSuite allSuites, 0)
  if not (null exlusiveSuites) then
    putStrLn $
      "\n\nRunning only " ++
      show (List.length exlusiveSuites) ++
      " suite(s) marked as exclusive, ignoring all others."
  else
    putStrLn $
      "\n\nRunning " ++ show (List.length allSuites) ++ " test suite(s)."
  results <- sequence actions
  let
    mergedResults = mergeCounts results
    caseCount = cases mergedResults + skippedDueToExlusive
    triedCount = tried mergedResults
    errCount = errors mergedResults
    failCount = failures mergedResults
  putStrLn $
    "SUMMARY: Cases: " ++ show caseCount ++
    "  Tried: " ++ show triedCount ++
    "  Errors: " ++ show errCount ++
    "  Failures: " ++ show failCount
  if errCount > 0 && failCount > 0 then
    Exit.die "😱 😭 There have been errors and failures! 😱 😭"
  else if errCount > 0 then
    Exit.die "😱 There have been errors! 😱"
  else if failCount > 0 then
    Exit.die "😭 There have been test failures. 😭"
  else putStrLn "🎉 All tests have passed. 🎉"
  return mergedResults


{-| Runs the suite unless it is skipped.
-}
runConditionalSuite :: ConditionalSuite -> IO Counts
runConditionalSuite conditionalSuite = do
  case conditionalSuite of
    Run suite       ->
      runSuite suite
    Exclusive suite ->
      runSuite suite
    Skip _          ->
      return $ Counts 1 0 0 0


{-| Starts the app under test and the agent stub, then runs the test suite.
-}
runSuite :: Suite -> IO Counts
runSuite suite = do
  let
    suiteLabel = Suite.label suite
  putStrFlush $ "\nExecuting test suite: " ++ suiteLabel ++ "\n\n"

  let
    options = Suite.options suite
    customAgentName = Suite.customAgentName options
    agentStubCommand =
      (if Suite.usePidTranslation options
        then "SIMULATE_PID_TRANSLATION=true "
        else ""
      ) ++
      (if Maybe.isJust customAgentName
        then
          let
            Just agentName = customAgentName
          in
          "AGENT_NAME=\"" ++ agentName ++ "\" "
        else ""
      ) ++
      (if Suite.startupDelay options
        then "STARTUP_DELAY=2500 "
        else ""
      ) ++
      (if Suite.simulateConnectionLoss options
        then "SIMULATE_CONNECTION_LOSS=true "
        else ""
      )
      ++ "stack exec instana-haskell-agent-stub"

    appCommand =
      (if Maybe.isJust customAgentName
        then
          let
            Just agentName = customAgentName
          in
           "INSTANA_AGENT_NAME=\"" ++ agentName ++ "\" "
        else ""
      ) ++
      "INSTANA_LOG_LEVEL=INFO " ++
      "stack exec instana-haskell-test-wai-server"

  putStrLn $ "Running: " ++ agentStubCommand
  Process.withCreateProcess
    (Process.shell agentStubCommand)
    (\_ _ _ _ -> do
      putStrLn $ "Running: " ++ appCommand
      Process.withCreateProcess
        (Process.shell appCommand)
        (\_ _ _ _ -> runTests suite)
    )


runTests :: Suite -> IO Counts
runTests suite = do
  putStrFlush "⏱  waiting for agent stub to come up"
  _ <- HttpHelper.retryRequestRecovering TestHelper.pingAgentStub
  putStrLn "\n✅ agent stub is up"
  putStrFlush "⏱  waiting for app to come up"
  appPingResponse <- HttpHelper.retryRequestRecovering TestHelper.pingApp
  let
    appPingBody = HTTP.responseBody appPingResponse
    appPid = (read (LBSC8.unpack appPingBody) :: Int)
  putStrLn $ "\n✅ app is up, PID is " ++ (show appPid)
  results <-
    waitForAgentConnectionAndRun suite appPid
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


-- |Waits for the app under test to establish a connection to the agent, then
-- runs the tests of the given suite.
waitForAgentConnectionAndRun :: Suite -> Int -> IO Counts
waitForAgentConnectionAndRun suite appPid = do
  let
    options = Suite.options suite
  discoveries <-
    TestHelper.waitForExternalAgentConnection
      (Suite.usePidTranslation options)
      appPid
  case discoveries of
    Left message ->
      assertFailure $
        "Could not start test suites " ++ (Suite.label suite) ++
        ". The agent connection could not be established: " ++ message
    Right (_, pid) ->
      runTestSuite pid suite


runTestSuite :: String -> Suite -> IO Counts
runTestSuite pid suite = do
  integrationTestsIO <- wrapSuite pid suite
  runTestTT integrationTestsIO


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

