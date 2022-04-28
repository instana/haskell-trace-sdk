{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.Runner (runSuites) where


import qualified Data.ByteString.Lazy.Char8             as LBSC8
import           Data.List                              as List
import qualified Data.Maybe                             as Maybe
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as HTTP
import           System.Environment                     (lookupEnv)
import           System.Exit                            as Exit
import           System.Log.Logger                      (infoM)
import           System.Process                         as Process
import           Test.HUnit                             (Counts (Counts),
                                                         Test (TestLabel, TestList),
                                                         assertFailure)
import qualified Test.HUnit                             as HUnit
import qualified Test.HUnit.Text                        as HUnitText

import           Instana.SDK.IntegrationTest.HUnitExtra (SuiteResult,
                                                         mergeTestResults)
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.Logging    (testLogger)
import           Instana.SDK.IntegrationTest.Suite      (AppUnderTest,
                                                         ConditionalSuite (..),
                                                         Suite)
import qualified Instana.SDK.IntegrationTest.Suite      as Suite
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper


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
    infoM testLogger  $
      "Running only " ++
      show (List.length exlusiveSuites) ++
      " suite(s) marked as exclusive, ignoring all others."
  else
    infoM testLogger $
      "Running " ++ show (List.length allSuites) ++ " test suite(s)."
  results <- sequence actions
  let
    (mergedCounts, mergedReport) = mergeTestResults results
    caseCount = HUnit.cases mergedCounts + skippedDueToExlusive
    triedCount = HUnit.tried mergedCounts
    errCount = HUnit.errors mergedCounts
    failCount = HUnit.failures mergedCounts
  if errCount > 0 && failCount > 0 then do
    logReport mergedReport
    logSummary caseCount triedCount errCount failCount
    Exit.die "😱 😭 There have been errors and failures! 😱 😭"
  else if errCount > 0 then do
    logReport mergedReport
    logSummary caseCount triedCount errCount failCount
    Exit.die "😱 There have been errors! 😱"
  else if failCount > 0 then do
    logReport mergedReport
    logSummary caseCount triedCount errCount failCount
    Exit.die "😭 There have been test failures. 😭"
  else do
    logSummary caseCount triedCount errCount failCount
    infoM testLogger "🎉 All tests have passed. 🎉"
  return mergedCounts


logReport :: [String] -> IO ()
logReport report = do
  _ <- sequence $
    map
      (\reportLine -> infoM testLogger reportLine)
      ("COLLECTED FAILURES:" : report)
  return ()


logSummary :: Int -> Int -> Int -> Int -> IO ()
logSummary caseCount triedCount errCount failCount =
 infoM testLogger $
    "SUMMARY: Cases: " ++ show caseCount ++
    "  Tried: " ++ show triedCount ++
    "  Errors: " ++ show errCount ++
    "  Failures: " ++ show failCount


{-| Runs the suite unless it is skipped.
-}
runConditionalSuite :: ConditionalSuite -> IO SuiteResult
runConditionalSuite conditionalSuite = do
  case conditionalSuite of
    Run suite       ->
      runSuite suite
    Exclusive suite ->
      runSuite suite
    Skip _          ->
      return $ (Counts 1 0 0 0, [])


{-| Starts the app under test and the agent stub, then runs the test suite.
-}
runSuite :: Suite -> IO SuiteResult
runSuite suite = do
  let
    suiteLabel = Suite.label suite
  infoM testLogger $ "Executing test suite: " ++ suiteLabel

  logLevelEnvVar <- lookupEnv logLevelKey
  let
    options = Suite.options suite
    logLevel = Maybe.fromMaybe "INFO" logLevelEnvVar
    agentStubCommand =
      buildCommand
        [ ("SIMULATE_PID_TRANSLATION",
             booleanEnv $ Suite.usePidTranslation options)
        , ("STARTUP_DELAY",
            (if Suite.startupDelay options then Just "2500" else Nothing))
        , ("SIMULATE_CONNECTION_LOSS",
             booleanEnv $ Suite.simulateConnectionLoss options)
        , ("SECRETS_CONFIG", Suite.customSecretsConfig options)
        , ("LOG_LEVEL", Just logLevel)
        ]
        "stack exec instana-haskell-agent-stub"
    appCommands =
      map
        (\appUnderTest ->
          buildCommand
            [ ("APP_LOG_LEVEL", Just logLevel)
            , ("INSTANA_SERVICE_NAME", Suite.customServiceName options)
            , ("INSTANA_LOG_LEVEL", Just logLevel)
            , ("INSTANA_DISABLE_W3C_TRACE_CORRELATION",
               booleanEnv $ Suite.disableW3cTraceCorrelation options)
            ]
            "stack exec " ++ (Suite.executable appUnderTest)
        )
        (Suite.appsUnderTest options)
    startAllApplicationsUnderTest =
      sequence $
        map
          (\cmd -> do
            infoM testLogger $ "Running: " ++ cmd
            -- We should rather use withCreateProcess instead of createProcess
            -- for the apps under test, too - like for the agentStubCommand, see
            -- below. But how to do that in a loop for an arbitrary
            -- number of processes?
            Process.createProcess $ Process.shell cmd
          )
          appCommands


  infoM testLogger $ "Running: " ++ agentStubCommand
  Process.withCreateProcess
    (Process.shell agentStubCommand)
    (\_ _ _ _ -> do
      _ <- startAllApplicationsUnderTest
      runTests suite
    )


runTests :: Suite -> IO SuiteResult
runTests suite = do
  infoM testLogger "⏱  waiting for agent stub to come up"
  _ <- HttpHelper.retryRequestRecovering TestHelper.pingAgentStub
  infoM testLogger "✅ agent stub is up"
  appsWithPids <-
    sequence $ map pingApp (Suite.appsUnderTest $ Suite.options suite)

  -- We currently assume that there is exactly one app under test that also
  -- connects to the agent.
  let
    appPid =
      snd $
        head $
          filter (\(app, _) -> Suite.connectsToAgent app) appsWithPids

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
  _ <- TestHelper.shutDownApps (Suite.appsUnderTest $ Suite.options suite)
  return results


pingApp :: AppUnderTest -> IO (AppUnderTest, Int)
pingApp appUnderTest = do
  infoM testLogger $
    "⏱  waiting for app " ++ Suite.executable appUnderTest ++ " to come up"
  appPingResponse <-
    HttpHelper.retryRequestRecovering $ TestHelper.pingApp appUnderTest
  let
    appPingBody = HTTP.responseBody appPingResponse
    appPid = (read (LBSC8.unpack appPingBody) :: Int)
  infoM testLogger $
    "✅ app " ++ Suite.executable appUnderTest ++
    " is up, PID is " ++ (show appPid)
  return (appUnderTest, appPid)


-- |Waits for the app under test to establish a connection to the agent, then
-- runs the tests of the given suite.
waitForAgentConnectionAndRun :: Suite -> Int -> IO SuiteResult
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


collectReport :: HUnit.PutText (Int, [String])
collectReport = HUnit.PutText addToReport (0, [])


-- inspired by
-- https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/src/Test.HUnit.Text.html#putTextToHandle
-- to avoid excessive output of progress lines
-- Cases/Tried/Errors/Failures lines, while adding the capability to collect
-- report lines for printing a summary after all suites have been ran.
addToReport :: String -> Bool -> (Int, [String]) -> IO (Int, [String])
-- persistent test report lines, print and collect
addToReport line True (cnt, lines_) = do
  putStrLn (erase cnt ++ line);
  if T.isPrefixOf (T.pack "Cases: ") (T.pack line) then
    -- Do not collect the test progress lines "Cases: 24 Tried: 24 ..."
    return $ (0, lines_)
  else
    -- Collect everything else, most importantly, detailed info about
    -- failed assertions
    return $ (0, lines_ ++ [line])
-- non-persistent progress lines, print but don't collect
addToReport line False (_, lines_) = do
  putStr ('\r' : line)
  return (length line, lines_)


erase :: Int -> String
erase cnt =
  if cnt == 0 then "" else "\r" ++ replicate cnt ' ' ++ "\r"


runTestSuite :: String -> Suite -> IO SuiteResult
runTestSuite pid suite = do
  integrationTestsIO <- wrapSuite pid suite
  (counts, (_, report)) <- HUnitText.runTestText
    collectReport
    integrationTestsIO
  return (counts, report)


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


buildCommand :: [(String, Maybe String)] -> String -> String
buildCommand envVars command =
  let
    envVarsString =
      foldl
        (\cmd (key, var) ->
          case var of
            Just v  -> cmd ++ " " ++ key ++ "=\"" ++ v ++ "\""
            Nothing -> cmd
        )
        ""
        envVars
  in
    if null envVarsString then
      command
    else
      envVarsString ++ " " ++ command


booleanEnv :: Bool -> Maybe String
booleanEnv b =
  if b then Just "true" else Nothing


-- |Environment variable for the log level
logLevelKey :: String
logLevelKey = "TEST_LOG_LEVEL"

