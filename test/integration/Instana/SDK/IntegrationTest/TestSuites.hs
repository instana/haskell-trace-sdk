module Instana.SDK.IntegrationTest.TestSuites (allTests) where


import           System.Process                           as Process
import           Test.HUnit

import qualified Instana.SDK.IntegrationTest.Connection   as Connection
import qualified Instana.SDK.IntegrationTest.HighLevelApi as HighLevelApi
import           Instana.SDK.IntegrationTest.HUnitExtra   (mergeCounts)
import qualified Instana.SDK.IntegrationTest.LowLevelApi  as LowLevelApi
import qualified Instana.SDK.IntegrationTest.Runner       as TestRunner
import           Instana.SDK.IntegrationTest.Suite        (Suite (Suite))
import qualified Instana.SDK.IntegrationTest.Suite        as Suite


allTests :: IO Counts
allTests = do
  spanRecordingSuite           <- testSpanRecording
  connectionEstablishmentSuite <- testConnectionEstablishment
  connectionLoss               <- testConnectionLoss
  agentRestart                 <- testAgentRestart
  pidTranslation               <- testPidTranslation
  customAgentName              <- testCustomAgentName
  let
    mergedResults =
      mergeCounts
        [ spanRecordingSuite
        , connectionEstablishmentSuite
        , connectionLoss
        , agentRestart
        , pidTranslation
        , customAgentName
        ]
    caseCount = cases mergedResults
    triedCount = tried mergedResults
    errCount = errors mergedResults
    failCount = failures mergedResults
  putStrLn $
    "SUMMARY: Cases: " ++ show caseCount ++
    "  Tried: " ++ show triedCount ++
    "  Errors: " ++ show errCount ++
    "  Failures: " ++ show failCount
  if errCount > 0
    then putStrLn "😱 There have been errors! 😱"
    else
      if failCount > 0
        then putStrLn "😭 There have been test failures. 😭"
        else putStrLn "🎉 All tests have passed. 🎉"
  return mergedResults


testSpanRecording :: IO Counts
testSpanRecording =
  let
    suiteGenerator =
      (\instana ->
        [ Suite
           { Suite.label = "Low Level API"
           , Suite.tests =
              [ LowLevelApi.shouldRecordSpans instana
              , LowLevelApi.shouldRecordNonRootEntry instana
              , LowLevelApi.shouldMergeData instana
              ]
           }
        , Suite
           { Suite.label = "High Level API"
           , Suite.tests =
              [ HighLevelApi.shouldRecordSpans instana
              , HighLevelApi.shouldRecordNonRootEntry instana
              , HighLevelApi.shouldMergeData instana
              ]
           }
        ]
      , Suite.defaultOpts
      )
  in
    Process.withCreateProcess
      (Process.shell "stack exec instana-haskell-agent-stub")
      (TestRunner.runTestsIgnoringHandles suiteGenerator)


testConnectionEstablishment :: IO Counts
testConnectionEstablishment =
  let
    suiteGenerator =
      (\_ ->
        [ Suite
           { Suite.label = "Initial Connection Establishment"
           , Suite.tests = [
               Connection.shouldRetryInitialConnectionEstablishment
             ]
           }
        ]
      , Suite.defaultOpts
      )
  in
    Process.withCreateProcess
      (Process.shell
        "STARTUP_DELAY=2500 stack exec instana-haskell-agent-stub"
      )
     (TestRunner.runTestsIgnoringHandles suiteGenerator)


testConnectionLoss :: IO Counts
testConnectionLoss =
  let
    suiteGenerator =
      (\instana ->
        [ Suite
          { Suite.label = "Connection Loss"
          , Suite.tests = [
              Connection.shouldReestablishLostConnection instana
            ]
          }
        ]
      , Suite.defaultOpts
      )
  in
    Process.withCreateProcess
      (Process.shell
        "SIMULATE_CONNECTION_LOSS=true stack exec instana-haskell-agent-stub"
      )
     (TestRunner.runTestsIgnoringHandles suiteGenerator)


testAgentRestart :: IO Counts
testAgentRestart =
  let
    suiteGenerator =
      (\instana ->
        [ Suite
            { Suite.label = "Agent Restart"
            , Suite.tests = [
                Connection.shouldReconnectAfterAgentRestart instana
              ]
            }
        ]
      , Suite.defaultOpts
      )
  in
    Process.withCreateProcess
      (Process.shell
        "stack exec instana-haskell-agent-stub"
      )
     (TestRunner.runTestsIgnoringHandles suiteGenerator)


testPidTranslation :: IO Counts
testPidTranslation =
  let
    suiteGenerator =
      (\instana ->
        [ Suite
           { Suite.label =  "PID transaltion"
           , Suite.tests = [ Connection.shouldUseTranslatedPid instana ]

           }
        ]
      , Suite.withPidTranslation
      )
  in
    Process.withCreateProcess
      (Process.shell
        "SIMULATE_PID_TRANSLATION=why_not stack exec instana-haskell-agent-stub"
      )
     (TestRunner.runTestsIgnoringHandles suiteGenerator)


testCustomAgentName :: IO Counts
testCustomAgentName =
  let
    suiteGenerator =
      (\_ ->
        [ Suite
          { Suite.label = "Custom Agent Name"
          , Suite.tests = [
              Connection.shouldUseCustomAgentName
            ]
          }
        ]
      , Suite.withCustomAgentName "Devil in Disguise"
      )
  in
    Process.withCreateProcess
      (Process.shell $
        "AGENT_NAME=\"Devil in Disguise\" " ++
        "stack exec instana-haskell-agent-stub"
      )
     (TestRunner.runTestsIgnoringHandles suiteGenerator)

