module Instana.SDK.IntegrationTest.TestSuites (allTests) where


import           System.Process                                 as Process
import           Test.HUnit

import qualified Data.List                                      as List
import           Data.Maybe                                     (isJust)
import qualified Instana.SDK.IntegrationTest.BracketApi         as BracketApi
import qualified Instana.SDK.IntegrationTest.Connection         as Connection
import qualified Instana.SDK.IntegrationTest.HttpTracingHeaders as HttpTracingHeaders
import           Instana.SDK.IntegrationTest.HUnitExtra         (ConditionalSuite (..),
                                                                 isExclusive,
                                                                 mergeCounts,
                                                                 unwrapOrSkip)
import qualified Instana.SDK.IntegrationTest.LowLevelApi        as LowLevelApi
import qualified Instana.SDK.IntegrationTest.Runner             as TestRunner
import           Instana.SDK.IntegrationTest.Suite              (Suite (Suite), SuiteGenerator (External, Internal))
import qualified Instana.SDK.IntegrationTest.Suite              as Suite


allTests :: IO Counts
allTests = do
  let
    allTheTests =
      [ testSpanRecording
      , testConnectionEstablishment
      , testConnectionLoss
      , testAgentRestart
      , testPidTranslation
      , testCustomAgentName
      , testHttpTracingHeaders
      ]
    exlusiveSuite =
      List.find isExclusive allTheTests
    unwrapped =
      case exlusiveSuite of
        Just suite ->
          [unwrapOrSkip suite]
        Nothing ->
          List.map unwrapOrSkip allTheTests
  if isJust exlusiveSuite then
    putStrLn $ "\n\nRunning one exclusive suite, ignoring all others."
  else
    putStrLn $ "\n\nRunning " ++ show (List.length unwrapped) ++ " test suite(s)..."
  results <- sequence unwrapped
  let
    mergedResults = mergeCounts results
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


testSpanRecording :: ConditionalSuite
testSpanRecording =
  let
    suiteGenerator =
      Internal (\instana ->
        [ Suite
           { Suite.label = "Low Level API"
           , Suite.tests = (\pid ->
              [ LowLevelApi.shouldRecordSpans instana pid
              , LowLevelApi.shouldRecordNonRootEntry instana pid
              , LowLevelApi.shouldMergeData instana pid
              ])
           }
        , Suite
           { Suite.label = "Bracket API"
           , Suite.tests = (\pid ->
              [ BracketApi.shouldRecordSpans instana pid
              , BracketApi.shouldRecordNonRootEntry instana pid
              , BracketApi.shouldMergeData instana pid
              ])
           }
        ]
      , Suite.defaultOpts
      )
  in
    Run $
      Process.withCreateProcess
        (Process.shell "stack exec instana-haskell-agent-stub")
        (TestRunner.runTestsIgnoringHandles suiteGenerator)


testConnectionEstablishment :: ConditionalSuite
testConnectionEstablishment =
  let
    suiteGenerator =
      Internal (\_ ->
        [ Suite
           { Suite.label = "Initial Connection Establishment"
           , Suite.tests = (\pid -> [
               Connection.shouldRetryInitialConnectionEstablishment pid
             ])
           }
        ]
      , Suite.defaultOpts
      )
  in
    Run $
      Process.withCreateProcess
        (Process.shell
          "STARTUP_DELAY=2500 stack exec instana-haskell-agent-stub"
        )
       (TestRunner.runTestsIgnoringHandles suiteGenerator)


testConnectionLoss :: ConditionalSuite
testConnectionLoss =
  let
    suiteGenerator =
      Internal (\instana ->
        [ Suite
          { Suite.label = "Connection Loss"
          , Suite.tests = (\pid -> [
              Connection.shouldReestablishLostConnection instana pid
            ])
          }
        ]
      , Suite.defaultOpts
      )
  in
    Run $
      Process.withCreateProcess
        (Process.shell
          "SIMULATE_CONNECTION_LOSS=true stack exec instana-haskell-agent-stub"
        )
       (TestRunner.runTestsIgnoringHandles suiteGenerator)


testAgentRestart :: ConditionalSuite
testAgentRestart =
  let
    suiteGenerator =
      Internal (\instana ->
        [ Suite
            { Suite.label = "Agent Restart"
            , Suite.tests = (\pid -> [
                Connection.shouldReconnectAfterAgentRestart instana pid
              ])
            }
        ]
      , Suite.defaultOpts
      )
  in
    Run $
      Process.withCreateProcess
        (Process.shell
          "stack exec instana-haskell-agent-stub"
        )
       (TestRunner.runTestsIgnoringHandles suiteGenerator)


testPidTranslation :: ConditionalSuite
testPidTranslation =
  let
    suiteGenerator =
      Internal (\instana ->
        [ Suite
           { Suite.label =  "PID transaltion"
           , Suite.tests = (\pid -> [
               Connection.shouldUseTranslatedPid instana pid
             ])
           }
        ]
      , Suite.withPidTranslation
      )
  in
    Run $
      Process.withCreateProcess
        (Process.shell
          "SIMULATE_PID_TRANSLATION=why_not stack exec instana-haskell-agent-stub"
        )
       (TestRunner.runTestsIgnoringHandles suiteGenerator)


testCustomAgentName :: ConditionalSuite
testCustomAgentName =
  let
    suiteGenerator =
      Internal (\_ ->
        [ Suite
          { Suite.label = "Custom Agent Name"
          , Suite.tests = (\pid -> [
              Connection.shouldUseCustomAgentName pid
            ])
          }
        ]
      , Suite.withCustomAgentName "Devil in Disguise"
      )
  in
    Run $
      Process.withCreateProcess
        (Process.shell $
          "AGENT_NAME=\"Devil in Disguise\" " ++
          "stack exec instana-haskell-agent-stub"
        )
       (TestRunner.runTestsIgnoringHandles suiteGenerator)


testHttpTracingHeaders :: ConditionalSuite
testHttpTracingHeaders =
  let
    suiteGenerator =
      External (
        [ Suite
            { Suite.label = "HTTP Tracing Headers"
            , Suite.tests = (\pid -> [
                HttpTracingHeaders.shouldCreateRootEntryWithBracketApi pid
              , HttpTracingHeaders.shouldCreateNonRootEntryWithBracketApi pid
              , HttpTracingHeaders.shouldSuppressWithBracketApi
              , HttpTracingHeaders.shouldCreateRootEntryWithLowLevelApi pid
              , HttpTracingHeaders.shouldCreateNonRootEntryWithLowLevelApi pid
              , HttpTracingHeaders.shouldSuppressWithLowLevelApi
              ])
            }
          ]
        , Suite.defaultOpts
        )
  in
  Run $
    Process.withCreateProcess
      (Process.shell "stack exec instana-haskell-agent-stub")
      (\_ _ _ _ ->
        (Process.withCreateProcess
          (Process.shell "INSTANA_LOG_LEVEL=TRACE INSTANA_LOG_LEVEL_STDOUT=TRACE stack exec instana-haskell-test-wai-server")
          (TestRunner.runTestsIgnoringHandles suiteGenerator)
        )
      )

