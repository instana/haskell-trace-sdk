{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.Connection
  ( shouldRetryInitialConnectionEstablishment
  , shouldReestablishLostConnection
  , shouldReconnectAfterAgentRestart
  , shouldUseTranslatedPid
  , shouldUseCustomAgentName
  ) where


import           Control.Concurrent                     (threadDelay)
import           Data.Maybe                             (isJust, isNothing)
import           Data.Text                              (Text)
import           Test.HUnit

import           Instana.SDK.AgentStub.TraceRequest     (From (..), Span)
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import           Instana.SDK.SDK                        (InstanaContext)
import qualified Instana.SDK.SDK                        as InstanaSDK


shouldRetryInitialConnectionEstablishment :: String -> IO Test
shouldRetryInitialConnectionEstablishment _ =
  return $
    TestLabel "shouldRetryInitialConnectionEstablishment" $
      TestCase $
        -- no actuall assertions needed, the fact that this function is executed
        -- is already proof that the connection to the agent stub has been
        -- established in spite of the artificial startup delay, which in turn
        -- verifies that the retry mechanism for the establishing the connection
        -- works
        return ()


shouldReestablishLostConnection :: InstanaContext -> String -> IO Test
shouldReestablishLostConnection instana _ =
  applyLabel "shouldReestablishLostConnection" $ do

    --    0 ms: send span 1
    recordSpans instana "haskell.dummy.connectionloss.entry-1"
    threadDelay $ 2000 * 1000

    -- 1500 ms: agent stub will switch into "connection loss simulation" mode
    --          (that is, spans will be rejected)

    -- 2000 ms: send span 2, will be rejected
    recordSpans instana "haskell.dummy.connectionloss.entry-2"
    threadDelay $ 2000 * 1000

    -- 3000 ms: span 2 will be send latest (force transmission every second)

    -- 3500 ms: agent stub will switch off "connection loss simulation" mode

    -- 4000 ms: send span 3
    recordSpans instana "haskell.dummy.connectionloss.entry-3"
    -- wait for span 3 to arrive, check that 1 and 3 have been received

    spansResult <- TestHelper.waitForSpansMatching
      [ "haskell.dummy.connectionloss.entry-3"
      ]

    TestHelper.resetSpans

    case spansResult of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeSpan1 =
            TestHelper.getSpanByName
              "haskell.dummy.connectionloss.entry-1"
              spans
          -- What about entry-2? Do we expect it to have been buffered and
          -- resend? Do we expect it to be dropped? Right now, it is dropped.
          maybeSpan2 =
            TestHelper.getSpanByName
              "haskell.dummy.connectionloss.entry-2"
              spans
          maybeSpan3 =
            TestHelper.getSpanByName
              "haskell.dummy.connectionloss.entry-3"
              spans
        if isNothing maybeSpan1
        then
          failIO "expected span before connection loss has not been recorded"
        else
          if isNothing maybeSpan3
          then
            failIO "expected span after connection loss has not been recorded"
          else
            return $ TestCase $
              assertBool "expected span 2 to be dropped" (isNothing maybeSpan2)


recordSpans :: InstanaContext -> Text -> IO ()
recordSpans instana spanName =
  InstanaSDK.withRootEntry
    instana
    spanName
    simulateWork


simulateWork :: IO ()
simulateWork =
  -- 10 milliseconds
  threadDelay $ 10 * 1000


shouldReconnectAfterAgentRestart :: InstanaContext -> String -> IO Test
shouldReconnectAfterAgentRestart instana _ =
  applyLabel "shouldReconnectAfterAgentRestart" $ do
  (_, spansResultsBefore) <-
    TestHelper.withSpanCreation
      (recordSpans instana "haskell.dummy.agent-restart.before-restart")
      [ "haskell.dummy.agent-restart.before-restart" ]

  -- reset discoveries, effectively simulating an agent restart
  TestHelper.resetDiscoveries

  -- send another span, should be rejected because, from the perspective
  -- of the agent stub, announce hasn't happen yet. At the same time, this
  -- this failure should trigger a new connection handshake between the
  -- monitored process and the agent, so after loosing this span  we can wait
  -- for the connection handshake to happen again (and alls spans after that
  -- should be processed again).
  _ <- recordSpans instana "haskell.dummy.agent-restart.after-restart-1"

  -- wait for connection self healing
  discoveries <- TestHelper.waitForInternalAgentConnection False
  case discoveries of
    Left message ->
      assertFailure $
        "Could not establish agent connection: " ++ message
    Right _ ->
      verifyRecconnectAfterAgentRestart instana spansResultsBefore


verifyRecconnectAfterAgentRestart ::
  InstanaContext
  -> Either String [Span]
  -> IO Test
verifyRecconnectAfterAgentRestart instana spansResultsBefore = do
  -- send another span, this one should be recorded again
  (_, spansResultAfter) <-
    TestHelper.withSpanCreation
      (recordSpans instana "haskell.dummy.agent-restart.after-restart-2")
      [ "haskell.dummy.agent-restart.after-restart-2" ]

  case (spansResultsBefore, spansResultAfter) of
    (Left failure1, Left failure2) ->
      failIO $ "Could not load recorded spans from agent stub: " ++
        failure1 ++ "; " ++ failure2
    (Left failure, _) ->
      failIO $
        "Could not load recorded spans from agent stub before " ++
        "restart: " ++ failure
    (_, Left failure) ->
      failIO $
        "Could not load recorded spans from agent stub after restart: " ++
          failure
    (Right spansBefore, Right spansAfter) -> do
      let
        maybeSpanBefore = TestHelper.getSpanByName
          "haskell.dummy.agent-restart.before-restart"
          spansBefore
        maybeSpanAfter1 = TestHelper.getSpanByName
          "haskell.dummy.agent-restart.after-restart-1"
          spansAfter
        maybeSpanAfter2 = TestHelper.getSpanByName
          "haskell.dummy.agent-restart.after-restart-2"
          spansAfter
      assertAllIO
        [ assertBool "span has not been recorded before agent restart" $
            isJust maybeSpanBefore
        , assertBool "span has not been recorded after agent restart" $
            isJust maybeSpanAfter2
        , assertBool "expected span 2 to be dropped" $
            isNothing maybeSpanAfter1
        ]


shouldUseTranslatedPid :: InstanaContext -> String -> IO Test
shouldUseTranslatedPid instana pid = do
  applyLabel "shouldUseTranslatedPid" $ do
    let
      from = Just $ From pid
    (_, spansResults) <-
      TestHelper.withSpanCreation
        (recordSpans instana "haskell.dummy.pid-translation")
        [ "haskell.dummy.pid-translation" ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeSpan = TestHelper.getSpanByName
            "haskell.dummy.pid-translation"
            spans
        if isNothing maybeSpan
          then
            failIO "expected span has not been recorded"
          else do
            let
              Just entrySpan = maybeSpan
            assertAllIO
              [ assertBool "entry timestamp" $ TraceRequest.ts entrySpan > 0
              , assertBool "entry duration" $ TraceRequest.d entrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k entrySpan)
              , assertEqual "entry error" 0 (TraceRequest.ec entrySpan)
              , assertEqual "entry from" from $ TraceRequest.f entrySpan
              ]


shouldUseCustomAgentName :: String -> IO Test
shouldUseCustomAgentName _ =
  return $
    TestLabel "shouldUseCustomAgentName" $
      TestCase $
        -- no actuall assertions needed, the fact that this function is executed
        -- is already proof that the connection to the agent stub has been
        -- established in spite of the custom agent name parameter.
        return ()

