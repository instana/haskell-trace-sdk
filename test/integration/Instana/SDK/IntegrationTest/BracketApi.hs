{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.BracketApi
  ( shouldRecordSpans
  , shouldRecordNonRootEntry
  , shouldMergeData
  ) where


import           Control.Concurrent                     (threadDelay)
import           Data.Aeson                             (Value, (.=))
import qualified Data.Aeson                             as Aeson
import           Data.Maybe                             (isNothing)
import           Test.HUnit

import           Instana.SDK.AgentStub.TraceRequest     (From (..))
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import           Instana.SDK.SDK                        (InstanaContext)
import qualified Instana.SDK.SDK                        as InstanaSDK


shouldRecordSpans :: InstanaContext -> String -> IO Test
shouldRecordSpans instana pid =
  applyLabel "shouldRecordSpans" $ do
    let
      from = Just $ From pid
    (result, spansResults) <-
      TestHelper.withSpanCreation
        (recordSpans instana)
        [ "haskell.dummy.root.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeRootEntrySpan =
            TestHelper.getSpanByName "haskell.dummy.root.entry" spans
          maybeExitSpan = TestHelper.getSpanByName "haskell.dummy.exit" spans
        if isNothing maybeRootEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just rootEntrySpan = maybeRootEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done::entry done" result
              , assertEqual "trace ID is consistent"
                  (TraceRequest.t exitSpan)
                  (TraceRequest.t rootEntrySpan)
              , assertEqual "root.traceId = root.spanId"
                  (TraceRequest.t rootEntrySpan)
                  (TraceRequest.s rootEntrySpan)
              , assertBool "root parent Id" $
                  isNothing $ TraceRequest.p rootEntrySpan
              , assertEqual "exit parent ID"
                  (Just $ TraceRequest.s rootEntrySpan)
                  (TraceRequest.p exitSpan)
              , assertBool "entry timestamp" $ TraceRequest.ts rootEntrySpan > 0
              , assertBool "entry duration" $ TraceRequest.d rootEntrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k rootEntrySpan)
              , assertEqual "entry error" 0 (TraceRequest.ec rootEntrySpan)
              , assertEqual "entry from" from $ TraceRequest.f rootEntrySpan
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              ]


recordSpans :: InstanaContext -> IO String
recordSpans instana = do
  result <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (recordExit instana)
  return $ result ++ "::entry done"


recordExit :: InstanaContext -> IO String
recordExit instana =
  InstanaSDK.withExit
    instana
    "haskell.dummy.exit"
    simulateSimpleExitCall


simulateSimpleExitCall :: IO String
simulateSimpleExitCall = do
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
  return "exit done"


shouldRecordNonRootEntry :: InstanaContext -> String -> IO Test
shouldRecordNonRootEntry instana pid =
  applyLabel "shouldRecordNonRootEntry" $ do
    let
      from = Just $ From pid
    (result, spansResults) <-
      TestHelper.withSpanCreation
        (recordNonRootEntry instana)
        [ "haskell.dummy.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeEntrySpan = TestHelper.getSpanByName "haskell.dummy.entry" spans
          maybeExitSpan = TestHelper.getSpanByName "haskell.dummy.exit" spans
        if isNothing maybeEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just entrySpan = maybeEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done::entry done" result
              , assertEqual "entry trace ID" "trace-id"
                  (TraceRequest.t entrySpan)
              , assertEqual "exit trace ID" "trace-id" (TraceRequest.t exitSpan)
              , assertBool "entry span ID" $
                  TraceRequest.s entrySpan /= "trace-id"
              , assertBool "entry span ID" $
                  TraceRequest.s entrySpan /= "parent-id"
              , assertEqual "entry parent ID"
                  (Just "parent-id")
                  (TraceRequest.p entrySpan)
              , assertEqual "exit parent ID"
                  (Just $ TraceRequest.s entrySpan)
                  (TraceRequest.p exitSpan)
              , assertBool "entry timestamp" $ TraceRequest.ts entrySpan > 0
              , assertBool "entry duration" $ TraceRequest.d entrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k entrySpan)
              , assertEqual "entry error" 0 (TraceRequest.ec entrySpan)
              , assertEqual "entry from" from $ TraceRequest.f entrySpan
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              ]


recordNonRootEntry :: InstanaContext -> IO String
recordNonRootEntry instana = do
  result <-
    InstanaSDK.withEntry
      instana
      "trace-id"
      "parent-id"
      "haskell.dummy.entry"
      (recordExit instana)
  return $ result ++ "::entry done"


shouldMergeData :: InstanaContext -> String -> IO Test
shouldMergeData instana pid =
  applyLabel "shouldMergeData" $ do
    let
      from = Just $ From pid
    (result, spansResults) <-
      TestHelper.withSpanCreation
        (recordSpansWithData instana)
        [ "haskell.dummy.root.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeRootEntrySpan =
            TestHelper.getSpanByName "haskell.dummy.root.entry" spans
          maybeExitSpan = TestHelper.getSpanByName "haskell.dummy.exit" spans
        if isNothing maybeRootEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just rootEntrySpan = maybeRootEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done::entry done" result
              , assertEqual "trace ID is consistent"
                  (TraceRequest.t rootEntrySpan)
                  (TraceRequest.t exitSpan)
              , assertEqual "root.traceId == root.spanId"
                  (TraceRequest.s rootEntrySpan)
                  (TraceRequest.t rootEntrySpan)
              , assertBool "root has no parent" $
                  isNothing $ TraceRequest.p rootEntrySpan
              , assertEqual "exit parent"
                  (Just $ TraceRequest.s rootEntrySpan)
                  (TraceRequest.p exitSpan)
              , assertBool "entry timestamp" $ TraceRequest.ts rootEntrySpan > 0
              , assertBool "entry duration" $ TraceRequest.d rootEntrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k rootEntrySpan)
              , assertEqual "entry error" 1 (TraceRequest.ec rootEntrySpan)
              , assertEqual "entry from" from $ TraceRequest.f rootEntrySpan
              , assertEqual "entry data"
                  ( Aeson.object
                    [ "data1"     .= ("value1" :: String)
                    , "data2"     .= (1302 :: Int)
                    , "startKind" .= ("entry" :: String)
                    , "data2"     .= (1302 :: Int)
                    , "data3"     .= ("value3" :: String)
                    , "endKind"   .= ("entry" :: String)
                    ]
                  )
                  (TraceRequest.spanData rootEntrySpan)
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 2 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              , assertEqual "exit data"
                  ( Aeson.object
                    [ "data1"     .= ("value1" :: String)
                    , "data2"     .= (1302 :: Int)
                    , "startKind" .= ("exit" :: String)
                    , "data2"     .= (1302 :: Int)
                    , "data3"     .= ("value3" :: String)
                    , "endKind"   .= ("exit" :: String)
                    , "nested"    .= (Aeson.object
                        [ "key1" .= ("nested.text.value1" :: String)
                        , "key2" .= ("nested.text.value2" :: String)
                        , "key3" .= (1604 :: Integer)
                        ]
                      )
                    ]
                  )
                  (TraceRequest.spanData exitSpan)
              ]


recordSpansWithData :: InstanaContext -> IO String
recordSpansWithData instana = do
  entryCallResult <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (recordExitWithData instana)
  return entryCallResult


recordExitWithData :: InstanaContext -> IO String
recordExitWithData instana = do
  InstanaSDK.addData instana (someSpanData "entry")
  exitCallResult <-
    InstanaSDK.withExit
      instana
      "haskell.dummy.exit"
      (simulateExitCall instana)
  InstanaSDK.incrementErrorCount instana
  InstanaSDK.addData instana (moreSpanData "entry")
  return $ exitCallResult ++ "::entry done"


simulateExitCall :: InstanaContext -> IO String
simulateExitCall instana = do
  InstanaSDK.addData instana (someSpanData "exit")
  -- some time needs to pass, otherwise the exit span' duration will be 0
  threadDelay $ 10 * 1000
  InstanaSDK.addToErrorCount instana 2
  InstanaSDK.addData instana (moreSpanData "exit")
  InstanaSDK.addDataAt instana "nested.key1" ("nested.text.value1" :: String)
  InstanaSDK.addDataAt instana "nested.key2" ("nested.text.value2" :: String)
  InstanaSDK.addDataAt instana "nested.key3" (1604 :: Int)
  return "exit done"


someSpanData :: String -> Value
someSpanData kind =
   Aeson.object
     [ "data1"     .= ("value1" :: String)
     , "data2"     .= (42 :: Int)
     , "startKind" .= kind
     ]


moreSpanData :: String -> Value
moreSpanData kind =
   Aeson.object
     [ "data2"   .= (1302 :: Int)
     , "data3"   .= ("value3" :: String)
     , "endKind" .= kind
     ]

