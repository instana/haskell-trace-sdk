{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.LowLevelApi
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
              [ assertEqual "result" "done" result
              , assertEqual
                  "trace ID is consistent"
                  (TraceRequest.t rootEntrySpan)
                  (TraceRequest.t exitSpan)
              , assertEqual
                  "root.traceId == root.spanId"
                  (TraceRequest.t rootEntrySpan)
                  (TraceRequest.s rootEntrySpan)
              , assertBool
                  "root has no parent ID" $
                    isNothing $ TraceRequest.p rootEntrySpan
              , assertEqual
                  "exit parent ID"
                  (Just $ TraceRequest.s rootEntrySpan)
                  (TraceRequest.p exitSpan)
              , assertBool "entry timespan" $ TraceRequest.ts rootEntrySpan > 0
              , assertBool "entry duration" $ TraceRequest.d rootEntrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k rootEntrySpan)
              , assertEqual "entry error" 0 (TraceRequest.ec rootEntrySpan)
              , assertEqual "entry from" from $ TraceRequest.f rootEntrySpan
              , assertBool "exit timespan" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              ]


recordSpans :: InstanaContext -> IO String
recordSpans instana = do
  InstanaSDK.startRootEntry
    instana
    "haskell.dummy.root.entry"
  result <- doExitCall instana
  InstanaSDK.completeEntry instana
  return result


doExitCall :: InstanaContext -> IO String
doExitCall instana = do
  InstanaSDK.startExit
    instana
    "haskell.dummy.exit"
  result <- simulateExitCall
  InstanaSDK.completeExit instana
  return result


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
          maybeEntrySpan =
            TestHelper.getSpanByName "haskell.dummy.entry" spans
          maybeExitSpan = TestHelper.getSpanByName "haskell.dummy.exit" spans
        if isNothing maybeEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just entrySpan = maybeEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "done" result
              , assertEqual "entry.traceId" "trace-id"
                  (TraceRequest.t entrySpan)
              , assertEqual "exit.traceId" "trace-id" (TraceRequest.t exitSpan)
              , assertBool "entry.spanId isn't trace ID" $
                  TraceRequest.s entrySpan /= "trace-id"
              , assertBool "entry.spanId isn't parent ID" $
                  TraceRequest.s entrySpan /= "parent-id"
              , assertEqual "entry.parentId"
                  (Just "parent-id")
                  (TraceRequest.p entrySpan)
              , assertEqual "exit.parentId"
                  (Just $ TraceRequest.s entrySpan)
                  (TraceRequest.p exitSpan)
              , assertBool "entry.timestamp" $ TraceRequest.ts entrySpan > 0
              , assertBool "entry.duration" $ TraceRequest.d entrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k entrySpan)
              , assertEqual "entry error" 0 (TraceRequest.ec entrySpan)
              , assertEqual "entry from" from $ TraceRequest.f entrySpan
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" (2) (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              ]


recordNonRootEntry :: InstanaContext -> IO String
recordNonRootEntry instana = do
  InstanaSDK.startEntry
    instana
    "trace-id"
    "parent-id"
    "haskell.dummy.entry"
  result <- doExitCall instana
  InstanaSDK.completeEntry instana
  return result


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
              [ assertEqual "result" "done" result
              , assertEqual "trace ID is consistent"
                  (TraceRequest.t exitSpan)
                  (TraceRequest.t rootEntrySpan)
              , assertEqual "traceId == spanId"
                  (TraceRequest.s rootEntrySpan)
                  (TraceRequest.t rootEntrySpan)
              , assertBool "root entry parent" $
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
                  , "nested"    .= (Aeson.object [
                      "entry" .= (Aeson.object [
                        "key" .= ("nested.entry.value" :: String)
                      ])
                    ])
                  , "endKind"   .= ("entry" :: String)
                  ]
                )
                (TraceRequest.spanData rootEntrySpan)
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 1 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              , assertEqual "exit data"
                ( Aeson.object
                  [ "data1"     .= ("value1" :: String)
                  , "data2"     .= (1302 :: Int)
                  , "startKind" .= ("exit" :: String)
                  , "data2"     .= (1302 :: Int)
                  , "data3"     .= ("value3" :: String)
                  , "nested"    .= (Aeson.object [
                      "exit" .= (Aeson.object [
                        "key" .= ("nested.exit.value" :: String)
                      ])
                    ])
                  , "endKind"   .= ("exit" :: String)
                  ]
                )
                (TraceRequest.spanData exitSpan)
              ]


recordSpansWithData :: InstanaContext -> IO String
recordSpansWithData instana = do
  InstanaSDK.startRootEntry
    instana
    "haskell.dummy.root.entry"
  InstanaSDK.addData instana (someSpanData "entry")
  result <- doExitCallWithData instana
  InstanaSDK.incrementErrorCount instana
  InstanaSDK.addData instana (moreSpanData "entry")
  InstanaSDK.addDataAt
    instana "nested.entry.key" ("nested.entry.value" :: String)
  InstanaSDK.completeEntry instana
  return result


doExitCallWithData :: InstanaContext -> IO String
doExitCallWithData instana = do
  InstanaSDK.startExit
    instana
    "haskell.dummy.exit"
  InstanaSDK.addData instana (someSpanData "exit")
  result <- simulateExitCall
  InstanaSDK.incrementErrorCount instana
  InstanaSDK.addData instana (moreSpanData "exit")
  InstanaSDK.addDataAt instana "nested.exit.key" ("nested.exit.value" :: String)
  InstanaSDK.completeExit instana
  return result


simulateExitCall :: IO String
simulateExitCall = do
  -- some time needs to pass, otherwise the exit span' duration will be 0
  threadDelay $ 10 * 1000
  return "done"


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

