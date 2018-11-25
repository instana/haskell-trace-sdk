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

import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import           Instana.SDK.SDK                        (InstanaContext)
import qualified Instana.SDK.SDK                        as InstanaSDK
import           Instana.SDK.Span.EntrySpan             (EntrySpan)


shouldRecordSpans :: InstanaContext -> IO Test
shouldRecordSpans instana =
  applyLabel "shouldRecordSpans" $ do
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
              , assertEqual "entry error" 0
                  (TraceRequest.ec rootEntrySpan)
              , assertBool "exit timespan" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              ]


recordSpans :: InstanaContext -> IO String
recordSpans instana = do
  entrySpan <-
    InstanaSDK.startRootEntry
      "haskell.dummy.root.entry"
  result <- doExitCall instana entrySpan
  InstanaSDK.completeEntry instana entrySpan 0
  return result


doExitCall :: InstanaContext -> EntrySpan -> IO String
doExitCall instana entrySpan = do
  exitSpan <-
    InstanaSDK.startExit
      entrySpan
      "haskell.dummy.exit"
  result <- simulateExitCall
  InstanaSDK.completeExit instana exitSpan 0
  return result


shouldRecordNonRootEntry :: InstanaContext -> IO Test
shouldRecordNonRootEntry instana =
  applyLabel "shouldRecordNonRootEntry" $ do
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
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" (2) (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              ]


recordNonRootEntry :: InstanaContext -> IO String
recordNonRootEntry instana = do
  entrySpan <-
    InstanaSDK.startEntry
      "trace-id"
      "parent-id"
      "haskell.dummy.entry"
  result <- doExitCall instana entrySpan
  InstanaSDK.completeEntry instana entrySpan 0
  return result


shouldMergeData :: InstanaContext -> IO Test
shouldMergeData instana =
  applyLabel "shouldMergeData" $ do
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
              , assertEqual "entry error" 1
                  (TraceRequest.ec rootEntrySpan)
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
              , assertEqual "exit error" 1 (TraceRequest.ec exitSpan)
              , assertEqual "exit data"
                ( Aeson.object
                  [ "data1"     .= ("value1" :: String)
                  , "data2"     .= (1302 :: Int)
                  , "startKind" .= ("exit" :: String)
                  , "data2"     .= (1302 :: Int)
                  , "data3"     .= ("value3" :: String)
                  , "endKind"   .= ("exit" :: String)
                  ]
                )
                (TraceRequest.spanData exitSpan)
              ]


recordSpansWithData :: InstanaContext -> IO String
recordSpansWithData instana = do
  entrySpan <-
    InstanaSDK.startRootEntryWithData
      "haskell.dummy.root.entry"
      (spanDataStart "entry")
  result <- doExitCallWithData instana entrySpan
  InstanaSDK.completeEntryWithData instana entrySpan 1 (spanDataEnd "entry")
  return result


doExitCallWithData :: InstanaContext -> EntrySpan -> IO String
doExitCallWithData instana entrySpan = do
  exitSpan <-
    InstanaSDK.startExitWithData
      entrySpan
      "haskell.dummy.exit"
      (spanDataStart "exit")
  result <- simulateExitCall
  InstanaSDK.completeExitWithData instana exitSpan 1 (spanDataEnd "exit")
  return result


simulateExitCall :: IO String
simulateExitCall = do
  -- some time needs to pass, otherwise the exit span' duration will be 0
  threadDelay $ 10 * 1000
  return "done"


spanDataStart :: String -> Value
spanDataStart kind =
   Aeson.object
     [ "data1"     .= ("value1" :: String)
     , "data2"     .= (42 :: Int)
     , "startKind" .= kind
     ]


spanDataEnd :: String -> Value
spanDataEnd kind =
   Aeson.object
     [ "data2"   .= (1302 :: Int)
     , "data3"   .= ("value3" :: String)
     , "endKind" .= kind
     ]

