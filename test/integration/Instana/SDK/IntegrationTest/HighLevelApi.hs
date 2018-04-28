{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.HighLevelApi
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
          maybeRootEntrySpan = TestHelper.getSpanByName "haskell.dummy.root.entry" spans
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
              , assertEqual "entry label"
                  "Haskell Dummy Root Entry Label"
                  (TraceRequest.label rootEntrySpan)
              , assertBool "entry error" $
                  not $ TraceRequest.spanError rootEntrySpan
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit label"
                  "Haskell Dummy Exit Label"
                  (TraceRequest.label exitSpan)
              , assertBool "exit error" $ not $ TraceRequest.spanError exitSpan
              ]


recordSpans :: InstanaContext -> IO String
recordSpans instana = do
  result <-
    InstanaSDK.withRootEntrySimple
      instana
      "haskell.dummy.root.entry"
      "Haskell Dummy Root Entry Label"
      (recordExit instana)
  return $ result ++ "::entry done"


recordExit :: InstanaContext -> EntrySpan -> IO String
recordExit instana entrySpan =
  InstanaSDK.withExitSimple
    instana
    entrySpan
    "haskell.dummy.exit"
    "Haskell Dummy Exit Label"
    simulateSimpleExitCall


simulateSimpleExitCall :: IO String
simulateSimpleExitCall = do
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
  return "exit done"


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
              , assertEqual "entry trace ID" "trace-id" (TraceRequest.t entrySpan)
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
              , assertEqual "entry label"
                  "Haskell Dummy Entry Label"
                  (TraceRequest.label entrySpan)
              , assertBool "entry error" $ not $ TraceRequest.spanError entrySpan
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit label"
                  "Haskell Dummy Exit Label"
                  (TraceRequest.label exitSpan)
              , assertBool "exit error" $ not $ TraceRequest.spanError exitSpan
              ]


recordNonRootEntry :: InstanaContext -> IO String
recordNonRootEntry instana = do
  result <-
    InstanaSDK.withEntrySimple
      instana
      "trace-id"
      "parent-id"
      "haskell.dummy.entry"
      "Haskell Dummy Entry Label"
      (recordExit instana)
  return $ result ++ "::entry done"


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
          maybeRootEntrySpan = TestHelper.getSpanByName "haskell.dummy.root.entry" spans
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
              , assertEqual "entry label"
                  "Haskell Dummy Root Entry Label"
                  (TraceRequest.label rootEntrySpan)
              , assertBool "entry error" $ TraceRequest.spanError rootEntrySpan
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
              , assertEqual "exit label"
                  "Haskell Dummy Exit Label"
                  (TraceRequest.label exitSpan)
              , assertBool "exit error" $ TraceRequest.spanError exitSpan
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
  entryCallResult <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      "Haskell Dummy Root Entry Label"
      (spanDataStart "entry")
      (recordExitWithData instana)
  return entryCallResult


recordExitWithData ::
  InstanaContext
  -> EntrySpan
  -> IO (String, Bool, Value)
recordExitWithData instana entrySpan = do
  exitCallResult <-
    InstanaSDK.withExit
      instana
      entrySpan
      "haskell.dummy.exit"
      "Haskell Dummy Exit Label"
      (spanDataStart "exit")
      simulateExitCall
  return (exitCallResult ++ "::entry done", True, spanDataEnd "entry")


simulateExitCall :: IO (String, Bool, Value)
simulateExitCall = do
  -- some time needs to pass, otherwise the exit span' duration will be 0
  threadDelay $ 10 * 1000
  return ("exit done", True, spanDataEnd "exit")


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

