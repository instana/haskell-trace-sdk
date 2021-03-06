{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.LowLevelApi
  ( shouldRecordSpans
  , shouldRecordNonRootEntry
  , shouldMergeTags
  ) where


import           Data.Aeson                             ((.=))
import qualified Data.Aeson                             as Aeson
import           Data.ByteString.Lazy.Char8             as LBSC8
import           Data.Maybe                             (isNothing)
import qualified Network.HTTP.Client                    as HTTP
import           Test.HUnit

import           Instana.SDK.AgentStub.TraceRequest     (From (..))
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.Suite      as Suite
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper


shouldRecordSpans :: String -> IO Test
shouldRecordSpans pid =
  applyLabel "shouldRecordSpans" $ do
    let
      from = Just $ From pid "agent-stub-id"
    (result, spansResults) <-
      TestHelper.withSpanCreation
        createRootEntry
        [ "haskell.dummy.root.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
         failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeRootEntrySpan =
            TestHelper.getSpanBySdkName "haskell.dummy.root.entry" spans
          maybeExitSpan = TestHelper.getSpanBySdkName "haskell.dummy.exit" spans
        if isNothing maybeRootEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just rootEntrySpan = maybeRootEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done" result
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


createRootEntry :: IO String
createRootEntry = do
  response <-
    HttpHelper.doAppRequest
      Suite.testServer "low/level/api/root" "POST" []
  return $ LBSC8.unpack $ HTTP.responseBody response


shouldRecordNonRootEntry :: String -> IO Test
shouldRecordNonRootEntry pid =
  applyLabel "shouldRecordNonRootEntry" $ do
    let
      from = Just $ From pid "agent-stub-id"
    (result, spansResults) <-
      TestHelper.withSpanCreation
        createNonRootEntry
        [ "haskell.dummy.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeEntrySpan =
            TestHelper.getSpanBySdkName "haskell.dummy.entry" spans
          maybeExitSpan = TestHelper.getSpanBySdkName "haskell.dummy.exit" spans
        if isNothing maybeEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just entrySpan = maybeEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done" result
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


createNonRootEntry :: IO String
createNonRootEntry = do
  response <-
    HttpHelper.doAppRequest Suite.testServer "low/level/api/non-root" "POST" []
  return $ LBSC8.unpack $ HTTP.responseBody response


shouldMergeTags :: String -> IO Test
shouldMergeTags pid =
  applyLabel "shouldMergeTags" $ do
    let
      from = Just $ From pid "agent-stub-id"
    (result, spansResults) <-
      TestHelper.withSpanCreation
        createSpansWithTags
        [ "haskell.dummy.root.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeRootEntrySpan =
            TestHelper.getSpanBySdkName "haskell.dummy.root.entry" spans
          maybeExitSpan = TestHelper.getSpanBySdkName "haskell.dummy.exit" spans
        if isNothing maybeRootEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just rootEntrySpan = maybeRootEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done" result
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
                  [ "sdk"    .= (Aeson.object
                    [ "name" .= ("haskell.dummy.root.entry" :: String)
                    , "type" .= ("entry" :: String)
                    , "custom" .= (Aeson.object
                      [ "tags" .= (Aeson.object
                        [ "startKind" .= ("entry" :: String)
                        , "endKind"   .= ("entry" :: String)
                        , "data1"     .= ("value1" :: String)
                        , "data2"     .= (1302 :: Int)
                        , "data3"     .= ("value3" :: String)
                        , "nested"    .= (Aeson.object [
                            "entry" .= (Aeson.object [
                              "key" .= ("nested.entry.value" :: String)
                            ])
                          ])
                        ])
                      ])
                    ])
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
                  [ "sdk"    .= (Aeson.object
                    [ "name" .= ("haskell.dummy.exit" :: String)
                    , "type" .= ("exit" :: String)
                    , "custom" .= (Aeson.object
                      [ "tags" .= (Aeson.object
                        [ "startKind" .= ("exit" :: String)
                        , "endKind"   .= ("exit" :: String)
                        , "data1"     .= ("value1" :: String)
                        , "data2"     .= (1302 :: Int)
                        , "data3"     .= ("value3" :: String)
                        , "nested"    .= (Aeson.object
                          [ "exit" .= (Aeson.object
                            [ "key" .= ("nested.exit.value" :: String)
                            ])
                          ])
                        ])
                      ])
                    ])
                  ]
                )
                (TraceRequest.spanData exitSpan)
              ]


createSpansWithTags :: IO String
createSpansWithTags = do
  response <-
    HttpHelper.doAppRequest Suite.testServer "low/level/api/with-tags" "POST" []
  return $ LBSC8.unpack $ HTTP.responseBody response

