{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.SpanTest (allTests) where


import           Test.HUnit

import qualified Instana.SDK.Internal.Id     as Id
import           Instana.SDK.Span.EntrySpan  (EntrySpan (RootEntrySpan))
import           Instana.SDK.Span.RootEntry  (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry  as RootEntry
import qualified Instana.SDK.Span.SimpleSpan as SimpleSpan
import           Instana.SDK.Span.Span       (Span (..), SpanKind (..))
import qualified Instana.SDK.Span.Span       as Span
import           Instana.SDK.Span.SpanData   (SpanData (SpanData))
import qualified Instana.SDK.Span.SpanData   as SpanData


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldAddStringNotNested" shouldAddStringNotNested
    , TestLabel "shouldAddStringNested" shouldAddStringNested
    , TestLabel "shouldAddStringDeeplyNested" shouldAddStringDeeplyNested
    , TestLabel "shouldAddIntDeeplyNested" shouldAddIntDeeplyNested
    , TestLabel "shouldAddDoubleDeeplyNested" shouldAddDoubleDeeplyNested
    , TestLabel "shouldAddBooleanDeeplyNested" shouldAddBooleanDeeplyNested
    , TestLabel "shouldAddTwoAtDifferentPaths" shouldAddTwoAtDifferentPaths
    , TestLabel "shouldAddTwoAtSamePath" shouldAddTwoAtSamePath
    , TestLabel "shouldAddTwoAtSameNestedPath" shouldAddTwoAtSameNestedPath
    , TestLabel "shouldAddMultipleRegistered" shouldAddMultipleRegistered
    , TestLabel "shouldMergeListValues" shouldMergeListValues
    , TestLabel "shouldAddMultipleTags" shouldAddMultipleTags
    , TestLabel "shouldConvertToSimpleSpanFormat" shouldConvertToSimpleSpanFormat
    ]


shouldAddStringNotNested :: Test
shouldAddStringNotNested =
  let
    span_ = Span.addRegisteredDataAt "path" ("value" :: String) $ entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddStringNotNested"
      (SpanData [
        SpanData.simpleAnnotation "path" ("value" :: String)
      ])
      spanData


shouldAddStringNested :: Test
shouldAddStringNested =
  let
    span_ =
      Span.addRegisteredDataAt
        "nested.path"
        ("value" :: String) $
          entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddStringNested"
      (SpanData [
        SpanData.objectAnnotation "nested" [
          SpanData.simpleAnnotation "path" ("value" :: String)
        ]
      ])
      spanData


shouldAddStringDeeplyNested :: Test
shouldAddStringDeeplyNested =
  let
    span_ =
      Span.addRegisteredDataAt
        "really.deeply.nested.path"
        ("deeplyNestedValue" :: String)
        entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddStringDeeplyNested"
      (SpanData [
        SpanData.objectAnnotation "really" [
          SpanData.objectAnnotation "deeply" [
            SpanData.objectAnnotation "nested" [
              SpanData.simpleAnnotation "path" ("deeplyNestedValue" :: String)
            ]
          ]
        ]
      ])
      spanData


shouldAddIntDeeplyNested :: Test
shouldAddIntDeeplyNested =
  let
    span_ =
      Span.addRegisteredDataAt
        "really.deeply.nested.path"
        (42 :: Int) $
          entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddIntDeeplyNested"
      (SpanData [
        SpanData.objectAnnotation "really" [
          SpanData.objectAnnotation "deeply" [
            SpanData.objectAnnotation "nested" [
              SpanData.simpleAnnotation "path" (42 :: Int)
            ]
          ]
        ]
      ])
      spanData


shouldAddDoubleDeeplyNested :: Test
shouldAddDoubleDeeplyNested =
  let
    span_ =
      Span.addRegisteredDataAt
        "really.deeply.nested.path"
        (28.08 :: Double) $
          entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddDoubleDeeplyNested"
      (SpanData [
        SpanData.objectAnnotation "really" [
          SpanData.objectAnnotation "deeply" [
            SpanData.objectAnnotation "nested" [
              SpanData.simpleAnnotation "path" (28.08 :: Double)
            ]
          ]
        ]
      ])
      spanData


shouldAddBooleanDeeplyNested :: Test
shouldAddBooleanDeeplyNested =
  let
    span_ = Span.addRegisteredDataAt "really.deeply.nested.path" True $ entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddBooleanDeeplyNested"
      (SpanData [
        SpanData.objectAnnotation "really" [
          SpanData.objectAnnotation "deeply" [
            SpanData.objectAnnotation "nested" [
              SpanData.simpleAnnotation "path" True
            ]
          ]
        ]
      ])
      spanData


shouldAddTwoAtDifferentPaths :: Test
shouldAddTwoAtDifferentPaths =
  let
    span_ =
      Span.addRegisteredDataAt "nested2.key" (2 :: Int) $
      Span.addRegisteredDataAt "nested1.key" (1 :: Int) $
      entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddTwoAtDifferentPaths"
      (SpanData
        [ SpanData.objectAnnotation "nested1"
          [ SpanData.simpleAnnotation "key" (1 :: Int) ]
        , SpanData.objectAnnotation "nested2"
          [ SpanData.simpleAnnotation "key" (2 :: Int) ]
        ]
      )
      spanData


shouldAddTwoAtSamePath :: Test
shouldAddTwoAtSamePath =
  let
    span_ =
      Span.addRegisteredDataAt "nested.key2" (2 :: Int) $
      Span.addRegisteredDataAt "nested.key1" (1 :: Int) $
      entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddTwoAtSamePath"
      (SpanData
        [ SpanData.objectAnnotation "nested"
          [ SpanData.simpleAnnotation "key1" (1 :: Int)
          , SpanData.simpleAnnotation "key2" (2 :: Int)
          ]
        ]
      )
      spanData


shouldAddTwoAtSameNestedPath :: Test
shouldAddTwoAtSameNestedPath =
  let
    span_ =
      Span.addRegisteredDataAt "nested.deeper.key2" (2 :: Int) $
      Span.addRegisteredDataAt "nested.deeper.key1" (1 :: Int) $
      entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddTwoAtSameNestedPath"
      (SpanData
        [ SpanData.objectAnnotation "nested"
          [ SpanData.objectAnnotation "deeper"
            [ SpanData.simpleAnnotation "key1" (1 :: Int)
            , SpanData.simpleAnnotation "key2" (2 :: Int)
            ]
          ]
        ]
      )
      spanData


shouldAddMultipleRegistered :: Test
shouldAddMultipleRegistered =
  let
    span_ =
      Span.addRegisteredDataAt "nested.key3" (12.07 :: Double) $
      Span.addRegisteredDataAt "nested.key2" (2 :: Int) $
      Span.addRegisteredDataAt "nested.key1" ("value.n.1" :: String) $
      Span.addRegisteredDataAt "list" (["one", "two", "three"] :: [String]) $
      Span.addRegisteredDataAt "key3" (16.04 :: Double) $
      Span.addRegisteredDataAt "key2" (13 :: Int) $
      Span.addRegisteredDataAt "key1" ("value1" :: String) $
      entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddMultipleRegistered"
      (SpanData
        [ SpanData.simpleAnnotation "key1" ("value1" :: String)
        , SpanData.simpleAnnotation "key2" (13 :: Int)
        , SpanData.simpleAnnotation "key3" (16.04 :: Double)
        , SpanData.simpleAnnotation "list" (["one", "two", "three"] :: [String])
        , SpanData.objectAnnotation "nested"
          [ SpanData.simpleAnnotation "key1" ("value.n.1" :: String)
          , SpanData.simpleAnnotation "key2" (2 :: Int)
          , SpanData.simpleAnnotation "key3" (12.07 :: Double)
          ]
        ]
      )
      spanData


shouldMergeListValues :: Test
shouldMergeListValues =
  let
    span_ =
      Span.addRegisteredValueAt "list"
        (SpanData.listValue (["three", "four"] :: [String])) $
          Span.addRegisteredValueAt "list"
            (SpanData.listValue (["one", "two"] :: [String])) $
              entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldMergeListValues"
      (SpanData
        [ SpanData.listAnnotation
            "list"
            (["one", "two", "three", "four"] :: [String])
        ]
      )
      spanData


shouldAddMultipleTags :: Test
shouldAddMultipleTags =
  let
    span_ =
      Span.addTagAt "nested.key3" (12.07 :: Double) $
      Span.addTagAt "nested.key2" (2 :: Int) $
      Span.addTagAt "nested.key1" ("value.n.1" :: String) $
      Span.addTagAt "key3" (16.04 :: Double) $
      Span.addTagAt "key2" (13 :: Int) $
      Span.addTagAt "key1" ("value1" :: String) $
      entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "shouldAddMultipleTags"
      (SpanData
        [ SpanData.objectAnnotation "sdk"
          [ SpanData.objectAnnotation "custom"
            [ SpanData.objectAnnotation "tags"
              [ SpanData.simpleAnnotation "key1" ("value1" :: String)
              , SpanData.simpleAnnotation "key2" (13 :: Int)
              , SpanData.simpleAnnotation "key3" (16.04 :: Double)
              , SpanData.objectAnnotation "nested"
                [ SpanData.simpleAnnotation "key1" ("value.n.1" :: String)
                , SpanData.simpleAnnotation "key2" (2 :: Int)
                , SpanData.simpleAnnotation "key3" (12.07 :: Double)
                ]
              ]
            ]
          ]
        ]
      )
      spanData


shouldConvertToSimpleSpanFormat :: Test
shouldConvertToSimpleSpanFormat =
  let
    span_ =
      Span.addRegisteredDataAt "really.deeply.nested.path" True entrySpan
    simple = SimpleSpan.convert span_
    traceId = SimpleSpan.traceId simple
    spanId = SimpleSpan.spanId simple
    parentId = SimpleSpan.parentId simple
    spanName = SimpleSpan.spanName simple
    timestamp = SimpleSpan.timestamp simple
    kind = SimpleSpan.kind simple
    errorCount = SimpleSpan.errorCount simple
    spanData = SimpleSpan.spanData simple
  in
  TestCase $ do
    assertEqual "traceId" "traceId" traceId
    assertEqual "spanId" "traceId" spanId
    assertEqual "parentId" Nothing parentId
    assertEqual "spanName" "test.entry" spanName
    assertEqual "timestamp" 1514761200000 timestamp
    assertEqual "kind" EntryKind kind
    assertEqual "errorCount" 0 errorCount
    assertEqual
      "spanData"
      (SpanData [
        SpanData.objectAnnotation "really" [
          SpanData.objectAnnotation"deeply" [
            SpanData.objectAnnotation "nested" [
              SpanData.simpleAnnotation "path" True
            ]
          ]
        ]
      ])
      spanData


entrySpan :: Span
entrySpan =
  Entry $
    RootEntrySpan $
      RootEntry
        { RootEntry.spanAndTraceId  = Id.fromString "traceId"
        , RootEntry.spanName        = "test.entry"
        , RootEntry.timestamp       = 1514761200000
        , RootEntry.errorCount      = 0
        , RootEntry.serviceName     = Nothing
        , RootEntry.synthetic       = False
        , RootEntry.correlationType = Nothing
        , RootEntry.correlationId   = Nothing
        , RootEntry.spanData        = SpanData.empty
        , RootEntry.w3cTraceContext = Nothing
        }

