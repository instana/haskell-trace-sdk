{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.SpanTest (allTests) where


import           Data.Aeson                 (Value, (.=))
import qualified Data.Aeson                 as Aeson
import           Test.HUnit

import qualified Instana.SDK.Internal.Id    as Id
import           Instana.SDK.Span.EntrySpan (EntrySpan (RootEntrySpan))
import           Instana.SDK.Span.RootEntry (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry as RootEntry
import           Instana.SDK.Span.Span      (Span (..))
import qualified Instana.SDK.Span.Span      as Span


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldAddStringNotNested" shouldAddStringNotNested
    , TestLabel "shouldAddStringNested" shouldAddStringNested
    , TestLabel "shouldAddStringDeeplyNested" shouldAddStringDeeplyNested
    , TestLabel "shouldAddIntDeeplyNested" shouldAddIntDeeplyNested
    , TestLabel "shouldAddDoubleDeeplyNested" shouldAddDoubleDeeplyNested
    , TestLabel "shouldAddBooleanDeeplyNested" shouldAddBooleanDeeplyNested
    , TestLabel "shouldAddMultipleRegistered" shouldAddMultipleRegistered
    , TestLabel "shouldAddMultipleTags" shouldAddMultipleTags
    ]


shouldAddStringNotNested :: Test
shouldAddStringNotNested =
  let
    span_ = Span.addRegisteredDataAt "path" ("value" :: String) $ entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "add non nested"
      (Aeson.object [
        "path" .= ("value" :: String)
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
    assertEqual "add nested"
      (Aeson.object [
        "nested" .= (Aeson.object [
          "path" .= ("value" :: String)
        ])
      ])
      spanData


shouldAddStringDeeplyNested :: Test
shouldAddStringDeeplyNested =
  let
    span_ =
      Span.addRegisteredDataAt
        "really.deeply.nested.path"
        ("deeplyNestedValue" :: String) $
          entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "add deeply nested"
      (Aeson.object [
        "really" .= (Aeson.object [
          "deeply" .= (Aeson.object [
            "nested" .= (Aeson.object [
              "path" .= ("deeplyNestedValue" :: String)
            ])
          ])
        ])
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
    assertEqual "add deeply nested"
      (Aeson.object [
        "really" .= (Aeson.object [
          "deeply" .= (Aeson.object [
            "nested" .= (Aeson.object [
              "path" .= (42 :: Int)
            ])
          ])
        ])
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
    assertEqual "add deeply nested"
      (Aeson.object [
        "really" .= (Aeson.object [
          "deeply" .= (Aeson.object [
            "nested" .= (Aeson.object [
              "path" .= (28.08 :: Double)
            ])
          ])
        ])
      ])
      spanData


shouldAddBooleanDeeplyNested :: Test
shouldAddBooleanDeeplyNested =
  let
    span_ = Span.addRegisteredDataAt "really.deeply.nested.path" True $ entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "add deeply nested"
      (Aeson.object [
        "really" .= (Aeson.object [
          "deeply" .= (Aeson.object [
            "nested" .= (Aeson.object [
              "path" .= True
            ])
          ])
        ])
      ])
      spanData


shouldAddMultipleRegistered :: Test
shouldAddMultipleRegistered =
  let
    span_ =
      Span.addRegisteredDataAt "nested.key3" (12.07 :: Double) $
      Span.addRegisteredDataAt "nested.key2" (2 :: Int) $
      Span.addRegisteredDataAt "nested.key1" ("value.n.1" :: String) $
      Span.addRegisteredDataAt "key3" (16.04 :: Double) $
      Span.addRegisteredDataAt "key2" (13 :: Int) $
      Span.addRegisteredDataAt "key1" ("value1" :: String) $
      entrySpan
    spanData = Span.spanData span_
  in
  TestCase $
    assertEqual "add deeply nested"
      (Aeson.object
        [ "key1" .= ("value1" :: String)
        , "key2" .= (13 :: Int)
        , "key3" .= (16.04 :: Double)
        , "nested" .= (Aeson.object
          [ "key1" .= ("value.n.1" :: String)
          , "key2" .= (2 :: Int)
          , "key3" .= (12.07 :: Double)
          ])
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
    assertEqual "add deeply nested"
      (Aeson.object
        [ "sdk" .= (Aeson.object
          [ "custom" .= (Aeson.object
            [ "tags" .= (Aeson.object
              [ "key1" .= ("value1" :: String)
              , "key2" .= (13 :: Int)
              , "key3" .= (16.04 :: Double)
              , "nested" .= (Aeson.object
                [ "key1" .= ("value.n.1" :: String)
                , "key2" .= (2 :: Int)
                , "key3" .= (12.07 :: Double)
                ])
              ])
            ])
          ])
        ]
      )
      spanData


entrySpan :: Span
entrySpan =
  Entry $
    RootEntrySpan $
      RootEntry
        { RootEntry.spanAndTraceId = Id.fromString "traceId"
        , RootEntry.spanName       = "test.entry"
        , RootEntry.timestamp      = 1514761200000
        , RootEntry.spanData       = initialData
        , RootEntry.errorCount     = 0
        }


initialData :: Value
initialData = Aeson.object []

