{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.SpanDataTest (allTests) where


import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Aeson
import           Test.HUnit

import           Instana.SDK.Span.SpanData (SpanData (SpanData))
import qualified Instana.SDK.Span.SpanData as SpanData


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldCreateEmpty" shouldCreateEmpty
    , TestLabel "shouldSerializeNothingToNull" shouldSerializeNothingToNull
    , TestLabel "shouldSerializeToJson" shouldSerializeToJson
    , TestLabel "shouldMergeSimpleAnnotationIntoEmpty" shouldMergeSimpleAnnotationIntoEmpty
    , TestLabel "shouldMergeObjectAnnotationIntoEmpty" shouldMergeObjectAnnotationIntoEmpty
    , TestLabel "shouldMergeObjects" shouldMergeObjects
    , TestLabel "shouldMergeNestedObjects" shouldMergeNestedObjects
    , TestLabel "shouldOverwriteSingleAnnotation" shouldOverwriteSingleAnnotation
    , TestLabel "shouldOverwriteWithSingleAnnotation" shouldOverwriteWithSingleAnnotation
    , TestLabel "shouldMergeTwoAtDifferentPaths" shouldMergeTwoAtDifferentPaths
    , TestLabel "shouldMergeListValues" shouldMergeListValues
    ]


shouldCreateEmpty :: Test
shouldCreateEmpty =
  TestCase $
    assertEqual "shouldCreateEmpty"
      (SpanData []) SpanData.empty


shouldSerializeNothingToNull :: Test
shouldSerializeNothingToNull =
  TestCase $
    assertEqual "shouldSerializeNothingToNull"
      (Aeson.object [])
      (Aeson.toJSON
        (SpanData [SpanData.optionalAnnotation "key" (Nothing :: Maybe String)])
      )


shouldSerializeToJson :: Test
shouldSerializeToJson =
  TestCase $
    assertEqual "shouldSerializeToJson"
      (Aeson.object
        [ "sdk" .= (Aeson.object
          [ "custom" .= (Aeson.object
            [ "tags" .= (Aeson.object
              [ "tag1" .= ("value1" :: String)
              , "tag2" .= (["a", "b"] :: [String])
              , "tag3" .= (16.04 :: Double)
              , "nested" .= (Aeson.object
                [ "nested1" .= ("value.n.1" :: String)
                , "nested2" .= (2 :: Int)
                , "nested3" .= (12.07 :: Double)
                ]
              )]
            )]
          )]
        )
        , "simple" .= ("value" :: String)
        , "list" .= ([1, 2, 3] :: [Int])
        , "optional-exists" .= ("yes" :: String)
        ]
      )
      (Aeson.toJSON
        (SpanData
          [ SpanData.objectAnnotation "sdk"
            [ SpanData.objectAnnotation "custom"
              [ SpanData.objectAnnotation "tags"
                [ SpanData.simpleAnnotation "tag1" ("value1" :: String)
                , SpanData.listAnnotation "tag2" (["a", "b"] :: [String])
                , SpanData.simpleAnnotation "tag3" (16.04 :: Double)
                , SpanData.objectAnnotation "nested"
                  [ SpanData.simpleAnnotation "nested1" ("value.n.1" :: String)
                  , SpanData.simpleAnnotation "nested2" (2 :: Int)
                  , SpanData.simpleAnnotation "nested3" (12.07 :: Double)
                  ]
                ]
              ]
            ]
          , SpanData.simpleAnnotation "simple" ("value" :: String)
          , SpanData.listAnnotation "list" ([1, 2, 3] :: [Int])
          , SpanData.optionalAnnotation "optional-exists" (Just "yes" :: Maybe String)
          , SpanData.optionalAnnotation "optional-does-not-exist" (Nothing :: Maybe String)
          ]
        )
      )


shouldMergeSimpleAnnotationIntoEmpty :: Test
shouldMergeSimpleAnnotationIntoEmpty =
  let
    spanData =
      SpanData.merge
        (SpanData.simpleAnnotation "path" ("value" :: String))
        SpanData.empty
  in
  TestCase $
    assertEqual "shouldMergeSimpleAnnotationIntoEmpty"
      (SpanData [
        SpanData.simpleAnnotation "path" ("value" :: String)
      ])
      spanData


shouldMergeObjectAnnotationIntoEmpty :: Test
shouldMergeObjectAnnotationIntoEmpty =
  let
    spanData =
      SpanData.merge
        (SpanData.objectAnnotation "nested" [
          SpanData.simpleAnnotation "path" ("value" :: String)
        ])
        SpanData.empty
  in
  TestCase $
    assertEqual "shouldMergeObjectAnnotationIntoEmpty"
      (SpanData [
        SpanData.objectAnnotation "nested" [
          SpanData.simpleAnnotation "path" ("value" :: String)
        ]
      ])
      spanData


shouldMergeObjects :: Test
shouldMergeObjects =
  let
    spanData =
      SpanData.merge
        ( SpanData.objectAnnotation "nested"
          [ SpanData.simpleAnnotation "key2" (2 :: Int) ]
        ) $
        SpanData.merge
          ( SpanData.objectAnnotation "nested"
            [ SpanData.simpleAnnotation "key1" (1 :: Int) ]
          )
          SpanData.empty
  in
  TestCase $
    assertEqual "shouldMergeObjects"
      (SpanData
        [ SpanData.objectAnnotation "nested"
          [ SpanData.simpleAnnotation "key1" (1 :: Int)
          , SpanData.simpleAnnotation "key2" (2 :: Int)
          ]
        ]
      )
      spanData


shouldMergeNestedObjects :: Test
shouldMergeNestedObjects =
  let
    spanData =
      SpanData.merge
        ( SpanData.objectAnnotation "nested"
          [ SpanData.objectAnnotation "deeper"
            [ SpanData.simpleAnnotation "key2" (2 :: Int) ]
          ]
        ) $
        SpanData.merge
          ( SpanData.objectAnnotation "nested"
            [ SpanData.objectAnnotation "deeper"
              [ SpanData.simpleAnnotation "key1" (1 :: Int) ]
            ]
          )
          SpanData.empty
  in
  TestCase $
    assertEqual "shouldMergeNestedObjects"
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


shouldOverwriteSingleAnnotation :: Test
shouldOverwriteSingleAnnotation =
  let
    spanData =
      SpanData.merge
        (SpanData.objectAnnotation "key"
            [ SpanData.simpleAnnotation "value" (2 :: Int) ]
        )
        (SpanData [ SpanData.simpleAnnotation "key" (1 :: Int) ])
  in
  TestCase $
    assertEqual "shouldOverwriteSingleAnnotation"
      (SpanData
        [ SpanData.objectAnnotation "key"
          [ SpanData.simpleAnnotation "value" (2 :: Int) ]
        ]
      )
      spanData


shouldOverwriteWithSingleAnnotation :: Test
shouldOverwriteWithSingleAnnotation =
  let
    spanData =
      SpanData.merge
        (SpanData.simpleAnnotation "key" (2 :: Int))
        (SpanData
          [ SpanData.objectAnnotation "key"
            [ SpanData.simpleAnnotation "value" (1 :: Int) ]
          ]
        )

  in
  TestCase $
    assertEqual "shouldOverwriteSingleAnnotation"
      (SpanData [ SpanData.simpleAnnotation "key"  (2 :: Int) ])
      spanData


shouldMergeTwoAtDifferentPaths :: Test
shouldMergeTwoAtDifferentPaths =
  let
    spanData =
      SpanData.merge
        (SpanData.objectAnnotation "nested2"
          [ SpanData.simpleAnnotation "key" (2 :: Int) ]) $
        SpanData.merge
          (SpanData.objectAnnotation "nested1"
            [ SpanData.simpleAnnotation "key" (1 :: Int) ])
          SpanData.empty
  in
  TestCase $
    assertEqual "shouldMergeTwoAtDifferentPaths"
      (SpanData
        [ SpanData.objectAnnotation "nested1"
          [ SpanData.simpleAnnotation "key" (1 :: Int) ]
        , SpanData.objectAnnotation "nested2"
          [ SpanData.simpleAnnotation "key" (2 :: Int) ]
        ]
      )
      spanData


shouldMergeListValues :: Test
shouldMergeListValues =
  let
    spanData =
      SpanData.merge
        (SpanData.listAnnotation "list" ([ "three", "four" ] :: [String])) $
        SpanData.merge
          (SpanData.listAnnotation "list" ([ "one", "two" ] :: [String]))
          SpanData.empty
  in
  TestCase $
    assertEqual "shouldMergeListValues"
      (SpanData
        [ SpanData.listAnnotation "list" (["one", "two", "three", "four"] :: [String]) ]
      )
      spanData

