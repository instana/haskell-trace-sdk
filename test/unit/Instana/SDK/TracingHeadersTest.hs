{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.TracingHeadersTest (allTests) where


import           Test.HUnit

import           Instana.SDK.TracingHeaders (TracingLevel (..))
import qualified Instana.SDK.TracingHeaders as TracingHeaders


allTests :: Test
allTests =
  TestList
    [ TestLabel
        "shouldParseNonExistingXInstanaL"
        shouldParseNonExistingXInstanaL
    , TestLabel "shouldParseXInstanaL1" shouldParseXInstanaL1
    , TestLabel "shouldParseXInstanaL1Untrimmed" shouldParseXInstanaL1Untrimmed
    , TestLabel "shouldParseXInstanaL0" shouldParseXInstanaL0
    , TestLabel "shouldParseXInstanaL0Untrimmed" shouldParseXInstanaL0Untrimmed
    , TestLabel
        "shouldParseXInstanaL0AndIgnoreCorrelation"
        shouldParseXInstanaL0AndIgnoreCorrelation
    , TestLabel
        "shouldParseXInstanaL1WithCorrelation"
        shouldParseXInstanaL1WithCorrelation
    , TestLabel
        "shouldParseXInstanaL1WithOddlyFormattedCorrelation"
        shouldParseXInstanaL1WithOddlyFormattedCorrelation
    ]


shouldParseNonExistingXInstanaL :: Test
shouldParseNonExistingXInstanaL =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL Nothing
  in
  TestCase $ do
    assertEqual "level" level Trace
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL1 :: Test
shouldParseXInstanaL1 =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "1"
  in
  TestCase $ do
    assertEqual "level" level Trace
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL1Untrimmed :: Test
shouldParseXInstanaL1Untrimmed =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "  1  "
  in
  TestCase $ do
    assertEqual "level" level Trace
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL0 :: Test
shouldParseXInstanaL0 =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "0"
  in
  TestCase $ do
    assertEqual "level" level Suppress
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL0Untrimmed :: Test
shouldParseXInstanaL0Untrimmed =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "  0  "
  in
  TestCase $ do
    assertEqual "level" level Suppress
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL0AndIgnoreCorrelation :: Test
shouldParseXInstanaL0AndIgnoreCorrelation =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just " 0 ,  correlationType=web; correlationId=1234567890abcdef"
  in
  TestCase $ do
    assertEqual "level" level Suppress
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL1WithCorrelation :: Test
shouldParseXInstanaL1WithCorrelation =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "1,correlationType=web;correlationId=1234567890abcdef"
  in
  TestCase $ do
    assertEqual "level" level Trace
    assertEqual "correlation type" (Just "web") correlationType
    assertEqual "correlation ID" (Just "1234567890abcdef") correlationId


shouldParseXInstanaL1WithOddlyFormattedCorrelation :: Test
shouldParseXInstanaL1WithOddlyFormattedCorrelation =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "  1 ,  correlationType = web  ;  correlationId =  1234567890abcdef"
  in
  TestCase $ do
    assertEqual "level" level Trace
    assertEqual "correlation type" (Just "web") correlationType
    assertEqual "correlation ID" (Just "1234567890abcdef") correlationId


