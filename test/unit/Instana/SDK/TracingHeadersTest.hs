{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.TracingHeadersTest (allTests) where


import qualified Data.CaseInsensitive       as CI
import qualified Network.Wai                as Wai
import           Test.HUnit

import           Instana.SDK.TracingHeaders (TracingLevel (..))
import qualified Instana.SDK.TracingHeaders as TracingHeaders


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldReadHeaders" shouldReadHeaders
    , TestLabel
        "shouldReadHeaderNamesCaseInsensitive"
        shouldReadHeaderNamesCaseInsensitive
    , TestLabel "shouldReadDefaultValues" shouldReadDefaultValues
    , TestLabel
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


shouldReadDefaultValues :: Test
shouldReadDefaultValues =
  let
    tracingHeaders =
      TracingHeaders.readHttpTracingHeaders Wai.defaultRequest
  in
  TestCase $ do
    assertEqual "trace ID"
      Nothing
      (TracingHeaders.traceId tracingHeaders)
    assertEqual "span ID"
      Nothing
      (TracingHeaders.spanId tracingHeaders)
    assertEqual "level" Trace (TracingHeaders.level tracingHeaders)
    assertEqual "correlationType"
      Nothing
      (TracingHeaders.correlationType tracingHeaders)
    assertEqual "correlationId"
      Nothing
      (TracingHeaders.correlationId tracingHeaders)
    assertEqual "synthetic"
      False
      (TracingHeaders.synthetic tracingHeaders)
    assertEqual "traceparent"
      Nothing
      (TracingHeaders.traceparent tracingHeaders)
    assertEqual "tracestate"
      Nothing
      (TracingHeaders.tracestate tracingHeaders)


shouldReadHeaders :: Test
shouldReadHeaders =
  let
    tracingHeaders =
      TracingHeaders.readHttpTracingHeaders $
        Wai.defaultRequest
          { Wai.requestHeaders =
              [ (CI.mk "X-INSTANA-T", "1234567890abcdef")
              , (CI.mk "X-INSTANA-S", "abcdef1234567890")
              , (CI.mk "X-INSTANA-L", "1")
              , (CI.mk "X-INSTANA-SYNTHETIC", "1")
              , (CI.mk "traceparent",
                  "00-00000000000000001234567890abcdef-abcdef1234567890-01"
                )
              , (CI.mk "tracestate",
                  "foo=bar,in=1234567890abcdef;abcdef1234567890,beep=bop"
                )
              ]
          }
  in
  TestCase $ do
    assertEqual "trace ID"
      (Just "1234567890abcdef")
      (TracingHeaders.traceId tracingHeaders)
    assertEqual "span ID"
      (Just "abcdef1234567890")
      (TracingHeaders.spanId tracingHeaders)
    assertEqual "level" Trace (TracingHeaders.level tracingHeaders)
    assertEqual "correlationType"
      Nothing
      (TracingHeaders.correlationType tracingHeaders)
    assertEqual "correlationId"
      Nothing
      (TracingHeaders.correlationId tracingHeaders)
    assertEqual "synthetic"
      True
      (TracingHeaders.synthetic tracingHeaders)
    assertEqual "traceparent"
      (Just "00-00000000000000001234567890abcdef-abcdef1234567890-01")
      (TracingHeaders.traceparent tracingHeaders)
    assertEqual "tracestate"
      (Just "foo=bar,in=1234567890abcdef;abcdef1234567890,beep=bop")
      (TracingHeaders.tracestate tracingHeaders)


shouldReadHeaderNamesCaseInsensitive :: Test
shouldReadHeaderNamesCaseInsensitive =
  let
    tracingHeaders =
      TracingHeaders.readHttpTracingHeaders $
        Wai.defaultRequest
          { Wai.requestHeaders =
              [ (CI.mk "x-inSTanA-t", "1234567890abcdef")
              , (CI.mk "X-Instana-S", "abcdef1234567890")
              , (CI.mk "x-instana-synthetic", "1")
              , (CI.mk "TRACEPARENT",
                  "00-00000000000000001234567890abcdef-abcdef1234567890-01"
                )
              , (CI.mk "TRACESTATE",
                  "foo=bar,in=1234567890abcdef;abcdef1234567890,beep=bop"
                )
              ]
          }
  in
  TestCase $ do
    assertEqual "trace ID"
      (Just "1234567890abcdef")
      (TracingHeaders.traceId tracingHeaders)
    assertEqual "span ID"
      (Just "abcdef1234567890")
      (TracingHeaders.spanId tracingHeaders)
    assertEqual "level" Trace (TracingHeaders.level tracingHeaders)
    assertEqual "correlationType"
      Nothing
      (TracingHeaders.correlationType tracingHeaders)
    assertEqual "correlationId"
      Nothing
      (TracingHeaders.correlationId tracingHeaders)
    assertEqual "synthetic"
      True
      (TracingHeaders.synthetic tracingHeaders)
    assertEqual "traceparent"
      (Just "00-00000000000000001234567890abcdef-abcdef1234567890-01")
      (TracingHeaders.traceparent tracingHeaders)
    assertEqual "tracestate"
      (Just "foo=bar,in=1234567890abcdef;abcdef1234567890,beep=bop")
      (TracingHeaders.tracestate tracingHeaders)




shouldParseNonExistingXInstanaL :: Test
shouldParseNonExistingXInstanaL =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL Nothing
  in
  TestCase $ do
    assertEqual "level" Trace level
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL1 :: Test
shouldParseXInstanaL1 =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "1"
  in
  TestCase $ do
    assertEqual "level" Trace level
    assertEqual "correlation type" Nothing correlationType
    assertEqual "correlation ID" Nothing correlationId


shouldParseXInstanaL1Untrimmed :: Test
shouldParseXInstanaL1Untrimmed =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "  1  "
  in
  TestCase $ do
    assertEqual "level" Trace level
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
    assertEqual "level" Trace level
    assertEqual "correlation type" (Just "web") correlationType
    assertEqual "correlation ID" (Just "1234567890abcdef") correlationId


shouldParseXInstanaL1WithOddlyFormattedCorrelation :: Test
shouldParseXInstanaL1WithOddlyFormattedCorrelation =
  let
    (level, correlationType, correlationId) =
      TracingHeaders.parseXInstanaL $ Just "  1 ,  correlationType = web  ;  correlationId =  1234567890abcdef"
  in
  TestCase $ do
    assertEqual "level" Trace level
    assertEqual "correlation type" (Just "web") correlationType
    assertEqual "correlation ID" (Just "1234567890abcdef") correlationId

