{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.ServerTimingTest (allTests) where

import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as BSC8
import qualified Network.HTTP.Types                as HTTP
import           Test.HUnit

import qualified Instana.SDK.Internal.Id           as Id
import qualified Instana.SDK.Internal.ServerTiming as ServerTiming


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldAddToEmptyHeaders" shouldAddToEmptyHeaders
    , TestLabel "shouldAddToNonEmptyHeaders" shouldAddToNonEmptyHeaders
    , TestLabel
        "shouldAppendToExistingServerTiming"
        shouldAppendToExistingServerTiming
    , TestLabel
        "shouldAppendToExistingServerTimingAnotherHeaderPresent"
        shouldAppendToExistingServerTimingAnotherHeaderPresent
    , TestLabel
        "shouldReplaceExistingInTIdAtStart"
        shouldReplaceExistingInTIdAtStart
    , TestLabel
        "shouldReplaceExistingInTIdInTheMiddle"
        shouldReplaceExistingInTIdInTheMiddle
    , TestLabel
        "shouldReplaceExistingInTIdAtTheEnd"
        shouldReplaceExistingInTIdAtTheEnd
    ]


shouldAddToEmptyHeaders :: Test
shouldAddToEmptyHeaders =
  checkHeader
    "add to empty headers"
    "trace-id"
    []
    (BSC8.pack "intid;desc=trace-id")
    1


shouldAddToNonEmptyHeaders :: Test
shouldAddToNonEmptyHeaders =
  checkHeader
    "add to non empty headers without existing Server-Timing"
    "trace-id"
    [("Content-Type", "text/plain")]
    (BSC8.pack "intid;desc=trace-id")
    2


shouldAppendToExistingServerTiming :: Test
shouldAppendToExistingServerTiming =
  checkHeader
    "append to existing Server-Timing"
    "trace-id"
    [("Server-Timing", "foo;dur=23.2")]
    (BSC8.pack "foo;dur=23.2, intid;desc=trace-id")
    1


shouldAppendToExistingServerTimingAnotherHeaderPresent :: Test
shouldAppendToExistingServerTimingAnotherHeaderPresent =
  checkHeader
    "append to existing Server-Timing (with another header being present)"
    "trace-id"
    [("Content-Type", "text/plain"), ("Server-Timing", "foo;dur=23.2")]
    (BSC8.pack "foo;dur=23.2, intid;desc=trace-id")
    2


shouldReplaceExistingInTIdAtStart :: Test
shouldReplaceExistingInTIdAtStart =
  checkHeader
    "replace existing intid metric at the start of the Server-Timing value"
    "trace-id"
    [
      ("Server-Timing", "intid;desc=other-trace-id, foo;dur=23.2, bar;dur=1302"),
      ("Content-Type", "text/plain")
    ]
    (BSC8.pack "intid;desc=trace-id, foo;dur=23.2, bar;dur=1302")
    2


shouldReplaceExistingInTIdInTheMiddle :: Test
shouldReplaceExistingInTIdInTheMiddle =
  checkHeader
    "replace existing intid metric in the middle of the Server-Timing value"
    "trace-id"
    [
      ("Server-Timing", "foo;dur=23.2, intid;desc=other-trace-id, bar;dur=1302"),
      ("Content-Type", "text/plain")
    ]
    (BSC8.pack "foo;dur=23.2, intid;desc=trace-id, bar;dur=1302")
    2


shouldReplaceExistingInTIdAtTheEnd :: Test
shouldReplaceExistingInTIdAtTheEnd =
  checkHeader
    "replace existing intid metric at the end of the Server-Timing value"
    "trace-id"
    [
      ("Server-Timing", "foo;dur=23.2, bar;dur=1302, intid;desc=other-trace-id"),
      ("Content-Type", "text/plain")
    ]
    (BSC8.pack "foo;dur=23.2, bar;dur=1302, intid;desc=trace-id")
    2


checkHeader ::
  String
  -> String
  -> HTTP.ResponseHeaders
  -> ByteString
  -> Int
  -> Test
checkHeader label traceId originalHeaders expected numberOfExpectedHeaders =
  let
    resultingHeaders =
      ServerTiming.addTraceIdToServerTiming
        (Id.fromString traceId)
        originalHeaders
    actual = lookup "Server-Timing" resultingHeaders
    justExpected = Just expected
  in
  TestCase $ do
    assertEqual
      label
      justExpected
      actual
    assertEqual
      label
      numberOfExpectedHeaders
      (length resultingHeaders)

