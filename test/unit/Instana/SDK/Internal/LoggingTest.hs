module Instana.SDK.Internal.LoggingTest (allTests) where


import           Data.Maybe                   (isNothing)
import           System.Log.Logger            (Priority (..))
import           Test.HUnit

import qualified Instana.SDK.Internal.Logging as Logging


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldParseDebugUpperCase" shouldParseDebugUpperCase
    , TestLabel "shouldParseDebugLowerCase" shouldParseDebugLowerCase
    , TestLabel "shouldParseInfoUpperCase" shouldParseInfoUpperCase
    , TestLabel "shouldParseInfoLowerCase" shouldParseInfoLowerCase
    , TestLabel "shouldParseNoticeUpperCase" shouldParseNoticeUpperCase
    , TestLabel "shouldParseNoticeLowerCase" shouldParseNoticeLowerCase
    , TestLabel "shouldParseWarningUpperCase" shouldParseWarningUpperCase
    , TestLabel "shouldParseWarningLowerCase" shouldParseWarningLowerCase
    , TestLabel "shouldParseErrorUpperCase" shouldParseErrorUpperCase
    , TestLabel "shouldParseErrorLowerCase" shouldParseErrorLowerCase
    , TestLabel "shouldParseCriticalUpperCase" shouldParseCriticalUpperCase
    , TestLabel "shouldParseCriticalLowerCase" shouldParseCriticalLowerCase
    , TestLabel "shouldParseAlertUpperCase" shouldParseAlertUpperCase
    , TestLabel "shouldParseAlertLowerCase" shouldParseAlertLowerCase
    , TestLabel "shouldParseEmergencyUpperCase" shouldParseEmergencyUpperCase
    , TestLabel "shouldParseEmergencyLowerCase" shouldParseEmergencyLowerCase
    , TestLabel "shouldParseEmpty" shouldParseEmpty
    , TestLabel "shouldParseInvalid" shouldParseInvalid
    , TestLabel "minOfTwoNothings" minOfTwoNothings
    , TestLabel "minRightJust" minRightJust
    , TestLabel "minLeftJust" minLeftJust
    , TestLabel "minDebugWarning" minDebugWarning
    , TestLabel "minWarningDebug" minWarningDebug
    ]


shouldParseDebugUpperCase :: Test
shouldParseDebugUpperCase =
  let
    parsed = Logging.parseLogLevel "DEBUG"
  in
    TestCase $ assertEqual "parsed level" (Just DEBUG) parsed


shouldParseDebugLowerCase :: Test
shouldParseDebugLowerCase =
  let
    parsed = Logging.parseLogLevel "debug"
  in
    TestCase $ assertEqual "parsed level" (Just DEBUG) parsed



shouldParseInfoUpperCase :: Test
shouldParseInfoUpperCase =
  let
    parsed = Logging.parseLogLevel "INFO"
  in
    TestCase $ assertEqual "parsed level" (Just INFO) parsed


shouldParseInfoLowerCase :: Test
shouldParseInfoLowerCase =
  let
    parsed = Logging.parseLogLevel "info"
  in
    TestCase $ assertEqual "parsed level" (Just INFO) parsed


shouldParseNoticeUpperCase :: Test
shouldParseNoticeUpperCase =
  let
    parsed = Logging.parseLogLevel "NOTICE"
  in
    TestCase $ assertEqual "parsed level" (Just NOTICE) parsed


shouldParseNoticeLowerCase :: Test
shouldParseNoticeLowerCase =
  let
    parsed = Logging.parseLogLevel "notice"
  in
    TestCase $ assertEqual "parsed level" (Just NOTICE) parsed


shouldParseWarningUpperCase :: Test
shouldParseWarningUpperCase =
  let
    parsed = Logging.parseLogLevel "WARNING"
  in
    TestCase $ assertEqual "parsed level" (Just WARNING) parsed


shouldParseWarningLowerCase :: Test
shouldParseWarningLowerCase =
  let
    parsed = Logging.parseLogLevel "warning"
  in
    TestCase $ assertEqual "parsed level" (Just WARNING) parsed


shouldParseErrorUpperCase :: Test
shouldParseErrorUpperCase =
  let
    parsed = Logging.parseLogLevel "ERROR"
  in
    TestCase $ assertEqual "parsed level" (Just ERROR) parsed


shouldParseErrorLowerCase :: Test
shouldParseErrorLowerCase =
  let
    parsed = Logging.parseLogLevel "error"
  in
    TestCase $ assertEqual "parsed level" (Just ERROR) parsed


shouldParseCriticalUpperCase :: Test
shouldParseCriticalUpperCase =
  let
    parsed = Logging.parseLogLevel "CRITICAL"
  in
    TestCase $ assertEqual "parsed level" (Just CRITICAL) parsed


shouldParseCriticalLowerCase :: Test
shouldParseCriticalLowerCase =
  let
    parsed = Logging.parseLogLevel "critical"
  in
    TestCase $ assertEqual "parsed level" (Just CRITICAL) parsed


shouldParseAlertUpperCase :: Test
shouldParseAlertUpperCase =
  let
    parsed = Logging.parseLogLevel "ALERT"
  in
    TestCase $ assertEqual "parsed level" (Just ALERT) parsed


shouldParseAlertLowerCase :: Test
shouldParseAlertLowerCase =
  let
    parsed = Logging.parseLogLevel "alert"
  in
    TestCase $ assertEqual "parsed level" (Just ALERT) parsed


shouldParseEmergencyUpperCase :: Test
shouldParseEmergencyUpperCase =
  let
    parsed = Logging.parseLogLevel "EMERGENCY"
  in
    TestCase $ assertEqual "parsed level" (Just EMERGENCY) parsed


shouldParseEmergencyLowerCase :: Test
shouldParseEmergencyLowerCase =
  let
    parsed = Logging.parseLogLevel "emergency"
  in
    TestCase $ assertEqual "parsed level" (Just EMERGENCY) parsed


shouldParseEmpty :: Test
shouldParseEmpty =
  let
    parsed = Logging.parseLogLevel ""
  in
    TestCase $ assertBool "parsed level" (isNothing parsed)


shouldParseInvalid :: Test
shouldParseInvalid =
  let
    parsed = Logging.parseLogLevel "invalid"
  in
    TestCase $ assertBool "parsed level" (isNothing parsed)


minOfTwoNothings :: Test
minOfTwoNothings =
  TestCase $
    assertBool
      "parsed level"
      (isNothing $ Logging.minimumLogLevel Nothing Nothing)


minRightJust :: Test
minRightJust =
  TestCase $
    assertEqual
      "min"
      (Just DEBUG)
      (Logging.minimumLogLevel Nothing (Just DEBUG))


minLeftJust :: Test
minLeftJust =
  TestCase $
    assertEqual
      "min"
      (Just WARNING)
      (Logging.minimumLogLevel (Just WARNING) Nothing)


minDebugWarning :: Test
minDebugWarning =
  TestCase $
    assertEqual
      "min"
        (Just DEBUG)
        (Logging.minimumLogLevel (Just WARNING) (Just DEBUG))


minWarningDebug :: Test
minWarningDebug =
  TestCase $
    assertEqual
      "min"
      (Just DEBUG)
      (Logging.minimumLogLevel (Just DEBUG) (Just WARNING))

