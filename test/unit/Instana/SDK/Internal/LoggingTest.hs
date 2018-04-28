module Instana.SDK.Internal.LoggingTest (allTests) where


import           Data.Maybe                   (isNothing)
import           System.Log.Logger            (Priority (..))
import           Test.HUnit

import qualified Instana.SDK.Internal.Logging as Logging


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldParseDebug" shouldParseDebug
    , TestLabel "shouldParseInfo" shouldParseInfo
    , TestLabel "shouldParseNotice" shouldParseNotice
    , TestLabel "shouldParseWarning" shouldParseWarning
    , TestLabel "shouldParseError" shouldParseError
    , TestLabel "shouldParseCritical" shouldParseCritical
    , TestLabel "shouldParseAlert" shouldParseAlert
    , TestLabel "shouldParseEmergency" shouldParseEmergency
    , TestLabel "shouldParseEmpty" shouldParseEmpty
    , TestLabel "shouldParseInvalid" shouldParseInvalid
    , TestLabel "minOfTwoNothings" minOfTwoNothings
    , TestLabel "minRightJust" minRightJust
    , TestLabel "minLeftJust" minLeftJust
    , TestLabel "minDebugWarning" minDebugWarning
    , TestLabel "minWarningDebug" minWarningDebug
    ]


shouldParseDebug :: Test
shouldParseDebug =
  let
    parsed = Logging.parseLogLevel "DEBUG"
  in
    TestCase $ assertEqual "parsed level" (Just DEBUG) parsed


shouldParseInfo :: Test
shouldParseInfo =
  let
    parsed = Logging.parseLogLevel "INFO"
  in
    TestCase $ assertEqual "parsed level" (Just INFO) parsed


shouldParseNotice :: Test
shouldParseNotice =
  let
    parsed = Logging.parseLogLevel "NOTICE"
  in
    TestCase $ assertEqual "parsed level" (Just NOTICE) parsed


shouldParseWarning :: Test
shouldParseWarning =
  let
    parsed = Logging.parseLogLevel "WARNING"
  in
    TestCase $ assertEqual "parsed level" (Just WARNING) parsed


shouldParseError :: Test
shouldParseError =
  let
    parsed = Logging.parseLogLevel "ERROR"
  in
    TestCase $ assertEqual "parsed level" (Just ERROR) parsed


shouldParseCritical :: Test
shouldParseCritical =
  let
    parsed = Logging.parseLogLevel "CRITICAL"
  in
    TestCase $ assertEqual "parsed level" (Just CRITICAL) parsed


shouldParseAlert :: Test
shouldParseAlert =
  let
    parsed = Logging.parseLogLevel "ALERT"
  in
    TestCase $ assertEqual "parsed level" (Just ALERT) parsed


shouldParseEmergency :: Test
shouldParseEmergency =
  let
    parsed = Logging.parseLogLevel "EMERGENCY"
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

