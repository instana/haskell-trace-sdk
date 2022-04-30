{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.HttpTracing (allTests, runHttpTest) where


import           Control.Concurrent                     (threadDelay)
import           Data.Aeson                             ((.=))
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Char8                  as BSC8
import qualified Data.ByteString.Lazy.Char8             as LBSC8
import           Data.List                              (isInfixOf)
import qualified Data.List                              as List
import           Data.Maybe                             (isNothing, listToMaybe)
import           Instana.SDK.AgentStub.TraceRequest     (From (..), Span)
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import qualified Instana.SDK.IntegrationTest.Suite      as Suite
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import qualified Network.HTTP.Client                    as HTTP
import           Network.HTTP.Types                     (Header)
import qualified Network.HTTP.Types.Header
import           Test.HUnit


allTests :: String -> [IO Test]
allTests pid =
  [ shouldCreateRootEntryWithBracketApi pid
  , shouldAddWebsiteMonitoringCorrelationWithBracketApi pid
  , shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithBracketApi pid
  , shouldCreateNonRootEntryWithBracketApi pid
  , shouldSetSpanSyWithBracketApi pid
  , shouldSuppressWithBracketApi
  , shouldRedactDefaultSecrets pid
  , shouldCreateRootEntryWithLowLevelApi pid
  , shouldAddWebsiteMonitoringCorrelationWithLowLevelApi pid
  , shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithLowLevelApi pid
  , shouldCreateNonRootEntryWithLowLevelApi pid
  , shouldSetSpanSyWithLowLevelApi pid
  , shouldSuppressWithLowLevelApi
  ]


shouldCreateRootEntryWithBracketApi :: String -> IO Test
shouldCreateRootEntryWithBracketApi pid =
  applyLabel "shouldCreateRootEntryWithBracketApi" $
    runBracketTest
      pid
      []
      (applyConcat [rootEntryAsserts, bracketAsserts, notSyntheticAssert])


shouldAddWebsiteMonitoringCorrelationWithBracketApi :: String -> IO Test
shouldAddWebsiteMonitoringCorrelationWithBracketApi pid =
  applyLabel "shouldAddWebsiteMonitoringCorrelationWithBracketApi" $
    runBracketTest
      pid
      [("X-INSTANA-L", "1,correlationType=web;correlationId=1234567890abcdef")]
      (applyConcat [
          rootEntryAsserts
        , bracketAsserts
        , correlationAsserts
        , notSyntheticAssert
      ])


shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithBracketApi :: String -> IO Test
shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithBracketApi pid =
  applyLabel "shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithBracketApi" $
    runBracketTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      , ("X-INSTANA-L", "1,correlationType=web;correlationId=1234567890abcdef")
      ]
      (applyConcat [
          rootEntryAsserts
        , bracketAsserts
        , correlationAsserts
        , notSyntheticAssert
      ])


shouldCreateNonRootEntryWithBracketApi :: String -> IO Test
shouldCreateNonRootEntryWithBracketApi pid =
  applyLabel "shouldCreateNonRootEntryWithBracketApi" $ do
    runBracketTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      ]
      (applyConcat [nonRootEntryAsserts, bracketAsserts, notSyntheticAssert])


shouldSetSpanSyWithBracketApi :: String -> IO Test
shouldSetSpanSyWithBracketApi pid =
  applyLabel "shouldSetSpanSyWithBracketApi" $ do
    runBracketTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      , ("X-INSTANA-SYNTHETIC", "1")
      ]
      (applyConcat [nonRootEntryAsserts, bracketAsserts, syntheticAssert])


shouldSuppressWithBracketApi :: IO Test
shouldSuppressWithBracketApi =
  applyLabel "shouldSuppressWithBracketApi" $ do
    runSuppressedTest "http/bracket/api"


shouldRedactDefaultSecrets :: String -> IO Test
shouldRedactDefaultSecrets pid =
  applyLabel "shouldRedactDefaultSecrets" $
    runTest
      pid
      "http/bracket/api?query-param=value&api-key=1234&MYPASSWORD=abc&another-query-param=zzz1&secret=yes"
      []
      (applyConcat
        [ rootEntryAsserts
        , filterDefaultSecretsAsserts
        ])


shouldCreateRootEntryWithLowLevelApi :: String -> IO Test
shouldCreateRootEntryWithLowLevelApi pid =
  applyLabel "shouldCreateRootEntryWithLowLevelApi" $
    runLowLevelTest
      pid
      []
      (applyConcat [
          rootEntryAsserts
        , lowLevelAsserts
        , notSyntheticAssert
      ])


shouldAddWebsiteMonitoringCorrelationWithLowLevelApi :: String -> IO Test
shouldAddWebsiteMonitoringCorrelationWithLowLevelApi pid =
  applyLabel "shouldAddWebsiteMonitoringCorrelationWithLowLevelApi" $
    runLowLevelTest
      pid
      [("X-INSTANA-L", "1,correlationType=web;correlationId=1234567890abcdef")]
      (applyConcat [rootEntryAsserts, lowLevelAsserts, correlationAsserts, notSyntheticAssert])


shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithLowLevelApi :: String -> IO Test
shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithLowLevelApi pid =
  applyLabel "shouldIgnoreTraceIdParentIdIfWebsiteMonitoringCorrelationIsPresentWithLowLevelApi" $
    runLowLevelTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      , ("X-INSTANA-L", "1,correlationType=web;correlationId=1234567890abcdef")
      ]
      (applyConcat [rootEntryAsserts, lowLevelAsserts, correlationAsserts, notSyntheticAssert])


shouldCreateNonRootEntryWithLowLevelApi :: String -> IO Test
shouldCreateNonRootEntryWithLowLevelApi pid =
  applyLabel "shouldCreateNonRootEntryWithLowLevelApi" $ do
    runLowLevelTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      ]
      (applyConcat [
          nonRootEntryAsserts
        , lowLevelAsserts
        , notSyntheticAssert
      ])


shouldSetSpanSyWithLowLevelApi :: String -> IO Test
shouldSetSpanSyWithLowLevelApi pid =
  applyLabel "shouldSetSpanSyWithLowLevelApi" $ do
    runLowLevelTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      , ("X-INSTANA-SYNTHETIC", "1")
      ]
      (applyConcat [nonRootEntryAsserts, lowLevelAsserts, syntheticAssert])


shouldSuppressWithLowLevelApi :: IO Test
shouldSuppressWithLowLevelApi =
  applyLabel "shouldSuppressWithLowLevelApi" $ do
    runSuppressedTest "http/low/level/api"


runBracketTest :: String -> [Header] -> (Span -> [Assertion]) -> IO Test
runBracketTest pid headers extraAssertsForEntrySpan =
  runTest pid "http/bracket/api?some=query&parameters=1" headers extraAssertsForEntrySpan


runLowLevelTest :: String -> [Header] -> (Span -> [Assertion]) -> IO Test
runLowLevelTest pid headers extraAssertsForEntrySpan =
  runTest pid "http/low/level/api?some=query&parameters=2" headers extraAssertsForEntrySpan


runTest :: String -> String -> [Header] -> (Span -> [Assertion]) -> IO Test
runTest pid urlPath headers extraAssertsForEntrySpan =
  runHttpTest
    pid
    urlPath
    headers
    extraAssertsForEntrySpan
    (\exitSpan ->
      [ assertEqual "exit data"
        ( Aeson.object
          [ "http" .= (Aeson.object
              [ "method" .= ("GET" :: String)
              , "url"    .= ("http://127.0.0.1:1208/echo" :: String)
              , "params" .= ("some=query&parameters=2&pass=<redacted>" :: String)
              , "status" .= (200 :: Int)
              ]
            )
          ]
        )
        (TraceRequest.spanData exitSpan)
      ]
    )


runHttpTest ::
  String
  -> String
  -> [Header]
  -> (Span -> [Assertion])
  -> (Span -> [Assertion])
  -> IO Test
runHttpTest pid urlPath headers extraAssertsForEntrySpan extraAssertsForExitSpan = do
  response <-
    HttpHelper.doAppRequest Suite.testServer urlPath "GET" headers
  let
    responseBody = LBSC8.unpack $ HTTP.responseBody response
    from = Just $ From pid "agent-stub-id"
    responseHeaders = HTTP.responseHeaders response
    serverTimingTuple :: Maybe (Network.HTTP.Types.Header.HeaderName, BSC8.ByteString)
    serverTimingTuple =
      listToMaybe $
        filter
          (\ (headerName, _) -> headerName == "Server-Timing")
          responseHeaders
    serverTimingValue = BSC8.unpack <$> (snd <$> serverTimingTuple)

  spansResults <-
    TestHelper.waitForRegisteredSpansMatching
      [ "haskell.wai.server", "haskell.http.client" ]
  case spansResults of
    Left failure ->
      failIO $ "Could not load recorded spans from agent stub: " ++ failure
    Right spans -> do
      let
        maybeEntrySpan =
          TestHelper.getSpanByRegisteredName "haskell.wai.server" spans
        maybeExitSpan =
          TestHelper.getSpanByRegisteredName "haskell.http.client" spans
      if isNothing maybeEntrySpan || isNothing maybeExitSpan
        then
          failIO "expected spans have not been recorded"
        else do
          let
            Just entrySpan = maybeEntrySpan
            Just exitSpan = maybeExitSpan
          assertAllIO $
            (commonAsserts entrySpan exitSpan responseBody from serverTimingValue) ++
            (extraAssertsForEntrySpan entrySpan) ++
            (extraAssertsForExitSpan exitSpan)


runSuppressedTest :: String -> IO Test
runSuppressedTest urlPath = do
  response <-
    HttpHelper.doAppRequest
      Suite.testServer
      urlPath
      "GET"
      [("X-INSTANA-L", "0")]
  let
    responseBody = LBSC8.unpack $ HTTP.responseBody response
  -- wait a few seconds, then check that no spans have been recorded
  threadDelay $ 5 * 1000 * 1000
  spansResults <-
    TestHelper.waitForRegisteredSpansMatching []
  case spansResults of
    Left failure ->
      failIO $ "Could not load recorded spans from agent stub: " ++ failure
    Right spans -> do
      if not (null spans)
        then
          failIO "spans have been recorded although they should have not"   else
          assertAllIO
            [ assertBool
                "downstream X-INSTANA-L"
                (isInfixOf "\"X-INSTANA-L\":\"0\"" responseBody)
            , assertBool
                "no downstream X-INSTANA-T"
                (not $ isInfixOf "X-INSTANA-T" responseBody)
            , assertBool
                "no downstream X-INSTANA-S"
                (not $ isInfixOf "X-INSTANA-S" responseBody)
            ]


rootEntryAsserts :: Span -> [Assertion]
rootEntryAsserts entrySpan =
  [ assertEqual "root.traceId = root.spanId"
      (TraceRequest.t entrySpan)
      (TraceRequest.s entrySpan)
  , assertBool "root parent Id" $
      isNothing $ TraceRequest.p entrySpan
  ]


nonRootEntryAsserts :: Span -> [Assertion]
nonRootEntryAsserts entrySpan =
  [ assertEqual "root.traceId"
      "test-trace-id"
      (TraceRequest.t entrySpan)
  , assertBool "root.spanId" $
      "test-trace-id" /= (TraceRequest.s entrySpan)
  , assertEqual "root parent Id"
      (Just $ "test-span-id")
      (TraceRequest.p entrySpan)
  ]


commonAsserts ::
  Span
  -> Span
  -> String
  -> Maybe From
  -> Maybe String
  -> [Assertion]
commonAsserts entrySpan exitSpan responseBody from serverTimingValue =
  [ assertEqual "trace ID is consistent"
      (TraceRequest.t exitSpan)
      (TraceRequest.t entrySpan)
  , assertEqual "Server-Timing header with trace ID is present"
      (Just $ "intid;desc=" ++ (TraceRequest.t entrySpan))
      (serverTimingValue)
  , assertEqual "exit parent ID"
      (Just $ TraceRequest.s entrySpan)
      (TraceRequest.p exitSpan)
  , assertBool
      ("wrong downstream X-INSTANA-T: " ++ responseBody ++
          ", expected " ++ TraceRequest.t entrySpan)
      (isInfixOf
        ("\"X-INSTANA-T\":\"" ++ (TraceRequest.t entrySpan) ++ "\"")
        responseBody
      )
  , assertBool
      ("wrong downstream X-INSTANA-S: " ++ responseBody ++
          ", expected " ++ TraceRequest.s exitSpan)
      (isInfixOf
        ("\"X-INSTANA-S\":\"" ++ (TraceRequest.s exitSpan) ++ "\"")
        responseBody
      )
  , assertBool
      "no downstream X-INSTANA-L"
      (not $ isInfixOf "X-INSTANA-L" responseBody)
  , assertBool "entry timestamp" $ TraceRequest.ts entrySpan > 0
  , assertBool "entry duration" $ TraceRequest.d entrySpan > 0
  , assertEqual "entry kind" 1 (TraceRequest.k entrySpan)
  , assertEqual "entry error" 0 (TraceRequest.ec entrySpan)
  , assertEqual "entry from" from $ TraceRequest.f entrySpan
  , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
  , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
  , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
  , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
  , assertEqual "exit from" from $ TraceRequest.f exitSpan
  ]


correlationAsserts :: Span -> [Assertion]
correlationAsserts entrySpan =
  [ assertEqual "entry correlation type"
      (Just "web")
      (TraceRequest.crtp entrySpan)
  , assertEqual "entry correlation ID"
      (Just "1234567890abcdef")
      (TraceRequest.crid entrySpan)
  ]


bracketAsserts :: Span -> [Assertion]
bracketAsserts entrySpan =
  [ assertEqual "entry data"
    ( Aeson.object
      [ "http"       .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "host"   .= ("127.0.0.1:1207" :: String)
          , "url"    .= ("/http/bracket/api" :: String)
          , "params" .= ("some=query&parameters=1" :: String)
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData entrySpan)
  ]


lowLevelAsserts :: Span -> [Assertion]
lowLevelAsserts entrySpan =
  [ assertEqual "entry data"
    ( Aeson.object
      [ "http"       .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "host"   .= ("127.0.0.1:1207" :: String)
          , "url"    .= ("/http/low/level/api" :: String)
          , "params" .= ("some=query&parameters=2" :: String)
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData entrySpan)
  ]


syntheticAssert :: Span -> [Assertion]
syntheticAssert span_ =
  [ assertEqual "span.sy" (Just True) (TraceRequest.sy span_)
  ]


notSyntheticAssert :: Span -> [Assertion]
notSyntheticAssert span_ =
  [ assertEqual "span.sy" Nothing (TraceRequest.sy span_)
  ]


filterDefaultSecretsAsserts :: Span -> [Assertion]
filterDefaultSecretsAsserts entrySpan =
  [ assertEqual "entry data"
    ( Aeson.object
      [ "http"       .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "host"   .= ("127.0.0.1:1207" :: String)
          , "url"    .= ("/http/bracket/api" :: String)
          , "params" .= ("query-param=value&api-key=<redacted>&MYPASSWORD=<redacted>&another-query-param=zzz1&secret=<redacted>" :: String)
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData entrySpan)
  ]


applyConcat :: [a -> [b]] -> a -> [b]
applyConcat functions a =
  concat $ List.map ($ a) functions

