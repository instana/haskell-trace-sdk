{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.WaiMiddleware (allTests) where


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
  [ shouldCreateRootEntry pid
  , shouldCreateNonRootEntry pid
  , shouldAddWebsiteMonitoringCorrelation pid
  , shouldSuppress
  , shouldCopeWithWrongNestingOfIoActions pid
  ]


shouldCreateRootEntry :: String -> IO Test
shouldCreateRootEntry pid =
  applyLabel "shouldCreateRootEntry" $
    runMiddlewareTest
      pid
      "api"
      []
      (applyConcat [rootEntryAsserts, asserts "api"])


shouldCreateNonRootEntry :: String -> IO Test
shouldCreateNonRootEntry pid =
  applyLabel "shouldCreateNonRootEntry" $ do
    runMiddlewareTest
      pid
      "api"
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      ]
      (applyConcat [nonRootEntryAsserts, asserts "api"])


shouldAddWebsiteMonitoringCorrelation :: String -> IO Test
shouldAddWebsiteMonitoringCorrelation pid =
  applyLabel "shouldAddWebsiteMonitoringCorrelation" $
    runMiddlewareTest
      pid
      "api"
      [("X-INSTANA-L", "   1 ,   correlationType = web ;  correlationId =  1234567890abcdef  ")]
      (applyConcat [rootEntryAsserts, asserts "api", correlationAsserts])


shouldSuppress :: IO Test
shouldSuppress =
  applyLabel "shouldSuppress" $ do
    runSuppressedTest "api"


shouldCopeWithWrongNestingOfIoActions :: String -> IO Test
shouldCopeWithWrongNestingOfIoActions pid =
  applyLabel "shouldCopeWithWrongNestingOfIoActions" $
    runMiddlewareTest
    pid
    "wrong-nesting"
    []
    (applyConcat [rootEntryAsserts, asserts "wrong-nesting"])


runMiddlewareTest ::
  String
  -> String
  -> [Header]
  -> (Span -> [Assertion]) -> IO Test
runMiddlewareTest pid route headers extraAsserts =
  runTest pid (route ++ "?some=query&parameters=1") headers extraAsserts


runTest :: String -> String -> [Header] -> (Span -> [Assertion]) -> IO Test
runTest pid urlPath headers extraAsserts = do
  response <-
    HttpHelper.doAppRequest Suite.testServerWithMiddleware urlPath "GET" headers
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
            (commonAsserts
              entrySpan
              exitSpan
              responseBody
              from
              serverTimingValue
            ) ++
            (extraAsserts entrySpan)


runSuppressedTest :: String -> IO Test
runSuppressedTest urlPath = do
  response <-
    HttpHelper.doAppRequest
      Suite.testServerWithMiddleware
      urlPath
      "GET"
      [("X-INSTANA-L", "0")]
  let
    responseBody = LBSC8.unpack $ HTTP.responseBody response
  -- wait a second, then check that no spans have been recorded
  threadDelay $ 10 * 1000
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
  [ assertEqual "Server-Timing header with trace ID is present"
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
  , assertEqual "exit data"
    ( Aeson.object
      [ "http" .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "url"    .= ("http://127.0.0.1:1208/echo" :: String)
          , "params" .= ("some=query&parameters=2" :: String)
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData exitSpan)
  ]


asserts :: String -> Span -> [Assertion]
asserts route entrySpan =
  [ assertEqual "entry data"
    ( Aeson.object
      [ "http"       .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "host"   .= ("127.0.0.1:1207" :: String)
          , "url"    .= (("/" ++ route) :: String)
          , "params" .= ("some=query&parameters=1" :: String)
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData entrySpan)
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


applyConcat :: [a -> [b]] -> a -> [b]
applyConcat functions a =
  concat $ List.map ($ a) functions

