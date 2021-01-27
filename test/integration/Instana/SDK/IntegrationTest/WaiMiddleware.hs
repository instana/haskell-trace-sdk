{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.WaiMiddleware
  ( shouldCreateRootEntry
  , shouldCreateNonRootEntry
  , shouldSuppress
  ) where


import           Control.Concurrent                     (threadDelay)
import           Data.Aeson                             ((.=))
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Lazy.Char8             as LBSC8
import qualified Data.List                              as List
import           Data.Maybe                             (isNothing)
import           Instana.SDK.AgentStub.TraceRequest     (From (..), Span)
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel, applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import qualified Network.HTTP.Client                    as HTTP
import           Network.HTTP.Types                     (Header)
import           Test.HUnit


shouldCreateRootEntry :: String -> IO Test
shouldCreateRootEntry pid =
  applyLabel "shouldCreateRootEntry" $
    runMiddlewareTest pid [] (applyConcat [rootEntryAsserts, asserts])


shouldCreateNonRootEntry :: String -> IO Test
shouldCreateNonRootEntry pid =
  applyLabel "shouldCreateNonRootEntry" $ do
    runMiddlewareTest
      pid
      [ ("X-INSTANA-T", "test-trace-id")
      , ("X-INSTANA-S", "test-span-id")
      ]
      (applyConcat [nonRootEntryAsserts, asserts])


shouldSuppress :: IO Test
shouldSuppress =
  applyLabel "shouldSuppress" $ do
    runSuppressedTest "api"


runMiddlewareTest :: String -> [Header] -> (Span -> [Assertion]) -> IO Test
runMiddlewareTest pid headers extraAsserts =
  runTest pid "api?some=query&parameters=1" headers extraAsserts


runTest :: String -> String -> [Header] -> (Span -> [Assertion]) -> IO Test
runTest pid urlPath headers extraAsserts = do
  response <-
    HttpHelper.doAppRequest urlPath "GET" headers
  let
    result = LBSC8.unpack $ HTTP.responseBody response
    from = Just $ From pid "agent-stub-id"
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
            (commonAsserts entrySpan exitSpan result from) ++
            (extraAsserts entrySpan)


runSuppressedTest :: String -> IO Test
runSuppressedTest urlPath = do
  response <-
    HttpHelper.doAppRequest urlPath "GET" [("X-INSTANA-L", "0")]
  let
    result = LBSC8.unpack $ HTTP.responseBody response
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
            [ assertEqual "result" "{\"response\": \"ok\"}" result
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


commonAsserts :: Span -> Span -> String -> Maybe From -> [Assertion]
commonAsserts entrySpan exitSpan result from =
  [ assertEqual "result" "{\"response\": \"ok\"}" result
  , assertEqual "trace ID is consistent"
      (TraceRequest.t exitSpan)
      (TraceRequest.t entrySpan)
  , assertEqual "exit parent ID"
      (Just $ TraceRequest.s entrySpan)
      (TraceRequest.p exitSpan)
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
          , "url"    .= ("http://127.0.0.1:1302/" :: String)
          , "params" .= ("some=query&parameters=2" :: String)
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData exitSpan)
  ]


asserts :: Span -> [Assertion]
asserts entrySpan =
  [ assertEqual "entry data"
    ( Aeson.object
      [ "http"       .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "host"   .= ("127.0.0.1:1207" :: String)
          , "url"    .= ("/api" :: String)
          , "params" .= ("some=query&parameters=1" :: String)
          ]
        )
      ]
    )
    (TraceRequest.spanData entrySpan)
  ]


applyConcat :: [a -> [b]] -> a -> [b]
applyConcat functions a =
  concat $ List.map ($ a) functions
