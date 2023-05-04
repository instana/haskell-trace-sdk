{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.ExtraHttpHeaders
  ( shouldApplyExtraHeadersFromCommonTracerConfig
  , shouldApplyExtraHeadersFromLegacyConfig
  ) where


import           Data.Aeson                              ((.=))
import qualified Data.Aeson                              as Aeson
import           Instana.SDK.AgentStub.TraceRequest      (Span)
import qualified Instana.SDK.AgentStub.TraceRequest      as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra  (applyLabel)
import           Instana.SDK.IntegrationTest.HttpTracing (runHttpTest)
import           Test.HUnit


shouldApplyExtraHeadersFromCommonTracerConfig :: String -> IO Test
shouldApplyExtraHeadersFromCommonTracerConfig pid =
  applyLabel "shouldApplyExtraHeadersFromCommonTracerConfig" $
    runHttpTest
      pid
      "http/bracket/api"
      [("X-Request-Header-On-Entry", "request header on entry value")]
      entrySpanDataAsserts
      exitSpanDataAsserts


shouldApplyExtraHeadersFromLegacyConfig :: String -> IO Test
shouldApplyExtraHeadersFromLegacyConfig pid =
  applyLabel "shouldApplyExtraHeadersFromLegacyConfig" $
    runHttpTest
      pid
      "http/bracket/api"
      [("X-Request-Header-On-Entry", "request header on entry value")]
      entrySpanDataAsserts
      exitSpanDataAsserts


entrySpanDataAsserts :: Span -> [Assertion]
entrySpanDataAsserts entrySpan =
  [ assertEqual "entry data"
    ( Aeson.object
      [ "http"       .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "host"   .= ("127.0.0.1:1207" :: String)
          , "url"    .= ("/http/bracket/api" :: String)
          , "status" .= (200 :: Int)
          , "header" .= (Aeson.object
            [ "X-Response-Header-On-Entry" .= ("response header on entry value" :: String)
            , "X-Request-Header-On-Entry"  .= ("request header on entry value" :: String)
            ])
          ]
        )
      ]
    )
    (TraceRequest.spanData entrySpan)
  ]


exitSpanDataAsserts :: Span -> [Assertion]
exitSpanDataAsserts exitSpan =
  [ assertEqual "exit data"
    ( Aeson.object
      [ "http" .= (Aeson.object
          [ "method" .= ("GET" :: String)
          , "url"    .= ("http://127.0.0.1:1208/echo" :: String)
          , "status" .= (200 :: Int)
          , "header" .= (Aeson.object
            [ "X-Request-Header-On-Exit"  .= ("request header on exit value" :: String)
            , "X-Response-Header-On-Exit" .= ("response header on exit value" :: String)
            ])
          ]
        )
      ]
    )
    (TraceRequest.spanData exitSpan)
  ]

