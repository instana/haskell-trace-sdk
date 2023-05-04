{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.CustomSecrets (allTests) where


import           Data.Aeson                              ((.=))
import qualified Data.Aeson                              as Aeson
import           Instana.SDK.AgentStub.TraceRequest      (Span)
import qualified Instana.SDK.AgentStub.TraceRequest      as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra  (applyLabel)
import           Instana.SDK.IntegrationTest.HttpTracing (runHttpTest)
import           Test.HUnit



allTests :: String -> [IO Test]
allTests pid =
  [ shouldRedactCustomSecrets pid
  ]


shouldRedactCustomSecrets :: String -> IO Test
shouldRedactCustomSecrets pid =
  applyLabel "shouldRedactCustomSecrets" $
    runHttpTest
      pid
      ("http/bracket/api?"
        -- this will be captured since the custom secret config overrides the default secret list
        ++ "api-key=1234&"
        -- this should be redacted according to the custom config
        ++ "hidden-param=value&"
        -- this will be captured since the custom secret config overrides the default secret list
        ++ "MYPASSWORD=abc&"
        -- this should be redacted according to the custom config
        ++ "this-will-be-obscured-by-the-regex-matcher=value&"
        -- this will be captured since the custom secret config overrides the default secret list
        ++ "secret=yes&"
        -- this will be captured since the regex for hidden has no leading .*
        ++ "not-hidden=value"
      )
      []
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
          , "params" .= (
              -- this will be captured since the custom secret config overrides the default secret list
              "api-key=1234&"
              -- this should be filterd according to the custom config
              ++ "hidden-param=<redacted>&"
              -- this will be captured since the custom secret config overrides the default secret list
              ++ "MYPASSWORD=abc&"
              -- this should be filterd according to the custom config
              ++ "this-will-be-obscured-by-the-regex-matcher=<redacted>&"
              -- this will be captured since the custom secret config overrides the default secret list
              ++ "secret=yes&"
              -- this will be captured since the regex for hidden has no leading .*
              ++ "not-hidden=value" :: String
            )
          , "status" .= (200 :: Int)
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
          , "params" .= (
              "api-key=1234&"
              ++ "hidden-param=<redacted>&"
              ++ "MYPASSWORD=abc&"
              ++ "this-will-be-obscured-by-the-regex-matcher=<redacted>&"
              ++ "secret=yes&"
              ++ "not-hidden=value" :: String
            )
          , "status" .= (200 :: Int)
          ]
        )
      ]
    )
    (TraceRequest.spanData exitSpan)
  ]

