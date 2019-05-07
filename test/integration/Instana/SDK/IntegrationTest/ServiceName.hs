{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.IntegrationTest.ServiceName
  ( shouldUseServiceNameEnvVar
  ) where


import           Data.Aeson                             ((.=))
import qualified Data.Aeson                             as Aeson
import           Data.ByteString.Lazy.Char8             as LBSC8
import           Data.Maybe                             (isNothing)
import qualified Network.HTTP.Client                    as HTTP
import           Test.HUnit

import           Instana.SDK.AgentStub.TraceRequest     (From (..))
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper


shouldUseServiceNameEnvVar :: String -> IO Test
shouldUseServiceNameEnvVar pid =
  applyLabel "shouldUseServiceNameEnvVar" $ do
    let
      from = Just $ From pid "agent-stub-id"
    (result, spansResults) <-
      TestHelper.withSpanCreation
        createSpansWithServiceName
        [ "haskell.dummy.root.entry"
        , "haskell.dummy.exit"
        ]
    case spansResults of
      Left failure ->
        failIO $ "Could not load recorded spans from agent stub: " ++ failure
      Right spans -> do
        let
          maybeRootEntrySpan =
            TestHelper.getSpanBySdkName "haskell.dummy.root.entry" spans
          maybeExitSpan = TestHelper.getSpanBySdkName "haskell.dummy.exit" spans
        if isNothing maybeRootEntrySpan || isNothing maybeExitSpan
          then
            failIO "expected spans have not been recorded"
          else do
            let
              Just rootEntrySpan = maybeRootEntrySpan
              Just exitSpan = maybeExitSpan
            assertAllIO
              [ assertEqual "result" "exit done::entry done" result
              , assertEqual "trace ID is consistent"
                  (TraceRequest.t rootEntrySpan)
                  (TraceRequest.t exitSpan)
              , assertEqual "root.traceId == root.spanId"
                  (TraceRequest.s rootEntrySpan)
                  (TraceRequest.t rootEntrySpan)
              , assertBool "root has no parent" $
                  isNothing $ TraceRequest.p rootEntrySpan
              , assertEqual "exit parent"
                  (Just $ TraceRequest.s rootEntrySpan)
                  (TraceRequest.p exitSpan)
              , assertBool "entry timestamp" $ TraceRequest.ts rootEntrySpan > 0
              , assertBool "entry duration" $ TraceRequest.d rootEntrySpan > 0
              , assertEqual "entry kind" 1 (TraceRequest.k rootEntrySpan)
              , assertEqual "entry error" 0 (TraceRequest.ec rootEntrySpan)
              , assertEqual "entry from" from $ TraceRequest.f rootEntrySpan
              , assertEqual "entry data"
                ( Aeson.object
                  -- service name from env var:
                  [ "service" .= ("Custom Service Name" :: String)
                  , "sdk"     .= (Aeson.object
                    [ "name"  .= ("haskell.dummy.root.entry" :: String)
                    , "type"  .= ("entry" :: String)
                    ])
                  ]
                )
                (TraceRequest.spanData rootEntrySpan)
              , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
              , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
              , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
              , assertEqual "exit error" 0 (TraceRequest.ec exitSpan)
              , assertEqual "exit from" from $ TraceRequest.f exitSpan
              , assertEqual "exit data"
                ( Aeson.object
                  -- service name from span tag:
                  [ "service" .= ("Service Exit" :: String)
                  , "sdk"    .= (Aeson.object
                    [ "name" .= ("haskell.dummy.exit" :: String)
                    , "type" .= ("exit" :: String)
                    ])
                  ]
                )
                (TraceRequest.spanData exitSpan)
              ]


createSpansWithServiceName :: IO String
createSpansWithServiceName = do
  response <-
    HttpHelper.doAppRequest
      "bracket/api/with-service-name-exit-only"
      "POST"
      []
  return $ LBSC8.unpack $ HTTP.responseBody response

