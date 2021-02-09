{-|
Module      : Instana.Wai.Middleware.Entry.Internal
Description : Internals of the WAI Instana Tracing Middleware

Automatically creates entry spans for all incoming HTTP requests in a WAI
application.
-}
module Instana.Wai.Middleware.Entry.Internal
  ( traceHttpEntries
  ) where


import           Instana.SDK.Internal.Id           (Id)
import qualified Instana.SDK.Internal.ServerTiming as ServerTiming
import           Network.Wai                       (Middleware, Response)
import qualified Network.Wai                       as Wai

import           Instana.SDK.SDK                   (InstanaContext,
                                                    currentTraceId,
                                                    withHttpEntry)


{-| Run the tracing middleware given an initialized Instana SDK context. The
middleware will create entry spans automatically. It will also add (or append
to) the HTTP respons header (Server-Timing) that is used for website monitoring
back end correlation.
-}
traceHttpEntries :: InstanaContext -> Middleware
traceHttpEntries instana app request respond = do
  withHttpEntry instana request $ do
    traceIdMaybe <- currentTraceId instana
    case traceIdMaybe of
      Just traceId ->
        app request $ respond . addHeader traceId
      Nothing ->
        app request respond


addHeader :: Id -> Response -> Response
addHeader traceId =
  Wai.mapResponseHeaders $ ServerTiming.addTraceIdToServerTiming traceId
