{-|
Module      : Instana.Wai.Middleware.Entry.Internal
Description : Internals of the WAI Instana Tracing Middleware

Automatically creates entry spans for all incoming HTTP requests in a WAI
application.
-}
module Instana.Wai.Middleware.Entry.Internal
  ( traceHttpEntries
  ) where


import           Network.Wai     (Middleware)

import           Instana.SDK.SDK (InstanaContext, postProcessHttpResponse,
                                  withHttpEntry)


{-| Run the tracing middleware given an initialized Instana SDK context. The
middleware will create entry spans automatically. It will also add (or append
to) the HTTP respons header (Server-Timing) that is used for website monitoring
back end correlation.
-}
traceHttpEntries :: InstanaContext -> Middleware
traceHttpEntries instana app request respond = do
  withHttpEntry instana request $ do
    app request $ \response -> do
      response' <- postProcessHttpResponse instana response
      respond $ response'

