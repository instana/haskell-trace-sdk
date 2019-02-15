{-|
Module      : Instana.Wai.Middleware.Entry.Internal
Description : Internals of the WAI Instana Tracing Middleware

Automatically creates entry spans for all incoming HTTP requests in a WAI
application.
-}
module Instana.Wai.Middleware.Entry.Internal
  ( traceHttpEntries
  ) where


import           Network.Wai     (Application)

import           Instana.SDK.SDK (InstanaContext, withHttpEntry)


-- |Run the tracing middleware given an initialized Instana SDK context.
traceHttpEntries :: InstanaContext -> Application -> Application
traceHttpEntries instana app request respond = do
  withHttpEntry instana request $
    app request respond

