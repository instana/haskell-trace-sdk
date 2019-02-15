{-|
Module      : Instana.Wai.Middleware.Entry
Description : WAI Instana Tracing Middleware

Automatically creates entry spans for all incoming HTTP requests in a WAI
application. Note that exit spans still need to be created manually via the
withExit or startExit/stopExit.

== Example

@
main = do
  Warp.run 3000 $ InstanaWaiMiddleware.traceHttpEntries instana $ app
@
-}
module Instana.Wai.Middleware.Entry
  ( module Instana.Wai.Middleware.Entry.Internal
  ) where

import           Instana.Wai.Middleware.Entry.Internal (traceHttpEntries)

