{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Instana.SDK.AgentStub.StubAPI
  ( StubAPI
  , ResetAPI
  ) where


import           Servant

import           Instana.SDK.AgentStub.DiscoveryRequest (DiscoveryRequest)
import           Instana.SDK.AgentStub.TraceRequest     (Span)


type StubAPI =
       -- GET /stub/ping
       "ping"
       :> GetNoContent '[JSON] NoContent
       -- GET /stub/discoveries
  :<|> "discoveries"
       :> Get '[JSON] [DiscoveryRequest]
       -- GET /stub/agentReady
  :<|> "agentReady"
       :> Get '[JSON] [String]
       -- GET /stub/spans
  :<|> "spans"
       :> Get '[JSON] [Span]
       -- POST /stub/shutdown
  :<|> "shutdown"
       :> PostNoContent '[JSON] NoContent
  :<|> "reset"
       :> ResetAPI


type ResetAPI =
       -- POST /stub/reset/discoveries
       "discoveries"
       :> PostNoContent '[JSON] NoContent
       -- POST /stub/reset/spans
  :<|> "spans"
       :> PostNoContent '[JSON] NoContent

