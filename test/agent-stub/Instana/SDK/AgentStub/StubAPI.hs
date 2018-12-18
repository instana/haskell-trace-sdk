{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Instana.SDK.AgentStub.StubAPI
  ( StubAPI
  , ResetAPI
  ) where


import           Servant

import           Instana.SDK.AgentStub.DiscoveryRequest  (DiscoveryRequest)
import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import           Instana.SDK.AgentStub.TraceRequest      (Span)


type StubAPI =
       -- GET /stub/ping
       "ping"
       :> GetNoContent '[JSON] NoContent
       -- GET /stub/discoveries
  :<|> "discoveries"
       :> Get '[JSON] [DiscoveryRequest]
       -- GET /stub/agent-ready
  :<|> "agent-ready"
       :> Get '[JSON] [String]
       -- GET /stub/entity-data
  :<|> "entity-data"
       :> Get '[JSON] [EntityDataRequest]
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
       -- POST /stub/reset/agent-ready-requests
  :<|> "spans"
       :> PostNoContent '[JSON] NoContent

