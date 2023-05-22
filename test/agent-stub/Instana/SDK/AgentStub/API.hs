{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Instana.SDK.AgentStub.API (API, proxyApi) where


import           Servant

import           Instana.SDK.AgentStub.DiscoveryRequest  (DiscoveryRequest)
import           Instana.SDK.AgentStub.DiscoveryResponse (DiscoveryResponse)
import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import           Instana.SDK.AgentStub.StubAPI           (StubAPI)
import           Instana.SDK.AgentStub.TraceRequest      (TraceRequest)


type API =
       -- GET /
       Get '[JSON] NoContent

       -- PUT /com.instana.plugin.haskell.discovery
  :<|> "com.instana.plugin.haskell.discovery"
       :> ReqBody '[JSON] DiscoveryRequest
       :> Put '[JSON] DiscoveryResponse

       -- HEAD /com.instana.plugin.haskell.{pid} -> ready to accept?
  :<|> Capture "pid" String
       :> Head '[JSON] NoContent

       -- POST /com.instana.plugin.haskell.{pid} -> metrics a.k.a entity data
  :<|> Capture "pid" String
       :> ReqBody '[JSON] EntityDataRequest
       :> Post '[JSON] NoContent

       -- POST /com.instana.plugin.haskell.discovery.{pid}
  :<|> "com.instana.plugin.haskell"
       :> Capture "pid" String
       :> ReqBody '[JSON] TraceRequest
       :> Post '[JSON] NoContent

       -- /stub/ sub API
  :<|> "stub"
       :> StubAPI


type Head = (Verb 'HEAD 200)


proxyApi :: Proxy API
proxyApi = Proxy

