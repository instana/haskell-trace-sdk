module Instana.SDK.AgentStub.Recorders where


import           Control.Monad.ST                        (RealWorld, ST)
import           Data.STRef                              (STRef, newSTRef)

import           Instana.SDK.AgentStub.DiscoveryRequest  (DiscoveryRequest)
import           Instana.SDK.AgentStub.EntityDataRequest (EntityDataRequest)
import           Instana.SDK.AgentStub.TraceRequest      (Span)


type DiscoverRecorder = STRef RealWorld [DiscoveryRequest]


type AgentReadyRecorder = STRef RealWorld [String]


type EntityDataRecorder = STRef RealWorld [EntityDataRequest]


type SpanRecorder = STRef RealWorld [Span]


data Recorders =
  Recorders
    { discoveryRecorder  :: DiscoverRecorder
    , agentReadyRecorder :: AgentReadyRecorder
    , entityDataRecorder :: EntityDataRecorder
    , spanRecorder       :: SpanRecorder
    }


mkRecorders :: ST RealWorld Recorders
mkRecorders = do
  dr  <- newSTRef []
  arr <- newSTRef []
  edr <- newSTRef []
  sr  <- newSTRef []
  return $
    Recorders
      { discoveryRecorder  = dr
      , agentReadyRecorder = arr
      , entityDataRecorder = edr
      , spanRecorder       = sr
      }

