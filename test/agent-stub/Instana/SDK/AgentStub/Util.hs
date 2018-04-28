{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.Util
  ( shouldSimulateConnectionLoss
  , stToServant
  ) where


import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.ST             (RealWorld, ST)
import qualified Control.Monad.ST             as ST
import qualified Servant

import           Instana.SDK.AgentStub.Config (AgentStubConfig)
import qualified Instana.SDK.AgentStub.Config as Config


stToServant :: ST RealWorld a -> Servant.Handler a
stToServant = liftIO . ST.stToIO


shouldSimulateConnectionLoss :: AgentStubConfig -> Int -> Int -> Bool
shouldSimulateConnectionLoss config startupTime now =
  -- Simulate connection loss after from 1500ms after server startup until
  -- 3500ms seconds after startup.
  Config.simulateConnectionLoss config &&
    (now > startupTime + 1500 && now < startupTime + 3500)

