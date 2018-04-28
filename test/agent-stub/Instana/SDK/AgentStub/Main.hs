{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.Main where

import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (when)
import qualified Control.Monad.ST                as ST
import           Data.Time.Clock.POSIX           (getPOSIXTime)
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Handler.Warp        as Warp
import qualified Servant
import           System.Log.Logger               (infoM)

import qualified Instana.SDK.AgentStub.API       as API
import           Instana.SDK.AgentStub.Config    (AgentStubConfig)
import qualified Instana.SDK.AgentStub.Config    as Config
import           Instana.SDK.AgentStub.Logging   (agentStubLogger)
import qualified Instana.SDK.AgentStub.Logging   as Logging
import           Instana.SDK.AgentStub.Recorders (Recorders)
import qualified Instana.SDK.AgentStub.Recorders as Recorders
import qualified Instana.SDK.AgentStub.Server    as Server


main :: IO ()
main = do
  Logging.initLogging
  config <- Config.readConfig
  recorders <- ST.stToIO $ Recorders.mkRecorders
  startupTime <- round . (* 1000) <$> getPOSIXTime
  let
    host = Config.bindHost config
    port = Config.bindPort config
    startupDelay = Config.startupDelay config
    serverSettings =
      Warp.setHost host $
      Warp.setPort port $
      Warp.defaultSettings
  infoM agentStubLogger $ "Starting agent stub bound to " ++
             (show $ Warp.getHost serverSettings) ++ " and port " ++
             (show $ Warp.getPort serverSettings)
  when
    (startupDelay > 0)
    ( do
        infoM agentStubLogger $
          "Artificial startup delay: " ++ show startupDelay ++ "ms, pausing."
        threadDelay $ startupDelay * 1000
        infoM agentStubLogger $
          "Artificial startup delay has passed, resuming startup."
    )
  Warp.runSettings serverSettings $
    app config startupTime recorders


app ::
  AgentStubConfig
  -> Int
  -> Recorders
  -> Wai.Application
app config startupTime recorders =
  Servant.serve
    API.proxyApi
    (Server.mainServer
      config
      startupTime
      recorders
    )

