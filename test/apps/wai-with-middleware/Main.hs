{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent           (threadDelay)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Binary.Builder          as Builder
import qualified Data.ByteString.Lazy.Char8   as LBSC8
import           Instana.SDK.SDK              (InstanaContext)
import qualified Instana.SDK.SDK              as InstanaSDK
import qualified Instana.Wai.Middleware.Entry as InstanaWaiMiddleware
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Types           as HTTPTypes
import qualified Network.Wai                  as Wai
import qualified Network.Wai.Handler.Warp     as Warp
import           System.Environment           (lookupEnv)
import qualified System.Exit                  as Exit
import           System.IO                    (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple    (GenericHandler, fileHandler,
                                               streamHandler)
import           System.Log.Logger            (Priority (..), rootLoggerName,
                                               setHandlers, setLevel,
                                               updateGlobalLogger)
import           System.Log.Logger            (infoM)
import qualified System.Posix.Process         as Posix
import           System.Posix.Types           (CPid)


appLogger :: String
appLogger = "WaiWithMiddleware"


application :: InstanaContext -> HTTP.Manager -> CPid -> Wai.Application
application instana httpManager pid request respond = do
  let
    route = Wai.pathInfo request
    method = Wai.requestMethod request
  case (method, route) of
    (_, []) ->
      root respond
    (_, ["ping"]) ->
      ping respond pid
    ("GET", ["api"]) ->
      apiUnderTest instana httpManager request respond
    ("POST", ["shutdown"]) ->
      shutDown respond
    _ ->
      respond404 respond


root ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
root respond =
  respondWithPlainText
    respond
    "Instana Haskell Trace SDK Integration Test Wai Dummy App"


ping ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> CPid
  -> IO Wai.ResponseReceived
ping respond pid = do
  respond $
    Wai.responseLBS HTTPTypes.status200 [] $ LBSC8.pack $ show pid


apiUnderTest ::
  InstanaContext
  -> HTTP.Manager
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
apiUnderTest instana httpManager _ respond = do
  downstreamRequest <-
    HTTP.parseUrlThrow $
      "http://127.0.0.1:1208/echo?some=query&parameters=2&pass=secret"
  downstreamResponse <- InstanaSDK.withHttpExit
    instana
    downstreamRequest
    (\req -> do
      dowstreamRequest' <- HTTP.httpLbs req httpManager
      threadDelay $ 1000 -- make sure there is a duration > 0
      return dowstreamRequest'
    )
  respond $
    Wai.responseLBS
      HTTPTypes.status200
      [("Content-Type", "application/json; charset=UTF-8")]
      (HTTP.responseBody downstreamResponse)


shutDown ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
shutDown respond = do
  liftIO $ infoM appLogger $ "Wai/Warp app (/w middleware) shutdown requested"
  _ <-liftIO $ Posix.exitImmediately Exit.ExitSuccess
  respond $
    Wai.responseBuilder HTTPTypes.status204 [] Builder.empty


respondWithPlainText ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> String
  -> IO Wai.ResponseReceived
respondWithPlainText respond content =
  respond $
    Wai.responseLBS
      HTTPTypes.status200
      [("Content-Type", "text/plain")]
      (LBSC8.pack content)


respond404 ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
respond404 respond =
  respond $
    Wai.responseLBS HTTPTypes.status404 [] "not found"


main :: IO ()
main = do
  initLogging
  httpManager <- initHttpManager
  let
    config = InstanaSDK.defaultConfig { InstanaSDK.agentPort = Just 1302 }
  InstanaSDK.withConfiguredInstana config $ runApp httpManager


runApp :: HTTP.Manager -> InstanaContext -> IO ()
runApp httpManager instana = do
  pid <- Posix.getProcessID
  let
    host = "127.0.0.1"
    port = (1207 :: Int)
    warpSettings =
      ((Warp.setPort port) . (Warp.setHost "127.0.0.1")) Warp.defaultSettings
  infoM appLogger $
    "Starting Wai/Warp app (/w middleware) at " ++ host ++ ":" ++ show port ++
    " (PID: " ++ show pid ++ ")."
  let
    app = application instana httpManager pid
    appWithMiddleware = InstanaWaiMiddleware.traceHttpEntries instana app
  Warp.runSettings warpSettings $ appWithMiddleware


initLogging :: IO ()
initLogging = do
  logLevelEnvVar <- lookupEnv "APP_LOG_LEVEL"
  let
    logLevel =
      case logLevelEnvVar of
        Just "DEBUG" -> DEBUG
        _            -> INFO
  updateGlobalLogger appLogger $ setLevel logLevel
  appFileHandler <- fileHandler "wai-warp-app-with-middleware.log" logLevel
  appStreamHandler <- streamHandler stdout logLevel
  let
    formattedAppFileHandler = withFormatter appFileHandler
    formattedAppStreamHandler = withFormatter appStreamHandler
  updateGlobalLogger appLogger $
    setHandlers [ formattedAppFileHandler ]
  updateGlobalLogger rootLoggerName $
    setHandlers [ formattedAppStreamHandler ]


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where
    timeFormat = "%F %H:%M:%S.%4q %z"
    formatter = tfLogFormatter timeFormat "{$time $loggername $pid $prio} $msg"


initHttpManager :: IO HTTP.Manager
initHttpManager =
  HTTP.newManager $
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 5
      , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ 5000 * 1000
      }

