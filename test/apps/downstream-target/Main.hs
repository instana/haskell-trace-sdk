{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as Aeson
import qualified Data.Binary.Builder        as Builder
import qualified Data.ByteString.Char8      as BSC8
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.CaseInsensitive       as CaseInsensitive
import qualified Data.Map                   as Map
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTPTypes
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           System.Environment         (lookupEnv)
import qualified System.Exit                as Exit
import           System.IO                  (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (GenericHandler, fileHandler,
                                             streamHandler)
import           System.Log.Logger          (Priority (..), infoM,
                                             rootLoggerName, setHandlers,
                                             setLevel, updateGlobalLogger)
import qualified System.Posix.Process       as Posix
import           System.Posix.Types         (CPid)


appLogger :: String
appLogger = "DownstreamTarget"


application :: CPid -> Wai.Application
application pid request respond = do
  let
    route = Wai.pathInfo request
    method = Wai.requestMethod request
  case (method, route) of
    (_, []) ->
      root respond
    (_, ["ping"]) ->
      ping respond pid
    (_, ["echo"]) ->
      echoHeaders request respond
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
    "Downstream Target"


ping ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> CPid
  -> IO Wai.ResponseReceived
ping respond pid = do
  respond $
    Wai.responseLBS HTTPTypes.status200 [] $ LBSC8.pack $ show pid


echoHeaders ::
  Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
echoHeaders request respond = do
  let
    headers :: [(HTTPTypes.HeaderName, BSC8.ByteString)]
    headers = Wai.requestHeaders request
    mapped :: [(String, String)]
    mapped =
      map (
        \(headerName, value) ->
          (
            BSC8.unpack $ CaseInsensitive.original headerName,
            BSC8.unpack value
          )
      ) headers
    encodedHeaders = Aeson.encode $ Map.fromList mapped
  respond $
    Wai.responseLBS
      HTTPTypes.status200
      [ ("Content-Type", "application/json; charset=UTF-8")
      , ("X-Response-Header-On-Exit", "response header on exit value")
      , ("X-Response-Header-Downstream-To-App", "Value 3")
      ]
      encodedHeaders


shutDown ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
shutDown respond = do
  liftIO $ infoM appLogger $ "Downstream target app shutdown requested"
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
  runApp


runApp :: IO ()
runApp = do
  pid <- Posix.getProcessID
  let
    host = "127.0.0.1"
    port = (1208 :: Int)
    warpSettings =
      ((Warp.setPort port) . (Warp.setHost "127.0.0.1")) Warp.defaultSettings
  infoM appLogger $
    "Starting downstream target app at " ++ host ++ ":" ++ show port ++
    " (PID: " ++ show pid ++ ")."
  Warp.runSettings warpSettings $ application pid


initLogging :: IO ()
initLogging = do
  logLevelEnvVar <- lookupEnv "APP_LOG_LEVEL"
  let
    logLevel =
      case logLevelEnvVar of
        Just "DEBUG" -> DEBUG
        _            -> INFO
  updateGlobalLogger appLogger $ setLevel logLevel
  appFileHandler <- fileHandler "downstream-target.log" logLevel
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

