{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent         (threadDelay)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value, (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.Binary.Builder        as Builder
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import           Instana.SDK.SDK            (InstanaContext)
import qualified Instana.SDK.SDK            as InstanaSDK
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTPTypes
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Exit                as Exit
import           System.IO                  (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (GenericHandler, fileHandler,
                                             streamHandler)
import           System.Log.Logger          (Priority (..), rootLoggerName,
                                             setHandlers, setLevel,
                                             updateGlobalLogger)
import           System.Log.Logger          (infoM)
import qualified System.Posix.Process       as Posix
import           System.Posix.Types         (CPid)


appLogger :: String
appLogger = "WaiWarpApp"


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
    ("POST", ["bracket", "api", "root"]) ->
      bracketApiRootEntry instana respond
    ("POST", ["bracket", "api", "non-root"]) ->
      bracketApiNonRootEntry instana respond
    ("POST", ["bracket", "api", "with-data"]) ->
      bracketApiWithData instana respond
    ("POST", ["low", "level", "api", "root"]) ->
      lowLevelApiRootEntry instana respond
    ("POST", ["low", "level", "api", "non-root"]) ->
      lowLevelApiNonRootEntry instana respond
    ("POST", ["low", "level", "api", "with-data"]) ->
      lowLevelApiWithData instana respond
    ("GET", ["http", "bracket", "api"]) ->
      httpBracketApi instana httpManager request respond
    ("GET", ["http", "low", "level", "api"]) ->
      httpLowLevelApi instana httpManager request respond
    ("POST", ["single"]) ->
      createSingleSpan instana request respond
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


bracketApiRootEntry ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
bracketApiRootEntry instana respond = do
  result <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (withExit instana)
  respondWithPlainText respond $ result ++ "::entry done"


bracketApiNonRootEntry ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
bracketApiNonRootEntry instana respond = do
  result <-
    InstanaSDK.withEntry
      instana
      "trace-id"
      "parent-id"
      "haskell.dummy.entry"
      (withExit instana)
  respondWithPlainText respond $ result ++ "::entry done"


withExit :: InstanaContext -> IO String
withExit instana =
  InstanaSDK.withExit
    instana
    "haskell.dummy.exit"
    simulateExitCall


bracketApiWithData ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
bracketApiWithData instana respond = do
  entryCallResult <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (withExitWithData instana)
  respondWithPlainText respond $ entryCallResult


withExitWithData :: InstanaContext -> IO String
withExitWithData instana = do
  InstanaSDK.addData instana (someSpanData "entry")
  exitCallResult <-
    InstanaSDK.withExit
      instana
      "haskell.dummy.exit"
      (simulateExitCallWithData instana)
  InstanaSDK.incrementErrorCount instana
  InstanaSDK.addData instana (moreSpanData "entry")
  return $ exitCallResult ++ "::entry done"


simulateExitCallWithData :: InstanaContext -> IO String
simulateExitCallWithData instana = do
  InstanaSDK.addData instana (someSpanData "exit")
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
  InstanaSDK.addToErrorCount instana 2
  InstanaSDK.addData instana (moreSpanData "exit")
  InstanaSDK.addDataAt instana "nested.key1" ("nested.text.value1" :: String)
  InstanaSDK.addDataAt instana "nested.key2" ("nested.text.value2" :: String)
  InstanaSDK.addDataAt instana "nested.key3" (1604 :: Int)
  return "exit done"


lowLevelApiRootEntry ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
lowLevelApiRootEntry instana respond = do
  InstanaSDK.startRootEntry
    instana
    "haskell.dummy.root.entry"
  result <- doExitCall instana
  InstanaSDK.completeEntry instana
  respondWithPlainText respond result


lowLevelApiNonRootEntry ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
lowLevelApiNonRootEntry instana respond = do
  InstanaSDK.startEntry
    instana
    "trace-id"
    "parent-id"
    "haskell.dummy.entry"
  result <- doExitCall instana
  InstanaSDK.completeEntry instana
  respondWithPlainText respond result


doExitCall :: InstanaContext -> IO String
doExitCall instana = do
  InstanaSDK.startExit
    instana
    "haskell.dummy.exit"
  result <- simulateExitCall
  InstanaSDK.completeExit instana
  return result


lowLevelApiWithData ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
lowLevelApiWithData instana respond = do
  InstanaSDK.startRootEntry
    instana
    "haskell.dummy.root.entry"
  InstanaSDK.addData instana (someSpanData "entry")
  result <- doExitCallWithData instana
  InstanaSDK.incrementErrorCount instana
  InstanaSDK.addData instana (moreSpanData "entry")
  InstanaSDK.addDataAt
    instana "nested.entry.key" ("nested.entry.value" :: String)
  InstanaSDK.completeEntry instana
  respondWithPlainText respond result


doExitCallWithData :: InstanaContext -> IO String
doExitCallWithData instana = do
  InstanaSDK.startExit
    instana
    "haskell.dummy.exit"
  InstanaSDK.addData instana (someSpanData "exit")
  result <- simulateExitCall
  InstanaSDK.incrementErrorCount instana
  InstanaSDK.addData instana (moreSpanData "exit")
  InstanaSDK.addDataAt instana "nested.exit.key" ("nested.exit.value" :: String)
  InstanaSDK.completeExit instana
  return result


simulateExitCall :: IO String
simulateExitCall = do
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
  return "exit done"


someSpanData :: String -> Value
someSpanData kind =
   Aeson.object
     [ "data1"     .= ("value1" :: String)
     , "data2"     .= (42 :: Int)
     , "startKind" .= kind
     ]


moreSpanData :: String -> Value
moreSpanData kind =
   Aeson.object
     [ "data2"   .= (1302 :: Int)
     , "data3"   .= ("value3" :: String)
     , "endKind" .= kind
     ]


httpBracketApi ::
  InstanaContext
  -> HTTP.Manager
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
httpBracketApi instana httpManager requestIn respond =
  InstanaSDK.withHttpEntry instana requestIn "haskell.wai.server" $ do
    requestOut <-
      HTTP.parseUrlThrow $
        "http://127.0.0.1:1302/?some=query&parameters=2&pass=secret"
    _ <- InstanaSDK.withHttpExit
      instana
      requestOut
      (\req -> do
        _ <- HTTP.httpLbs req httpManager
        threadDelay $ 1000 -- make sure there is a duration > 0
      )
    respond $
      Wai.responseBuilder
        HTTPTypes.status200
        [("Content-Type", "application/json; charset=UTF-8")]
        "{\"response\": \"ok\"}"


httpLowLevelApi ::
  InstanaContext
  -> HTTP.Manager
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
httpLowLevelApi instana httpManager requestIn respond = do
  InstanaSDK.startHttpEntry instana requestIn "haskell.wai.server"
  requestOut <-
    HTTP.parseUrlThrow $
      "http://127.0.0.1:1302/?some=query&parameters=2&pass=secret"
  requestOut' <- InstanaSDK.startHttpExit instana requestOut
  _ <- HTTP.httpLbs requestOut' httpManager
  threadDelay $ 1000 -- make sure there is a duration > 0
  InstanaSDK.completeExit instana
  result <- respond $
    Wai.responseBuilder
      HTTPTypes.status200
      [("Content-Type", "application/json; charset=UTF-8")]
      "{\"response\": \"ok\"}"
  InstanaSDK.completeEntry instana
  return result


createSingleSpan ::
  InstanaContext
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
createSingleSpan instana requestIn respond = do
  let
    query = Wai.queryString requestIn
    maybeMaybeSpanName = lookup ("spanName" :: ByteString) query
    spanNameByteString =
      Maybe.fromMaybe "haskell.test.span" $ join maybeMaybeSpanName
    spanName = T.pack $ BS.unpack spanNameByteString
  InstanaSDK.withRootEntry instana spanName $ do
    threadDelay $ 1000 -- make sure there is a duration > 0
  respond $
    Wai.responseBuilder
      HTTPTypes.status200
      [("Content-Type", "application/json; charset=UTF-8")]
      "{\"response\": \"ok\"}"


shutDown ::
  (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
shutDown respond = do
  liftIO $ infoM appLogger $ "Wai/Warp app shutdown requested"
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
      ((Warp.setPort 1207) . (Warp.setHost "127.0.0.1")) Warp.defaultSettings
  infoM appLogger $
    "Starting Wai/Warp app at " ++ host ++ ":" ++ show port ++
    " (PID: " ++ show pid ++ ")."
  Warp.runSettings warpSettings $ application instana httpManager pid


initLogging :: IO ()
initLogging = do
  updateGlobalLogger appLogger $ setLevel INFO
  appFileHandler <- fileHandler "wai-warp-app.log" INFO
  appStreamHandler <- streamHandler stdout INFO
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

