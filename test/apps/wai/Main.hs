{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent         (threadDelay)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Binary.Builder        as Builder
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import           Instana.SDK.SDK            (InstanaContext)
import qualified Instana.SDK.SDK            as InstanaSDK
import           Instana.SDK.Span.SpanData  (Annotation)
import qualified Instana.SDK.Span.SpanData  as SpanData
import qualified Instana.SDK.Span.SpanType  as SpanType
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
    ("POST", ["bracket", "api", "with-tags"]) ->
      bracketApiWithTags instana respond
    ("POST", ["bracket", "api", "with-service-name"]) ->
      bracketApiWithServiceName instana respond
    ("POST", ["bracket", "api", "with-service-name-exit-only"]) ->
      bracketApiWithServiceNameExitOnly instana respond
    ("POST", ["low", "level", "api", "root"]) ->
      lowLevelApiRootEntry instana respond
    ("POST", ["low", "level", "api", "non-root"]) ->
      lowLevelApiNonRootEntry instana respond
    ("POST", ["low", "level", "api", "with-tags"]) ->
      lowLevelApiWithTags instana respond
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


bracketApiWithTags ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
bracketApiWithTags instana respond = do
  entryCallResult <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (withExitWithTags instana)
  respondWithPlainText respond $ entryCallResult


withExitWithTags :: InstanaContext -> IO String
withExitWithTags instana = do
  addTags instana (someSpanData "entry")
  exitCallResult <-
    InstanaSDK.withExit
      instana
      "haskell.dummy.exit"
      (simulateExitCallWithTags instana)
  InstanaSDK.incrementErrorCount instana
  addTags instana (moreSpanData "entry")
  return $ exitCallResult ++ "::entry done"


simulateExitCallWithTags :: InstanaContext -> IO String
simulateExitCallWithTags instana = do
  addTags instana (someSpanData "exit")
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
  InstanaSDK.addToErrorCount instana 2
  addTags instana (moreSpanData "exit")
  InstanaSDK.addTagAt instana "nested.key1" ("nested.text.value1" :: String)
  InstanaSDK.addTagAt instana "nested.key2" ("nested.text.value2" :: String)
  InstanaSDK.addTagAt instana "nested.key3" (1604 :: Int)
  return "exit done"


bracketApiWithServiceName ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
bracketApiWithServiceName instana respond = do
  entryCallResult <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (withExitWithServiceName instana)
  respondWithPlainText respond $ entryCallResult


withExitWithServiceName :: InstanaContext -> IO String
withExitWithServiceName instana = do
  InstanaSDK.setServiceName instana "Service Entry"
  exitCallResult <-
    InstanaSDK.withExit
      instana
      "haskell.dummy.exit"
      (simulateExitCallWithServiceName instana)
  return $ exitCallResult ++ "::entry done"


bracketApiWithServiceNameExitOnly ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
bracketApiWithServiceNameExitOnly instana respond = do
  entryCallResult <-
    InstanaSDK.withRootEntry
      instana
      "haskell.dummy.root.entry"
      (withExitWithServiceNameExitOnly instana)
  respondWithPlainText respond $ entryCallResult


withExitWithServiceNameExitOnly :: InstanaContext -> IO String
withExitWithServiceNameExitOnly instana = do
  exitCallResult <-
    InstanaSDK.withExit
      instana
      "haskell.dummy.exit"
      (simulateExitCallWithServiceName instana)
  return $ exitCallResult ++ "::entry done"


simulateExitCallWithServiceName :: InstanaContext -> IO String
simulateExitCallWithServiceName instana = do
  InstanaSDK.setServiceName instana "Service Exit"
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
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


lowLevelApiWithTags ::
  InstanaContext
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
lowLevelApiWithTags instana respond = do
  InstanaSDK.startRootEntry
    instana
    "haskell.dummy.root.entry"
  addTags instana (someSpanData "entry")
  result <- doExitCallWithTags instana
  InstanaSDK.incrementErrorCount instana
  addTags instana (moreSpanData "entry")
  InstanaSDK.addTagAt
    instana "nested.entry.key" ("nested.entry.value" :: String)
  InstanaSDK.completeEntry instana
  respondWithPlainText respond result


doExitCallWithTags :: InstanaContext -> IO String
doExitCallWithTags instana = do
  InstanaSDK.startExit
    instana
    "haskell.dummy.exit"
  addTags instana (someSpanData "exit")
  result <- simulateExitCall
  InstanaSDK.incrementErrorCount instana
  addTags instana (moreSpanData "exit")
  InstanaSDK.addTagAt instana "nested.exit.key" ("nested.exit.value" :: String)
  InstanaSDK.completeExit instana
  return result


simulateExitCall :: IO String
simulateExitCall = do
  -- some time needs to pass, otherwise the exit span's duration will be 0
  threadDelay $ 10 * 1000
  return "exit done"


addTags :: InstanaContext -> [Annotation] -> IO ()
addTags instana annotations =
  mapM_
    (\annotation -> InstanaSDK.addTag instana annotation)
    annotations


someSpanData :: String -> [Annotation]
someSpanData kind =
  [ SpanData.simpleAnnotation "data1"     ("value1" :: String)
  , SpanData.simpleAnnotation "data2"     (42 :: Int)
  , SpanData.simpleAnnotation "startKind" kind
  ]


moreSpanData :: String -> [Annotation]
moreSpanData kind =
  [ SpanData.simpleAnnotation "data2"   (1302 :: Int)
  , SpanData.simpleAnnotation "data3"   ("value3" :: String)
  , SpanData.simpleAnnotation "endKind" kind
  ]


httpBracketApi ::
  InstanaContext
  -> HTTP.Manager
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
httpBracketApi instana httpManager requestIn respond = do
  response <- do
    InstanaSDK.withHttpEntry instana requestIn $ do
      downstreamRequest <-
        HTTP.parseUrlThrow $
          "http://127.0.0.1:1208/echo?some=query&parameters=2&pass=secret"
      downstreamResponse <- InstanaSDK.withHttpExit
        instana
        (addDowntreamRequestHeaders downstreamRequest)
        (\req -> do
          downstreamRespone' <- HTTP.httpLbs req httpManager
          threadDelay $ 1000 -- make sure there is a duration > 0
          return downstreamRespone'
        )
      return $
        Wai.responseLBS
          HTTPTypes.status200
          [ ("Content-Type", "application/json; charset=UTF-8")
          , ("X-Response-Header-On-Entry", "response header on entry value")
          ]
          (HTTP.responseBody downstreamResponse)
  respond response


httpLowLevelApi ::
  InstanaContext
  -> HTTP.Manager
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
httpLowLevelApi instana httpManager requestIn respond = do
  InstanaSDK.startHttpEntry instana requestIn
  downstreamRequest <-
    HTTP.parseUrlThrow $
      "http://127.0.0.1:1208/echo?some=query&parameters=2&pass=secret"
  downstreamRequest' <-
    InstanaSDK.startHttpExit instana $
      addDowntreamRequestHeaders downstreamRequest
  downstreamResponse <- HTTP.httpLbs downstreamRequest' httpManager
  threadDelay $ 1000 -- make sure there is a duration > 0
  InstanaSDK.completeExit instana
  let
    response =
      Wai.responseLBS
        HTTPTypes.status200
        [ ("Content-Type", "application/json; charset=UTF-8")
        , ("X-Response-Header-On-Entry", "response header on entry value")
        ]
        (HTTP.responseBody downstreamResponse)
  response' <-
    InstanaSDK.postProcessHttpResponse instana response
  result <- respond response'
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
    spanType = SpanType.SdkSpan $ T.pack $ BS.unpack spanNameByteString
  InstanaSDK.withRootEntry instana spanType $ do
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


addDowntreamRequestHeaders :: HTTP.Request -> HTTP.Request
addDowntreamRequestHeaders request =
  request {
    HTTP.requestHeaders =
      [ ("X-Request-Header-On-Exit", "request header on exit value") ]
  }


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
    "Starting Wai/Warp app at " ++ host ++ ":" ++ show port ++
    " (PID: " ++ show pid ++ ")."
  Warp.runSettings warpSettings $ application instana httpManager pid


initLogging :: IO ()
initLogging = do
  logLevelEnvVar <- lookupEnv "APP_LOG_LEVEL"
  let
    logLevel =
      case logLevelEnvVar of
        Just "DEBUG" -> DEBUG
        _            -> INFO
  updateGlobalLogger appLogger $ setLevel logLevel
  appFileHandler <- fileHandler "wai-warp-app.log" logLevel
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

