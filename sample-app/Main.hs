{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent         (threadDelay)
import           Control.Monad              (when)
import           Instana.SDK.SDK            (InstanaContext)
import qualified Instana.SDK.SDK            as InstanaSDK
import           Instana.SDK.Span.EntrySpan (EntrySpan)
import           System.Environment         (lookupEnv)
import           System.IO                  (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple  (GenericHandler, fileHandler,
                                             streamHandler)
import           System.Log.Logger          (Priority (..), rootLoggerName,
                                             setHandlers, setLevel,
                                             updateGlobalLogger)
import           System.Log.Logger          (infoM)
import           Text.Read                  (readMaybe)


appLogger :: String
appLogger = "SampleApp"


main :: IO ()
main = do
  initLogging

  -- Example snippet for using the Instana SDK and relying only on environment
  -- variables/default values for the configuration.
  InstanaSDK.withInstana runApp

  -- Example snippet for using the Instana SDK and providing a configuration
  -- (agent host, port, ...) directly in code (falling back to environment
  -- variables and then to default values). You only need to specify the
  -- configuration values you are interested in and can omit everything else
  -- (see https://www.yesodweb.com/book/settings-types).
  --
  -- let
  --   config =
  --     InstanaSDK.defaultConfig
  --       { InstanaSDK.agentHost = Just "127.0.0.1"
  --       , InstanaSDK.agentPort = Just 42699
  --       , InstanaSDK.forceTransmissionAfter = Just 1000
  --       , InstanaSDK.forceTransmissionStartingAt = Just 500
  --       , InstanaSDK.maxBufferedSpans = Just 1000
  --       }
  -- InstanaSDK.withConfiguredInstana config runApp


runApp :: InstanaContext -> IO ()
runApp instana = do
  startKeepAliveLoop instana


data KeepAlive =
    No
  | Indefinitely
  | Seconds Int

{-| Keeps the process alive by scheduling an endless loop. The intention is that
the agent has a chance to find the announcing process in the OS' process list.
-}
startKeepAliveLoop :: InstanaContext -> IO ()
startKeepAliveLoop instana = do
  keepAliveSetting <- lookupEnv "KEEP_ALIVE"
  let
    keepAliveMax =
      if keepAliveSetting == Just "false"
        then No
        else
          case keepAliveSetting of
            Nothing -> Indefinitely
            Just envVal ->
               case readMaybe envVal of
                 Nothing        -> Indefinitely
                 Just parsedInt -> Seconds parsedInt
  case keepAliveMax of
    No -> do
      -- keep alive disabled, return no op to terminate immediately
      infoM appLogger "KEEP_ALIVE expicitly disabled, terminating."
      return ()
    Seconds s -> do
      infoM appLogger $ "Keeping process alive for " ++ show s ++ " seconds."
      keepAliveLoop instana (s * 10) 1
    Indefinitely -> do
      infoM appLogger $ "Keeping process alive indefinitely " ++
            "(KEEP_ALIVE not explicitly specified or not parseable)."
      keepAliveLoop instana (-1) 1


keepAliveLoop :: InstanaContext -> Int -> Int -> IO ()
keepAliveLoop instana maxIterations counter = do

  when (counter `mod` 20 == 0) $ do
    createSpansUsingLowLevelApi instana
  when (counter + 10 `mod` 20 == 0) $ do
    createSpansUsingHighLevelApi instana

  when (counter `mod` 100 == 0) $ do
    let
      maxStr =
        if maxIterations < 0
          then ""
          else ("/" ++ (show (maxIterations `div` 10)))
    infoM appLogger $
      "waiting [" ++ show (counter `div` 10) ++ maxStr ++ "]"

  -- delay by 100 ms
  threadDelay $ 100 * 1000

  if (maxIterations >= 0 && counter >= maxIterations)
  then do
    infoM appLogger $
      "waited for " ++ (show (maxIterations `div` 10)) ++
        " seconds, now terminating"
    return ()
  else do
    keepAliveLoop instana maxIterations (counter + 1)


createSpansUsingHighLevelApi :: InstanaContext -> IO ()
createSpansUsingHighLevelApi instana = do
  InstanaSDK.withRootEntrySimple
    instana
    "haskell.dummy.root.entry"
    (simulateExitCall instana)
  return ()


simulateExitCall :: InstanaContext -> EntrySpan -> IO ()
simulateExitCall instana entrySpan =
  InstanaSDK.withExitSimple
    instana
    entrySpan
    "haskell.dummy.exit"
    (putStrLn "Here be dragons!")


createSpansUsingLowLevelApi :: InstanaContext -> IO ()
createSpansUsingLowLevelApi instana = do
  -- Start an entry span (you would need to check if a trace ID and parent
  -- span ID are already available, for example from HTTP headers of an
  -- incoming request, and, if so, pass them on using InstanaSDK.startEntry
  -- instead of InstanaSDK.startRootEntry
  entrySpan <-
    InstanaSDK.startRootEntry
      "haskell.dummy.root.entry"
  exitSpan <-
    InstanaSDK.startExit
      entrySpan
      "haskell.dummy.exit"

  -- Now a real app would execute some exit call, say a DB call or an
  -- HTTP call.

  -- mark the exit call as completed
  InstanaSDK.completeExit instana exitSpan 0

  -- mark the entry as completed and return
  InstanaSDK.completeEntry instana entrySpan 0

  return ()


initLogging :: IO ()
initLogging = do
  updateGlobalLogger appLogger $ setLevel INFO
  appFileHandler <- fileHandler "sample-app.log" INFO
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
    where formatter = simpleLogFormatter "{$time $loggername $prio} $msg"

