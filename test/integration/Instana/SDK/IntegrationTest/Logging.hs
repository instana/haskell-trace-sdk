module Instana.SDK.IntegrationTest.Logging
  ( testLogger
  , initLogging
  ) where


import           System.Environment        (lookupEnv)
import           System.IO                 (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (GenericHandler, fileHandler,
                                            streamHandler)
import           System.Log.Logger         (Priority (..), rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger)


testLogger :: String
testLogger = "IntegrationTest"


initLogging :: IO ()
initLogging = do
  logLevelEnvVar <- lookupEnv "LOG_LEVEL"
  let
    logLevel =
      case logLevelEnvVar of
        Just "DEBUG" -> DEBUG
        _            -> INFO
  updateGlobalLogger testLogger $ setLevel logLevel
  updateGlobalLogger rootLoggerName $ setLevel logLevel
  testFileHandler <- fileHandler "integration-test.log" logLevel
  testStreamHandler <- streamHandler stdout logLevel
  let
    formattedAgentStubFileHandler = withFormatter testFileHandler
    formattedAgentStubStreamHandler = withFormatter testStreamHandler
  updateGlobalLogger testLogger $
    setHandlers [ formattedAgentStubFileHandler ]
  updateGlobalLogger rootLoggerName $
    setHandlers [ formattedAgentStubStreamHandler ]


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where
      timeFormat = "%F %H:%M:%S.%4q %z"
      formatter = tfLogFormatter timeFormat "$time $msg"

