module Instana.SDK.AgentStub.Logging
  ( agentStubLogger
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


agentStubLogger :: String
agentStubLogger = "AgentStub"


initLogging :: IO ()
initLogging = do
  logLevelEnvVar <- lookupEnv "LOG_LEVEL"
  let
    logLevel =
      case logLevelEnvVar of
        Just "DEBUG" -> DEBUG
        _            -> INFO
  updateGlobalLogger agentStubLogger $ setLevel logLevel
  updateGlobalLogger rootLoggerName $ setLevel logLevel
  agentStubFileHandler <- fileHandler "agent-stub.log" logLevel
  agentStubStreamHandler <- streamHandler stdout logLevel
  let
    formattedAgentStubFileHandler = withFormatter agentStubFileHandler
    formattedAgentStubStreamHandler = withFormatter agentStubStreamHandler
  updateGlobalLogger agentStubLogger $
    setHandlers [ formattedAgentStubFileHandler ]
  updateGlobalLogger rootLoggerName $
    setHandlers [ formattedAgentStubStreamHandler ]


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where
      timeFormat = "%F %H:%M:%S.%4q %z"
      formatter = tfLogFormatter timeFormat "<$time $loggername $pid $prio> $msg"

