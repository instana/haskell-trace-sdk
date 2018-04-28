module Instana.SDK.AgentStub.Logging
  ( agentStubLogger
  , initLogging
  ) where


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


logLevel :: Priority
logLevel = INFO


initLogging :: IO ()
initLogging = do
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
    where formatter = simpleLogFormatter "<$time $loggername $prio> $msg"

