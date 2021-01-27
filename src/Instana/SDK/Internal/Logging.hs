{-|
Module      : Instana.SDK.Internal.Logging
Description : Handles logging
-}
module Instana.SDK.Internal.Logging
  ( initLogger
  , instanaLogger

  -- exposed for testing
  , parseLogLevel
  -- exposed for testing
  , minimumLogLevel
  )
  where


import           Control.Monad             (when)
import           Data.Maybe                (catMaybes, isJust)
import           System.Directory          (getTemporaryDirectory)
import           System.Environment        (lookupEnv)
import           System.IO                 (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (GenericHandler, fileHandler,
                                            streamHandler)
import           System.Log.Logger         (Priority (..), rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger)


{-| Minimum Log level for messages written to the Instana Haskell SDK log file.
If not set, or set to an invalid log level, no log file will be created. If set
to a valid level, a log file named "instana-haskell-sdk.${pid}.log" will be
created in the system temp directory.

If neither this nor INSTANA_LOG_LEVEL_STDOUT are set, the Instana Haskell SDK
will not emit any log messages at all.
-}
logLevelKey :: String
logLevelKey = "INSTANA_LOG_LEVEL"


{-| Minimum Log level for messages written to stdout. This option should only be
used during development. If not set, or set to an invalid log level, no log
messages will be written to stdout.

If neither this nor INSTANA_LOG_LEVEL are set, the Instana Haskell SDK
will not emit any log messages at all.
-}
logLevelStdOutKey :: String
logLevelStdOutKey = "INSTANA_LOG_LEVEL_STDOUT"


{-| If neither this nor INSTANA_LOG_LEVEL are set, this setting is irrelevant
and will be ignored.

Otherwise, if this is set to a non-empty string, a stdout logging handler will
be attached to the root logger instead of the Instana Haskell SDK logger, i. e.:

    updateGlobalLogger rootLoggerName $ setHandlers instanaStdOutHandler

will be called. When INSTANA_LOG_LEVEL_STDOUT is also set, that log level will
be used, otherwise the highest log level (EMERGENCY) will be used for the
handler.

This setting should be used if no other part of the running process (for example
the app which uses the Instana Haskell SDK) has already configured hslogger. In
particular, if this has not been set, the assumption is that "someone else" will
execute something like this with the root logger:

    updateGlobalLogger rootLoggerName $ setHandlers [ ..., appStdOutHandler, ... ]

This is is necessary to avoid emitting all Instana log messages to stdout or to
avoid duplicating log messages on stdout in case INSTANA_LOG_LEVEL_STDOUT is
set.
See https://stackoverflow.com/a/40995265
-}
overrideHsloggerRootHandlerKey :: String
overrideHsloggerRootHandlerKey = "INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER"


-- |The SDK's logger name.
instanaLogger :: String
instanaLogger = "Instana"


-- |Initializes the SDK's logging.
initLogger :: String -> IO ()
initLogger pid = do
  logLevelFileStr <- lookupEnv logLevelKey
  logLevelStdOutStr <- lookupEnv logLevelStdOutKey
  let
    logLevelFile = logLevelFileStr >>= parseLogLevel
    logLevelStdOut = logLevelStdOutStr >>= parseLogLevel

  let
    minLogLevel = minimumLogLevel logLevelFile logLevelStdOut

  case minLogLevel of
    Just minLevel ->
      actuallyInitLogger pid minLevel logLevelFile logLevelStdOut
    Nothing -> do
      return ()


actuallyInitLogger ::
  String ->
  Priority ->
  Maybe Priority ->
  Maybe Priority ->
  IO ()
actuallyInitLogger pid minLogLevel logLevelFile logLevelStdOut = do
  updateGlobalLogger instanaLogger $ setLevel minLogLevel
  logFileHandler <-
    sequence $
      (\logLevel -> createFileHandler pid logLevel) <$> logLevelFile
  stdOutHandler <-
    sequence $
      (\logLevel -> createStdOutHandler logLevel) <$> logLevelStdOut
  setLogHandlers logFileHandler stdOutHandler


createFileHandler :: String -> Priority -> IO (GenericHandler Handle)
createFileHandler pid logLevel = do
  systemTempDir <- getTemporaryDirectory
  let
    systemTempDir' =
      case last systemTempDir of
        '/'  -> systemTempDir
        '\\' -> systemTempDir
        _    -> systemTempDir ++ "/"
    logPath = systemTempDir' ++ "instana-haskell-sdk." ++ pid ++ ".log"
  instanaFileHandler <- fileHandler logPath logLevel
  let
    formattedInstanaFileHandler = withFormatter instanaFileHandler
  return formattedInstanaFileHandler


createStdOutHandler :: Priority -> IO (GenericHandler Handle)
createStdOutHandler logLevel = do
  instanaStreamHandler <- streamHandler stdout logLevel
  let
    formattedInstanaStreamHandler = withFormatter instanaStreamHandler
  return formattedInstanaStreamHandler


setLogHandlers ::
  Maybe (GenericHandler Handle)
  -> Maybe (GenericHandler Handle)
  -> IO ()
setLogHandlers logFileHandler stdOutHandler = do
  overrideHsloggerRootHandlerVal <-
    lookupEnv overrideHsloggerRootHandlerKey
  let
    overrideHsloggerRootHandler =
      isJust overrideHsloggerRootHandlerVal

    handlers =
      catMaybes
        [ logFileHandler
        , if overrideHsloggerRootHandler
          then Nothing
          else stdOutHandler
        ]

  updateGlobalLogger instanaLogger $ setHandlers handlers

  -- override stdout handler on root logger level if requested
  when overrideHsloggerRootHandler
    (do
      actualStdOutHandler <-
        case stdOutHandler of
          Just handler ->
            return handler
          Nothing ->
            createStdOutHandler EMERGENCY
      let
        overrideRootHhandlers = [ actualStdOutHandler ]
      updateGlobalLogger rootLoggerName $ setHandlers overrideRootHhandlers
    )


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    -- http://hackage.haskell.org/packages/archive/hslogger/1.1.4/doc/html/System-Log-Formatter.html
    where
      timeFormat = "%F %H:%M:%S.%4q %z"
      formatter = tfLogFormatter timeFormat "[$time $loggername $pid $prio] $msg"


-- |Parses a string into a hslogger log level.
parseLogLevel :: String -> Maybe Priority
parseLogLevel logLevelStr =
  case logLevelStr of
    "DEBUG"     -> Just DEBUG
    "INFO"      -> Just INFO
    "NOTICE"    -> Just NOTICE
    "WARNING"   -> Just WARNING
    "ERROR"     -> Just ERROR
    "CRITICAL"  -> Just CRITICAL
    "ALERT"     -> Just ALERT
    "EMERGENCY" -> Just EMERGENCY
    _           -> Nothing


-- |Calculates the minimum of two log levels.
minimumLogLevel :: Maybe Priority -> Maybe Priority -> Maybe Priority
minimumLogLevel (Just l1) (Just l2) = Just $ min l1 l2
minimumLogLevel (Just l) Nothing    = Just l
minimumLogLevel Nothing (Just l)    = Just l
minimumLogLevel _ _                 = Nothing

