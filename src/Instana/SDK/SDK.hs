{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.SDK
Description : The main API of the Instana Haskell Trace SDK.

Instana.SDK.SDK is the main API of the Instana Haskell Trace SDK. Use one of
initInstana, initConfiguredInstana, withInstana, or withConfiguredInstana to get
an InstanaContext. Then use the context with any of the withRootEntry,
withEntry, withExit functions for tracing.
-}
module Instana.SDK.SDK
    ( Config
    , InstanaContext
    , agentHost
    , agentPort
    , agentName
    , completeEntry
    , completeEntryWithData
    , completeExit
    , completeExitWithData
    , defaultConfig
    , forceTransmissionAfter
    , forceTransmissionStartingAt
    , maxBufferedSpans
    , initInstana
    , initConfiguredInstana
    , startEntry
    , startEntryWithData
    , startExit
    , startExitWithData
    , startRootEntry
    , startRootEntryWithData
    , withConfiguredInstana
    , withEntrySimple
    , withEntry
    , withExitSimple
    , withExit
    , withInstana
    , withRootEntrySimple
    , withRootEntry
    ) where


import qualified Control.Concurrent.STM        as STM
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (Value)
import qualified Data.Aeson                    as Aeson
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import qualified Network.HTTP.Client           as HTTP
import qualified Network.Socket                as Socket
import qualified System.Posix.Process          as Process

import           Instana.SDK.Config
import           Instana.SDK.Internal.Command  (Command)
import qualified Instana.SDK.Internal.Command  as Command
import           Instana.SDK.Internal.Config   (FinalConfig)
import qualified Instana.SDK.Internal.Config   as InternalConfig
import           Instana.SDK.Internal.Context  (ConnectionState (..), InternalContext (InternalContext))
import qualified Instana.SDK.Internal.Context  as InternalContext
import qualified Instana.SDK.Internal.Id       as Id
import qualified Instana.SDK.Internal.Logging  as Logging
import qualified Instana.SDK.Internal.Worker   as Worker
import           Instana.SDK.Span.EntrySpan    (EntrySpan (..))
import           Instana.SDK.Span.ExitSpan     (ExitSpan (ExitSpan))
import qualified Instana.SDK.Span.ExitSpan     as ExitSpan
import           Instana.SDK.Span.NonRootEntry (NonRootEntry (NonRootEntry))
import qualified Instana.SDK.Span.NonRootEntry as NonRootEntry
import           Instana.SDK.Span.RootEntry    (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry    as RootEntry


{-| A container for all the things the Instana SDK needs to do its work.
-}
type InstanaContext = InternalContext


{-| Initializes the Instana SDK and the connection to the Instana agent.

The configuration is read from the environment, falling back to default values.
-}
initInstana :: IO InstanaContext
initInstana = do
  conf <- InternalConfig.readConfigFromEnvironmentAndApplyDefaults
  initInstanaInternal conf


{-| Initializes the Instana SDK and the connection to the Instana agent, then
calls the given function with the established connection.

The configuration is read from the environment, falling back to default values.
-}
withInstana :: MonadIO m => (InstanaContext -> m a) -> m a
withInstana fn = do
  conf <- liftIO InternalConfig.readConfigFromEnvironmentAndApplyDefaults
  withInstanaInternal conf fn


{-| Initializes the Instana SDK and the connection to the Instana agent, using the given Instana configuration.

Configuration settings that have not been set in the given configuration are
read from the environment, falling back to default values.
-}
initConfiguredInstana :: Config -> IO InstanaContext
initConfiguredInstana conf  = do
  confFromEnv <- InternalConfig.readConfigFromEnvironment
  let
     mergedConf = InternalConfig.mergeConfigs conf confFromEnv
  initInstanaInternal mergedConf


{-| Initializes the Instana SDK and the connection to the Instana agent, then
calls the given function with the established connection, using the given
Instana configuration.

Configuration settings that have not been set in the given configuration are
read from the environment, falling back to default values.
-}
withConfiguredInstana :: MonadIO m => Config -> (InstanaContext -> m a) -> m a
withConfiguredInstana conf fn = do
  confFromEnv <- liftIO $ InternalConfig.readConfigFromEnvironment
  let
     mergedConf = InternalConfig.mergeConfigs conf confFromEnv
  withInstanaInternal mergedConf fn


withInstanaInternal ::
  MonadIO m =>
  FinalConfig
  -> (InstanaContext -> m a)
  -> m a
withInstanaInternal conf fn = do
  context <- liftIO $ initInstanaInternal conf
  fn context


initInstanaInternal :: FinalConfig -> IO InstanaContext
initInstanaInternal conf = do
  pid <- Process.getProcessID
  Logging.initLogger $ show pid
  commandQueue <- STM.newTQueueIO
  spanQueue <- STM.newTVarIO $ Seq.empty
  connectionState <- STM.newTVarIO $ Unconnected
  fileDescriptor <- STM.newTVarIO $ Nothing
  -- HTTP.newManager is keep-alive by default (10 connections, we set it to 5)
  manager <- HTTP.newManager $
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 5
      , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ 5000 * 1000
      , HTTP.managerRawConnection =
          HTTP.rawConnectionModifySocket
            (\socket -> do
                let
                  fileDescriptorFromSocket = Socket.fdSocket socket
                STM.atomically $
                  STM.writeTVar fileDescriptor (Just fileDescriptorFromSocket)
            )
      }
  let
    context =
      InternalContext
        { InternalContext.config = conf
        , InternalContext.httpManager = manager
        , InternalContext.commandQueue = commandQueue
        , InternalContext.spanQueue = spanQueue
        , InternalContext.connectionState = connectionState
        , InternalContext.fileDescriptor = fileDescriptor
        }
  -- The worker thread will also try to establish the connection to the agent
  -- and only start its work when that was successful.
  Worker.spawnWorker context
  return context


-- |Wraps an IO action in 'startRootEntry' and 'completeEntry'. The wrapped
-- action receives the entry span in case it wants to add child spans to it.
withRootEntrySimple ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> Text
  -> (EntrySpan -> m a)
  -> m a
withRootEntrySimple context spanType label io =
  withRootEntry
    context
    spanType
    label
    emptyValue
    (\entrySpan ->
      (io entrySpan >>= (\result -> return (result, False, emptyValue)))
    )


-- |Wraps an IO action in 'startRootEntryWithData' and 'completeEntryWithData'.
-- The wrapped action receives the entry span in case it wants to add child
-- spans to it.
withRootEntry ::
  MonadIO m =>
  InstanaContext
  -> Text
  -> Text
  -> Value
  -> (EntrySpan -> m (a, Bool, Value))
  -> m a
withRootEntry context spanType label spanDataStart io = do
  entrySpan <- liftIO $ startRootEntryWithData spanType label spanDataStart
  (result, spanError, spanDataEnd) <- io entrySpan
  liftIO $ completeEntryWithData context entrySpan spanError spanDataEnd
  return result


-- |Creates a preliminary/incomplete root entry span, which should later be
-- completed with 'completeEntry' or 'completeEntryWithData'.
startRootEntry ::
  Text
  -> Text
  -> IO EntrySpan
startRootEntry spanType label =
  startRootEntryWithData spanType label emptyValue


-- |Creates a preliminary/incomplete root entry span with additional data, which
-- should later be completed with 'completeEntry' or 'completeEntryWithData'.
startRootEntryWithData ::
  Text
  -> Text
  -> Value
  -> IO EntrySpan
startRootEntryWithData spanType label spanData = do
  timestamp <- round . (* 1000) <$> getPOSIXTime
  traceId <- Id.generate
  return $
    RootEntrySpan $
      RootEntry
        { RootEntry.spanAndTraceId = traceId
        , RootEntry.spanType       = spanType
        , RootEntry.timestamp      = timestamp
        , RootEntry.label          = label
        , RootEntry.spanData       = spanData
        }


-- |Wraps an IO action in 'startEntry' and 'completeEntry'. The wrapped action
-- receives the entry span in case it wants to add child spans to it.
withEntrySimple ::
  MonadIO m =>
  InstanaContext
  -> String
  -> String
  -> Text
  -> Text
  -> (EntrySpan -> m a)
  -> m a
withEntrySimple context traceId parentId spanType label io =
  withEntry
    context
    traceId
    parentId
    spanType
    label
    emptyValue
    (\entrySpan ->
      (io entrySpan >>= (\result -> return (result, False, emptyValue)))
    )


-- |Wraps an IO action in 'startEntryWithData' and 'completeEntryWithData'. The
-- wrapped action receives the entry span in case it wants to add child spans to
-- it.
withEntry ::
  MonadIO m =>
  InstanaContext
  -> String
  -> String
  -> Text
  -> Text
  -> Value
  -> (EntrySpan -> m (a, Bool, Value))
  -> m a
withEntry context traceId parentId spanType label spanDataStart io = do
  entrySpan <- liftIO $ startEntryWithData traceId parentId spanType label spanDataStart
  (result, spanError, spanDataEnd) <- io entrySpan
  liftIO $ completeEntryWithData context entrySpan spanError spanDataEnd
  return result


-- |Creates a preliminary/incomplete entry span, which should later be completed
-- by calling 'completeEntry' or 'completeEntryWithData'.
startEntry ::
  String
  -> String
  -> Text
  -> Text
  -> IO EntrySpan
startEntry traceId parentId spanType label =
  startEntryWithData traceId parentId spanType label emptyValue


-- |Creates a preliminary/incomplete entry span with additional data, which
-- should later be completed by calling 'completeEntry' or
-- 'completeEntryWithData'.
startEntryWithData ::
  String
  -> String
  -> Text
  -> Text
  -> Value
  -> IO EntrySpan
startEntryWithData traceId parentId spanType label spanData = do
  timestamp <- round . (* 1000) <$> getPOSIXTime
  spanId <- Id.generate
  return $
    NonRootEntrySpan $
      NonRootEntry
        { NonRootEntry.traceId   = Id.fromString traceId
        , NonRootEntry.spanId    = spanId
        , NonRootEntry.parentId  = Id.fromString parentId
        , NonRootEntry.spanType  = spanType
        , NonRootEntry.timestamp = timestamp
        , NonRootEntry.label     = label
        , NonRootEntry.spanData  = spanData
        }


-- |Completes an entry span, to be called at the last possible moment before the
-- call has been processed completely.
completeEntry ::
  InstanaContext
  -> EntrySpan
  -> Bool
  -> IO ()
completeEntry context entrySpan spanError =
  enqueueCommand
    context
    (Command.CompleteEntry entrySpan spanError)


-- |Completes an entry span with addtional data, to be called at the last
-- possible moment before the call has been processed completely.
completeEntryWithData ::
  InstanaContext
  -> EntrySpan
  -> Bool
  -> Value
  -> IO ()
completeEntryWithData context entrySpan spanError spanData =
  enqueueCommand
    context
    (Command.CompleteEntryWithData entrySpan spanError spanData)


-- |Wraps an IO action in 'startExit' and 'completeExit'.
withExitSimple ::
  MonadIO m =>
  InstanaContext
  -> EntrySpan
  -> Text
  -> Text
  -> m a
  -> m a
withExitSimple context parent spanType label io =
    withExit
      context
      parent
      spanType
      label
      emptyValue
      (io >>= (\result -> return (result, False, emptyValue)))


-- |Wraps an IO action in 'startExitWithData' and 'completeExitWithData'.
withExit ::
  MonadIO m =>
  InstanaContext
  -> EntrySpan
  -> Text
  -> Text
  -> Value
  -> m (a, Bool, Value)
  -> m a
withExit context parent spanType label spanDataStart io = do
  exitSpan <- liftIO $ startExitWithData parent spanType label spanDataStart
  (result, spanError, spanDataEnd) <- io
  liftIO $ completeExitWithData context exitSpan spanError spanDataEnd
  return result


-- |Creates a preliminary/incomplete exit span, which should later be completed
-- with 'completeExit' or 'completeExitWithData'.
startExit ::
  EntrySpan
  -> Text
  -> Text
  -> IO ExitSpan
startExit parent spanType label =
  startExitWithData parent spanType label emptyValue


-- |Creates a preliminary/incomplete exit span with additional data, which
-- should later be completed with 'completeExit' or 'completeExitWithData'.
startExitWithData ::
  EntrySpan
  -> Text
  -> Text
  -> Value
  -> IO ExitSpan
startExitWithData parent spanType label spanData = do
  timestamp <- round . (* 1000) <$> getPOSIXTime
  return $
    ExitSpan
      { ExitSpan.parentSpan  = parent
      , ExitSpan.spanType    = spanType
      , ExitSpan.timestamp   = timestamp
      , ExitSpan.label       = label
      , ExitSpan.spanData    = spanData
      }


-- |Completes an exit span, to be called as soon as the remote call has
-- returned.
completeExit ::
  InstanaContext
  -> ExitSpan
  -> Bool
  -> IO ()
completeExit context exitSpan spanError =
  enqueueCommand
    context
    (Command.CompleteExit exitSpan spanError)


-- |Completes an exit span with addtional data, to be called as soon as the
-- remote call has returned.
completeExitWithData ::
  InstanaContext
  -> ExitSpan
  -> Bool
  -> Value
  -> IO ()
completeExitWithData context exitSpan spanError spanData =
  enqueueCommand
    context
    (Command.CompleteExitWithData exitSpan spanError spanData)


emptyValue :: Value
emptyValue = Aeson.object []


enqueueCommand :: InstanaContext -> Command -> IO ()
enqueueCommand context command = do
  -- TODO Maybe we better should use a bounded queue and drop stuff if we can't
  -- keep up. For now, this is an unbounded queue that might turn into a memory
  -- leak if a lot of spans are written and the HTTP requests to the agent can't
  -- keep up.
  let
    commandQueue = InternalContext.commandQueue context
  STM.atomically $ STM.writeTQueue commandQueue command

