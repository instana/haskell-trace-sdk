{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.EntrySpan
Description : An entry span
-}
module Instana.SDK.Span.EntrySpan
  ( EntrySpan(..)
  , addData
  , addToErrorCount
  , correlationId
  , correlationType
  , errorCount
  , parentId
  , serviceName
  , setCorrelationId
  , setCorrelationType
  , setServiceName
  , spanData
  , spanId
  , spanName
  , timestamp
  , traceId
  ) where


import           Data.Aeson                    (Value)
import           Data.Text                     (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id       (Id)
import           Instana.SDK.Span.NonRootEntry (NonRootEntry)
import qualified Instana.SDK.Span.NonRootEntry as NonRootEntry
import           Instana.SDK.Span.RootEntry    (RootEntry)
import qualified Instana.SDK.Span.RootEntry    as RootEntry


-- |An entry span.
data EntrySpan =
    RootEntrySpan RootEntry
  | NonRootEntrySpan NonRootEntry
  deriving (Eq, Generic, Show)


-- |Accessor for the trace ID.
traceId :: EntrySpan -> Id
traceId entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanAndTraceId entry
    NonRootEntrySpan entry -> NonRootEntry.traceId entry


-- |Accessor for the span ID.
spanId :: EntrySpan -> Id
spanId entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanAndTraceId entry
    NonRootEntrySpan entry -> NonRootEntry.spanId entry


-- |Parent span ID.
parentId :: EntrySpan -> Maybe Id
parentId entrySpan =
  case entrySpan of
    RootEntrySpan    _     -> Nothing
    NonRootEntrySpan entry -> Just $ NonRootEntry.parentId entry


-- |Name of span.
spanName :: EntrySpan -> Text
spanName entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanName entry
    NonRootEntrySpan entry -> NonRootEntry.spanName entry


-- |Start time.
timestamp :: EntrySpan -> Int
timestamp entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.timestamp entry
    NonRootEntrySpan entry -> NonRootEntry.timestamp entry


-- |Error count (error that occured while this span has been active).
errorCount :: EntrySpan -> Int
errorCount entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.errorCount entry
    NonRootEntrySpan entry -> NonRootEntry.errorCount entry


-- |Add to the error count.
addToErrorCount :: Int -> EntrySpan -> EntrySpan
addToErrorCount increment entrySpan =
  case entrySpan of
    RootEntrySpan    entry ->
      RootEntrySpan $ RootEntry.addToErrorCount increment entry
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.addToErrorCount increment entry


-- |An optional attribute for overriding the name of the service in Instana.
serviceName :: EntrySpan -> Maybe Text
serviceName entrySpan =
  case entrySpan of
    RootEntrySpan entry    -> RootEntry.serviceName entry
    NonRootEntrySpan entry -> NonRootEntry.serviceName entry


-- |Override the name of the service for the associated call in Instana.
setServiceName :: Text -> EntrySpan -> EntrySpan
setServiceName serviceName_ entrySpan =
  case entrySpan of
    RootEntrySpan entry ->
      RootEntrySpan $ RootEntry.setServiceName serviceName_ entry
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.setServiceName serviceName_ entry


-- |The website monitoring correlation type.
correlationType :: EntrySpan -> Maybe Text
correlationType entrySpan =
  case entrySpan of
    RootEntrySpan entry -> RootEntry.correlationType entry
    NonRootEntrySpan _  -> Nothing


-- |Set the website monitoring correlation type. This should only be set on
-- root entry spans. It will be silently ignored for other types of spans.
setCorrelationType :: Text -> EntrySpan -> EntrySpan
setCorrelationType correlationType_ entrySpan =
  case entrySpan of
    RootEntrySpan entry ->
      RootEntrySpan $ RootEntry.setCorrelationType correlationType_ entry
    NonRootEntrySpan _ ->
      entrySpan


-- |The website monitoring correlation ID.
correlationId :: EntrySpan -> Maybe Text
correlationId entrySpan =
  case entrySpan of
    RootEntrySpan entry -> RootEntry.correlationId entry
    NonRootEntrySpan _  -> Nothing


-- |Set the website monitoring correlation ID. This should only be set on
-- root entry spans. It will be silently ignored for other types of spans.
setCorrelationId :: Text -> EntrySpan -> EntrySpan
setCorrelationId correlationId_ entrySpan =
  case entrySpan of
    RootEntrySpan entry ->
      RootEntrySpan $ RootEntry.setCorrelationId correlationId_ entry
    NonRootEntrySpan _ ->
      entrySpan


-- |Optional additional span data.
spanData :: EntrySpan -> Value
spanData entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanData entry
    NonRootEntrySpan entry -> NonRootEntry.spanData entry


-- |Add a value to the span's data section.
addData :: Value -> EntrySpan -> EntrySpan
addData value entrySpan =
  case entrySpan of
    RootEntrySpan    entry ->
      RootEntrySpan $ RootEntry.addData value entry
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.addData value entry

