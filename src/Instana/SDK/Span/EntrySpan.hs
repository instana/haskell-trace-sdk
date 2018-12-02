{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.EntrySpan
Description : An entry span
-}
module Instana.SDK.Span.EntrySpan
  ( EntrySpan(..)
  , traceId
  , spanId
  , parentId
  , spanName
  , timestamp
  , errorCount
  , spanData
  , addData
  , addToErrorCount
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

