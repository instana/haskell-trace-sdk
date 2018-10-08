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
  , spanType
  , timestamp
  , label
  , spanData
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


-- |Type of span.
spanType :: EntrySpan -> Text
spanType entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanType entry
    NonRootEntrySpan entry -> NonRootEntry.spanType entry


-- |Start time.
timestamp :: EntrySpan -> Int
timestamp entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.timestamp entry
    NonRootEntrySpan entry -> NonRootEntry.timestamp entry


-- |Span label.
label :: EntrySpan -> Text
label entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.label entry
    NonRootEntrySpan entry -> NonRootEntry.label entry


-- |Optional additional span data.
spanData :: EntrySpan -> Value
spanData entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanData entry
    NonRootEntrySpan entry -> NonRootEntry.spanData entry

