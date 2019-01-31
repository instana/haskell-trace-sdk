{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.Span
Description : A type class for spans (entries and exits)
-}
module Instana.SDK.Span.Span
  ( Span (Entry, Exit)
  , SpanKind (EntryKind, ExitKind)
  , traceId
  , spanId
  , spanKind
  , parentId
  , spanName
  , timestamp
  , errorCount
  , addToErrorCount
  , spanData
  , addData
  , addDataAt
  ) where


import           Data.Aeson                 (Value, (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.List                  as List
import           Data.Text                  as T
import           Data.Text                  (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id    (Id)
import           Instana.SDK.Span.EntrySpan (EntrySpan)
import qualified Instana.SDK.Span.EntrySpan as EntrySpan
import           Instana.SDK.Span.ExitSpan  (ExitSpan)
import qualified Instana.SDK.Span.ExitSpan  as ExitSpan


-- |The span kind (entry, exit or intermediate).
data SpanKind =
    -- |The monitored componenent receives a call.
    EntryKind
    -- |The monitored componenent calls something else.
  | ExitKind
    -- |An additional annotation that is added to the trace while a traced call
    -- is being processed.
  | IntermediateKind
  deriving (Eq, Generic, Show)


-- |A span.
data Span =
    Entry EntrySpan
  | Exit ExitSpan
  deriving (Eq, Generic, Show)


-- |Accessor for the trace ID.
traceId :: Span -> Id
traceId span_ =
  case span_ of
    Entry entry -> EntrySpan.traceId entry
    Exit exit   -> ExitSpan.traceId exit


-- |Accessor for the span ID.
spanId :: Span -> Id
spanId span_ =
  case span_ of
    Entry entry -> EntrySpan.spanId entry
    Exit exit   -> ExitSpan.spanId exit


-- |Parent span ID.
parentId :: Span -> Maybe Id
parentId span_ =
  case span_ of
    Entry entry -> EntrySpan.parentId entry
    Exit exit   -> Just $ ExitSpan.parentId exit


-- |Name of span.
spanName :: Span -> Text
spanName span_ =
  case span_ of
    Entry entry -> EntrySpan.spanName entry
    Exit exit   -> ExitSpan.spanName exit


-- |Kind of span.
spanKind :: Span -> SpanKind
spanKind span_ =
  case span_ of
    Entry _ -> EntryKind
    Exit _  -> ExitKind


-- |Start time.
timestamp :: Span -> Int
timestamp span_ =
  case span_ of
    Entry entry -> EntrySpan.timestamp entry
    Exit exit   -> ExitSpan.timestamp exit


-- |Start time.
errorCount :: Span -> Int
errorCount span_ =
  case span_ of
    Entry entry -> EntrySpan.errorCount entry
    Exit exit   -> ExitSpan.errorCount exit


-- |Add to the error count.
addToErrorCount :: Int -> Span -> Span
addToErrorCount increment span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.addToErrorCount increment entry
    Exit exit ->
      Exit $ ExitSpan.addToErrorCount increment exit


-- |Optional additional span data.
spanData :: Span -> Value
spanData span_ =
  case span_ of
    Entry entry -> EntrySpan.spanData entry
    Exit exit   -> ExitSpan.spanData exit


-- |Add a value to the span's data section.
addData :: Value -> Span -> Span
addData value span_ =
  case span_ of
    Entry entry -> Entry $ EntrySpan.addData value entry
    Exit exit   -> Exit $ ExitSpan.addData value exit


-- |Add a value at the given path to the span's data section.
addDataAt :: Aeson.ToJSON a => Text -> a -> Span -> Span
addDataAt path value span_ =
  let
    pathSegments = T.splitOn "." path
    newData = List.foldr
      (\nextPathSegment accumulatedAesonValue ->
        Aeson.object [
          nextPathSegment .= accumulatedAesonValue
        ]
      )
      (Aeson.toJSON value)
      pathSegments
  in
  addData newData span_

