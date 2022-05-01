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
  , setSynthetic
  , setTpFlag
  , setW3cTraceContext
  , spanData
  , spanId
  , spanName
  , synthetic
  , tpFlag
  , timestamp
  , traceId
  , w3cTraceContext
  ) where


import           Data.Text                            (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id              (Id)
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)
import           Instana.SDK.Span.NonRootEntry        (NonRootEntry)
import qualified Instana.SDK.Span.NonRootEntry        as NonRootEntry
import           Instana.SDK.Span.RootEntry           (RootEntry)
import qualified Instana.SDK.Span.RootEntry           as RootEntry
import           Instana.SDK.Span.SpanData            (Annotation, SpanData)


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


-- |The synthetic flag.
synthetic :: EntrySpan -> Bool
synthetic entrySpan =
  case entrySpan of
    RootEntrySpan entry    -> RootEntry.synthetic entry
    NonRootEntrySpan entry -> NonRootEntry.synthetic entry


-- |The W3C Trace Context. An entry span only has an associated W3C trace
-- context, if W3C trace context headers have been received. In contrast,
-- exit spans always have an associated W3C trace context.
w3cTraceContext :: EntrySpan -> Maybe W3CTraceContext
w3cTraceContext entrySpan =
  case entrySpan of
    RootEntrySpan entry    -> RootEntry.w3cTraceContext entry
    NonRootEntrySpan entry -> NonRootEntry.w3cTraceContext entry


-- |Attaches a W3C trace context to the span.
setW3cTraceContext :: W3CTraceContext -> EntrySpan -> EntrySpan
setW3cTraceContext w3cTraceContext_ entrySpan =
   case entrySpan of
    RootEntrySpan entry ->
      RootEntrySpan $ RootEntry.setW3cTraceContext w3cTraceContext_ entry
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.setW3cTraceContext w3cTraceContext_ entry


-- |The span.tp flag. A span with span.tp = True has inherited the trace ID/
-- parent ID from W3C trace context instead of Instana headers. Only valid for
-- entry spans.
tpFlag :: EntrySpan -> Bool
tpFlag entrySpan =
  case entrySpan of
    RootEntrySpan _        -> False
    NonRootEntrySpan entry -> NonRootEntry.tpFlag entry


-- |Set the span.tp flag. A span with span.tp = True has inherited the trace ID/
-- parent ID from W3C trace context instead of Instana headers. Only valid for
-- non-root entry spans, will be silently ignored for root entry spans and exit
-- spans.
setTpFlag :: EntrySpan -> EntrySpan
setTpFlag entrySpan =
  case entrySpan of
    RootEntrySpan _        ->
      entrySpan
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.setTpFlag entry


-- |Set the synthetic flag. This should only be set on entry spans. It will be
-- silently ignored for other types of spans.
setSynthetic :: Bool -> EntrySpan -> EntrySpan
setSynthetic synthetic_ entrySpan =
  case entrySpan of
    RootEntrySpan entry ->
      RootEntrySpan $ RootEntry.setSynthetic synthetic_ entry
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.setSynthetic synthetic_ entry


-- |Optional additional span data.
spanData :: EntrySpan -> SpanData
spanData entrySpan =
  case entrySpan of
    RootEntrySpan    entry -> RootEntry.spanData entry
    NonRootEntrySpan entry -> NonRootEntry.spanData entry


-- |Add a value to the span's data section.
addData :: Annotation-> EntrySpan -> EntrySpan
addData value entrySpan =
  case entrySpan of
    RootEntrySpan    entry ->
      RootEntrySpan $ RootEntry.addData value entry
    NonRootEntrySpan entry ->
      NonRootEntrySpan $ NonRootEntry.addData value entry

