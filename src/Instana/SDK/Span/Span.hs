{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.Span
Description : A type for spans (entries, exits or intermediates).
-}
module Instana.SDK.Span.Span
  ( Span (Entry, Exit)
  , SpanKind (EntryKind, ExitKind, IntermediateKind)
  , addRegisteredData
  , addRegisteredDataAt
  , addTag
  , addTagAt
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
  , setW3cTraceContext
  , spanData
  , setTpFlag
  , spanId
  , spanKind
  , spanName
  , synthetic
  , timestamp
  , tpFlag
  , traceId
  , w3cTraceContext
  ) where


import           Data.Aeson                           (Value, (.=))
import qualified Data.Aeson                           as Aeson
import qualified Data.List                            as List
import           Data.Text                            as T
import           GHC.Generics

import           Instana.SDK.Internal.Id              (Id)
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)
import           Instana.SDK.Span.EntrySpan           (EntrySpan)
import qualified Instana.SDK.Span.EntrySpan           as EntrySpan
import           Instana.SDK.Span.ExitSpan            (ExitSpan)
import qualified Instana.SDK.Span.ExitSpan            as ExitSpan


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


-- |Error count.
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


-- |An optional attribute for overriding the name of the service in Instana.
serviceName :: Span -> Maybe Text
serviceName span_ =
  case span_ of
    Entry entry -> EntrySpan.serviceName entry
    Exit exit   -> ExitSpan.serviceName exit


-- |Override the name of the service for the associated call in Instana.
setServiceName :: Text -> Span -> Span
setServiceName serviceName_ span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.setServiceName serviceName_ entry
    Exit exit ->
      Exit $ ExitSpan.setServiceName serviceName_ exit


-- |The website monitoring correlation type.
correlationType :: Span -> Maybe Text
correlationType span_ =
  case span_ of
    Entry entry -> EntrySpan.correlationType entry
    Exit _      -> Nothing


-- |Set the website monitoring correlation type. This should only be set on
-- root entry spans. It will be silently ignored for other types of spans.
setCorrelationType :: Text -> Span -> Span
setCorrelationType correlationType_ span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.setCorrelationType correlationType_ entry
    Exit _ ->
      span_


-- |The website monitoring correlation ID.
correlationId :: Span -> Maybe Text
correlationId span_ =
  case span_ of
    Entry entry -> EntrySpan.correlationId entry
    Exit _      -> Nothing


-- |Set the website monitoring correlation ID. This should only be set on
-- root entry spans. It will be silently ignored for other types of spans.
setCorrelationId :: Text -> Span -> Span
setCorrelationId correlationId_ span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.setCorrelationId correlationId_ entry
    Exit _ ->
      span_


-- |The W3C Trace Context. An entry span only has an associated W3C trace
-- context, if W3C trace context headers have been received. In contrast,
-- exit spans always have an associated W3C trace context.
w3cTraceContext :: Span -> Maybe W3CTraceContext
w3cTraceContext span_ =
  case span_ of
    Entry entry -> EntrySpan.w3cTraceContext entry
    Exit exit   -> Just $ ExitSpan.w3cTraceContext exit


-- |Attaches a W3C trace context to the span.
setW3cTraceContext :: W3CTraceContext -> Span -> Span
setW3cTraceContext w3cTraceContext_ span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.setW3cTraceContext w3cTraceContext_ entry
    Exit exit ->
      Exit $ ExitSpan.setW3cTraceContext w3cTraceContext_ exit


-- |The span.tp flag. A span with span.tp = True has inherited the trace ID/
-- parent ID from W3C trace context instead of Instana headers. Only valid for
-- non-root entry spans.
tpFlag :: Span -> Bool
tpFlag span_ =
  case span_ of
    Entry entry -> EntrySpan.tpFlag entry
    Exit _      -> False


-- |Set the span.tp flag. A span with span.tp = True has inherited the trace ID/
-- parent ID from W3C trace context instead of Instana headers. Only valid for
-- non-root entry spans, will be silently ignored for root entry spans and exit
-- spans.
setTpFlag :: Span -> Span
setTpFlag span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.setTpFlag entry
    Exit _ ->
      span_


-- |The synthetic flag.
synthetic :: Span -> Bool
synthetic span_ =
  case span_ of
    Entry entry -> EntrySpan.synthetic entry
    Exit _      -> False


-- |Set the synthetic flag. This should only be set on entry spans. It will be
-- silently ignored for other types of spans.
setSynthetic :: Bool -> Span -> Span
setSynthetic synthetic_ span_ =
  case span_ of
    Entry entry ->
      Entry $ EntrySpan.setSynthetic synthetic_ entry
    Exit _ ->
      span_


-- |Optional additional span data.
spanData :: Span -> Value
spanData span_ =
  case span_ of
    Entry entry -> EntrySpan.spanData entry
    Exit exit   -> ExitSpan.spanData exit


-- |Add a value to the span's custom tags section. This should be used for SDK
-- spans instead of addRegisteredData.
addTag :: Value -> Span -> Span
addTag value span_ =
  addRegisteredDataAt "sdk.custom.tags" value span_


-- |Add a value to the given path to the span's custom tags section. This should
-- be used for SDK spans instead of addRegisteredDataAt.
addTagAt :: Aeson.ToJSON a => Text -> a -> Span -> Span
addTagAt path value span_ =
  addRegisteredDataAt (T.concat ["sdk.custom.tags.", path]) value span_



-- |Add a value to the span's data section. This should only be used for
-- registered spans, not for SDK spans. For SDK spans, you should use addTag
-- instead.
addRegisteredData :: Value -> Span -> Span
addRegisteredData value span_ =
  case span_ of
    Entry entry -> Entry $ EntrySpan.addData value entry
    Exit exit   -> Exit $ ExitSpan.addData value exit


-- |Add a value at the given path to the span's data section. For SDK spans, you
-- should use addTagAt instead.
addRegisteredDataAt :: Aeson.ToJSON a => Text -> a -> Span -> Span
addRegisteredDataAt path value span_ =
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
  addRegisteredData newData span_

