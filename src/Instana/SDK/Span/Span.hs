{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.Span
Description : A type for spans (entries, exits or intermediates).
-}
module Instana.SDK.Span.Span
  ( Span (Entry, Exit)
  , SpanKind (EntryKind, ExitKind, IntermediateKind)
  , addAnnotation
  , addAnnotationAt
  , addAnnotationValueAt
  , addToErrorCount
  , correlationId
  , correlationType
  , errorCount
  , initialData
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
  , spanKind
  , spanName
  , spanType
  , synthetic
  , timestamp
  , tpFlag
  , traceId
  , w3cTraceContext
  ) where


import           Data.Aeson                           (ToJSON)
import           Data.List                            as List
import           Data.Text                            as T
import           GHC.Generics

import           Instana.SDK.Internal.Id              (Id)
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)
import           Instana.SDK.Span.EntrySpan           (EntrySpan)
import qualified Instana.SDK.Span.EntrySpan           as EntrySpan
import           Instana.SDK.Span.ExitSpan            (ExitSpan)
import qualified Instana.SDK.Span.ExitSpan            as ExitSpan
import           Instana.SDK.Span.SpanData            (Annotation,
                                                       AnnotationValue,
                                                       SpanData (SpanData))
import qualified Instana.SDK.Span.SpanData            as SpanData
import           Instana.SDK.Span.SpanType            (SpanType (RegisteredSpan, SdkSpan))


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


-- |Type of span (registerd span vs. SDK span)
spanType :: Span -> SpanType
spanType span_ =
  case span_ of
    Entry entry -> EntrySpan.spanType entry
    Exit exit   -> ExitSpan.spanType exit


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
spanData :: Span -> SpanData
spanData span_ =
  case span_ of
    Entry entry -> EntrySpan.spanData entry
    Exit exit   -> ExitSpan.spanData exit


-- |Add an annotation to the span's data section. For SDK spans, the annotation
-- is added to span.data.sdk.custom.tags, for registered spans it is added
-- directly to span.data.
addAnnotation :: Annotation -> Span -> Span
addAnnotation annotation span_ =
  let
    wrappedAnnotation = wrapAnnotationIfNecessary span_ annotation
  in
  case span_ of
    Entry entry -> Entry $ EntrySpan.addAnnotation wrappedAnnotation entry
    Exit exit   -> Exit $ ExitSpan.addAnnotation wrappedAnnotation exit


-- |Add a simple value (string, boolean, number) at the given path to the span's
-- data section.
addAnnotationAt :: ToJSON a => Text -> a -> Span -> Span
addAnnotationAt path value span_ =
  let
    pathSegments = T.splitOn "." path
    lastPathSegment = List.last pathSegments
    pathPrefix = List.take (List.length pathSegments - 1) pathSegments
    newData = List.foldr
      (\nextPathSegment accumulated ->
        SpanData.objectAnnotation nextPathSegment [accumulated]
      )
      (SpanData.simpleAnnotation lastPathSegment value)
      pathPrefix
  in
  addAnnotation newData span_


-- |Add a list value at the given path to the span's data section. For SDK
-- spans, you should use addAnnotationValueToSdkSpan instead. For annotations with simple
-- values (string, number, boolean, etc.), you can also use the convenience
-- function addAnnotationAt.
addAnnotationValueAt :: Text -> AnnotationValue -> Span -> Span
addAnnotationValueAt path value span_ =
  let
    pathSegments = T.splitOn "." path
    lastPathSegment = List.last pathSegments
    pathPrefix = List.take (List.length pathSegments - 1) pathSegments
    newData = List.foldr
      (\nextPathSegment accumulated ->
        SpanData.objectAnnotation nextPathSegment [accumulated]
      )
      (SpanData.singleAnnotation lastPathSegment value)
      pathPrefix
  in
  addAnnotation newData span_


wrapAnnotationIfNecessary :: Span -> Annotation -> Annotation
wrapAnnotationIfNecessary span_ annotation =
  case spanType span_ of
    RegisteredSpan _ ->
      annotation
    SdkSpan _ ->
      ( SpanData.objectAnnotation "sdk" [
          SpanData.objectAnnotation "custom" [
            SpanData.objectAnnotation "tags" [
              annotation
            ]
          ]
        ]
      )


-- |Returns the initial data (span.data) for a SpanType value.
initialData :: SpanKind -> SpanType -> SpanData
initialData kind (SdkSpan s)     = initialSdkData kind s
initialData _ (RegisteredSpan _) = SpanData.empty


-- |Provides the initial data for an SDK span.
initialSdkData :: SpanKind -> Text -> SpanData
initialSdkData kind spanName_ =
  let
    sdkKind :: Text
    sdkKind =
      case kind of
        EntryKind        -> "entry"
        ExitKind         -> "exit"
        IntermediateKind -> "intermediate"
  in
  SpanData
    [ SpanData.objectAnnotation "sdk"
        [ SpanData.simpleAnnotation "name" spanName_
        , SpanData.simpleAnnotation "type" sdkKind
        ]
    ]

