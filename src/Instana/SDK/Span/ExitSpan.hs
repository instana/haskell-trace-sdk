{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.ExitSpan
Description : An exit span
-}
module Instana.SDK.Span.ExitSpan
  ( ExitSpan(..)
  , parentId
  , traceId
  , addAnnotation
  , addToErrorCount
  , setServiceName
  , setW3cTraceContext
  , spanName
  ) where


import           Data.Text                            (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id              (Id)
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)
import           Instana.SDK.Span.EntrySpan           (EntrySpan)
import qualified Instana.SDK.Span.EntrySpan           as EntrySpan
import           Instana.SDK.Span.SpanData            (Annotation, SpanData)
import qualified Instana.SDK.Span.SpanData            as SpanData
import           Instana.SDK.Span.SpanType            (SpanType)
import qualified Instana.SDK.Span.SpanType            as SpanType


-- |An exit span.
data ExitSpan  =
  ExitSpan
    {
      -- |The parent span
      parentSpan      :: EntrySpan
      -- |The span ID
    , spanId          :: Id
      -- |The type of the span (SDK span or registerd span)
    , spanType        :: SpanType
      -- |The time the span started
    , timestamp       :: Int
      -- |An attribute for overriding the name of the service in Instana
    , serviceName     :: Maybe Text
      -- |The number of errors that occured during processing
    , errorCount      :: Int
      -- |Additional data for the span.
    , spanData        :: SpanData
      -- |The W3C Trace Context. An entry span only has an associated W3C trace
      -- context, if W3C trace context headers have been received. In contrast,
      -- spans always have an associated W3C trace context.
    , w3cTraceContext :: W3CTraceContext
    } deriving (Eq, Generic, Show)


-- |The span name/type, e.g. a short string like "haskell.wai.server",
-- "haskell.http.client". For SDK spans this is always "sdk", the actual
-- name is then in span.data.sdk.name.
spanName :: ExitSpan -> Text
spanName = SpanType.spanName . spanType


-- |Accessor for the trace ID.
traceId :: ExitSpan -> Id
traceId exitSpan =
  EntrySpan.traceId $ parentSpan exitSpan


-- |Parent span ID.
parentId :: ExitSpan -> Id
parentId exitSpan =
  EntrySpan.spanId $ parentSpan exitSpan


-- |Add to the error count.
addToErrorCount :: Int -> ExitSpan -> ExitSpan
addToErrorCount increment exitSpan =
  let
    ec = errorCount exitSpan
  in
  exitSpan { errorCount = ec + increment }


-- |Override the name of the service for the associated call in Instana.
setServiceName :: Text -> ExitSpan -> ExitSpan
setServiceName serviceName_ exitSpan =
  exitSpan { serviceName = Just serviceName_ }


-- |Attaches a W3C trace context to the span.
setW3cTraceContext :: W3CTraceContext -> ExitSpan -> ExitSpan
setW3cTraceContext w3cTraceContext_ exitSpan =
  exitSpan { w3cTraceContext = w3cTraceContext_ }


-- |Add an annotation to the span's data section. For SDK spans, the annotation
-- is added to span.data.sdk.custom.tags, for registered spans it is added
-- directly to span.data.
addAnnotation :: Annotation -> ExitSpan -> ExitSpan
addAnnotation annotation exitSpan =
  exitSpan { spanData = SpanData.merge annotation $ spanData exitSpan }

