{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.NonRootEntry
Description : An entry span that is not the root of a trace
-}
module Instana.SDK.Span.NonRootEntry
  ( NonRootEntry(..)
  , addAnnotation
  , addToErrorCount
  , setServiceName
  , setSynthetic
  , setTpFlag
  , setW3cTraceContext
  , spanName
  ) where


import           Data.Text                            (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id              (Id)
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)
import           Instana.SDK.Span.SpanData            (Annotation, SpanData)
import qualified Instana.SDK.Span.SpanData            as SpanData
import           Instana.SDK.Span.SpanType            (SpanType)
import qualified Instana.SDK.Span.SpanType            as SpanType


-- |An entry span that is not the root span of a trace.
data NonRootEntry =
  NonRootEntry
    {
      -- |The trace ID
      traceId         :: Id
      -- |The span ID
    , spanId          :: Id
      -- |The ID of the parent span
    , parentId        :: Id
      -- |The type of the span (SDK span or registerd span)
    , spanType        :: SpanType
      -- |The time the span started
    , timestamp       :: Int
      -- |The number of errors that occured during processing
    , errorCount      :: Int
      -- |An attribute for overriding the name of the service in Instana
    , serviceName     :: Maybe Text
      -- |A flag indicating that this span represents a synthetic call
    , synthetic       :: Bool
      -- |Additional data for the span.
    , spanData        :: SpanData
      -- |The W3C Trace Context. An entry span only has an associated W3C trace
      -- context, if W3C trace context headers have been received.
    , w3cTraceContext :: Maybe W3CTraceContext
      -- |The span.tp flag. A span with span.tp = True has inherited the
      -- trace ID/parent ID from W3C trace context instead of Instana headers.
    , tpFlag          :: Bool
    } deriving (Eq, Generic, Show)


-- |The span name/type, e.g. a short string like "haskell.wai.server",
-- "haskell.http.client". For SDK spans this is always "sdk", the actual
-- name is then in span.data.sdk.name.
spanName :: NonRootEntry -> Text
spanName = SpanType.spanName . spanType


-- |Add to the error count.
addToErrorCount :: Int -> NonRootEntry -> NonRootEntry
addToErrorCount increment nonRootEntry =
  let
    ec = errorCount nonRootEntry
  in
  nonRootEntry { errorCount = ec + increment }


-- |Override the name of the service for the associated call in Instana.
setServiceName :: Text -> NonRootEntry -> NonRootEntry
setServiceName serviceName_ nonRootEntry =
  nonRootEntry { serviceName = Just serviceName_ }


-- |Attaches a W3C trace context to the span.
setW3cTraceContext :: W3CTraceContext -> NonRootEntry -> NonRootEntry
setW3cTraceContext w3cTraceContext_ nonRootEntry =
  nonRootEntry { w3cTraceContext = Just w3cTraceContext_ }


-- |Set the span.tp flag. A span with span.tp = True has inherited the trace ID/
-- parent ID from W3C trace context instead of Instana headers.
setTpFlag :: NonRootEntry -> NonRootEntry
setTpFlag nonRootEntry =
  nonRootEntry { tpFlag = True }


-- |Set the synthetic flag.
setSynthetic :: Bool -> NonRootEntry -> NonRootEntry
setSynthetic synthetic_ nonRootEntry =
  nonRootEntry { synthetic = synthetic_ }


-- |Add an annotation to the span's data section. For SDK spans, the annotation
-- is added to span.data.sdk.custom.tags, for registered spans it is added
-- directly to span.data.
addAnnotation :: Annotation -> NonRootEntry -> NonRootEntry
addAnnotation annotation nonRootEntry =
  nonRootEntry { spanData = SpanData.merge annotation $ spanData nonRootEntry }

