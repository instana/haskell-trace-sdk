{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.RootEntry
Description : A root entry span
-}
module Instana.SDK.Span.RootEntry
  ( RootEntry(..)
  , spanId
  , traceId
  , addAnnotation
  , addToErrorCount
  , setServiceName
  , setCorrelationType
  , setCorrelationId
  , setSynthetic
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


-- |An entry span that is the root span of a trace.
data RootEntry =
  RootEntry
    {
      -- |The trace ID and span ID (those are identical for root spans)
       spanAndTraceId :: Id
      -- |The type of the span (SDK span or registerd span)
     , spanType       :: SpanType
     -- |The time the span (and trace) started
    , timestamp       :: Int
      -- |The number of errors that occured during processing
    , errorCount      :: Int
      -- |An attribute for overriding the name of the service in Instana
    , serviceName     :: Maybe Text
      -- |A flag indicating that this span represents a synthetic call
    , synthetic       :: Bool
      -- |The website monitoring correlation type
    , correlationType :: Maybe Text
      -- |The website monitoring correlation ID
    , correlationId   :: Maybe Text
      -- |Additional data for the span.
    , spanData        :: SpanData
      -- |The W3C Trace Context. An entry span only has an associated W3C trace
      -- context, if W3C trace context headers have been received.
    , w3cTraceContext :: Maybe W3CTraceContext

    } deriving (Eq, Generic, Show)


-- |The span name/type, e.g. a short string like "haskell.wai.server",
-- "haskell.http.client". For SDK spans this is always "sdk", the actual
-- name is then in span.data.sdk.name.
spanName :: RootEntry -> Text
spanName = SpanType.spanName . spanType


-- |Accessor for the trace ID.
traceId :: RootEntry -> Id
traceId = spanAndTraceId


-- |Accessor for the span ID.
spanId :: RootEntry -> Id
spanId = spanAndTraceId


-- |Add to the error count.
addToErrorCount :: Int -> RootEntry -> RootEntry
addToErrorCount increment rootEntry =
  let
    ec = errorCount rootEntry
  in
  rootEntry { errorCount = ec + increment }


-- |Override the name of the service for the associated call in Instana.
setServiceName :: Text -> RootEntry -> RootEntry
setServiceName serviceName_ rootEntry =
  rootEntry { serviceName = Just serviceName_ }


-- |Attaches a W3C trace context to the span.
setW3cTraceContext :: W3CTraceContext -> RootEntry -> RootEntry
setW3cTraceContext w3cTraceContext_ rootEntry =
  rootEntry { w3cTraceContext = Just w3cTraceContext_ }


-- |Set the synthetic flag.
setSynthetic :: Bool -> RootEntry -> RootEntry
setSynthetic synthetic_ rootEntry =
  rootEntry { synthetic = synthetic_ }


-- |Set the website monitoring correlation type.
setCorrelationType :: Text -> RootEntry -> RootEntry
setCorrelationType correlationType_ rootEntry =
  rootEntry { correlationType = Just correlationType_ }


-- |Set the website monitoring correlation ID.
setCorrelationId :: Text -> RootEntry -> RootEntry
setCorrelationId correlationId_ rootEntry =
  rootEntry { correlationId = Just correlationId_ }


-- |Add an annotation to the span's data section. For SDK spans, the annotation
-- is added to span.data.sdk.custom.tags, for registered spans it is added
-- directly to span.data.
addAnnotation :: Annotation -> RootEntry -> RootEntry
addAnnotation annotation rootEntry =
  rootEntry { spanData = SpanData.merge annotation $ spanData rootEntry }

