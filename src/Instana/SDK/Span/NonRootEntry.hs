{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.NonRootEntry
Description : An entry span that is not the root of a trace
-}
module Instana.SDK.Span.NonRootEntry
  ( NonRootEntry(..)
  , addData
  , addToErrorCount
  , setServiceName
  , setSynthetic
  , setTpFlag
  , setW3cTraceContext
  ) where


import           Data.Aeson                           (Value)
import qualified Data.Aeson.Extra.Merge               as AesonExtra
import           Data.Text                            (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id              (Id)
import           Instana.SDK.Internal.W3CTraceContext (W3CTraceContext)


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
      -- |The span name/type, e.g. a short string like "haskell.wai.server",
      -- "haskell.http.client". For SDK spans this is always "sdk", the actual
      -- name is then in span.data.sdk.name.
    , spanName        :: Text
      -- |The time the span started
    , timestamp       :: Int
      -- |The number of errors that occured during processing
    , errorCount      :: Int
      -- |An attribute for overriding the name of the service in Instana
    , serviceName     :: Maybe Text
      -- |A flag indicating that this span represents a synthetic call
    , synthetic       :: Bool
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData        :: Value
      -- |The W3C Trace Context. An entry span only has an associated W3C trace
      -- context, if W3C trace context headers have been received.
    , w3cTraceContext :: Maybe W3CTraceContext
      -- |The span.tp flag. A span with span.tp = True has inherited the
      -- trace ID/parent ID from W3C trace context instead of Instana headers.
    , tpFlag          :: Bool
    } deriving (Eq, Generic, Show)


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


-- |Add a value to the span's data section.
addData :: Value -> NonRootEntry -> NonRootEntry
addData newData nonRootEntry =
  let
    currentData = spanData nonRootEntry
    mergedData = AesonExtra.lodashMerge currentData newData
  in
  nonRootEntry { spanData = mergedData }

