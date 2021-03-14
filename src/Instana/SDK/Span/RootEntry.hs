{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.RootEntry
Description : A root entry span
-}
module Instana.SDK.Span.RootEntry
  ( RootEntry(..)
  , spanId
  , traceId
  , addData
  , addToErrorCount
  , setServiceName
  , setCorrelationType
  , setCorrelationId
  , setSynthetic
  ) where


import           Data.Aeson              (Value)
import qualified Data.Aeson.Extra.Merge  as AesonExtra
import           Data.Text               (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id (Id)


-- |An entry span that is the root span of a trace.
data RootEntry =
  RootEntry
    {
      -- |The trace ID and span ID (those are identical for root spans)
      spanAndTraceId  :: Id
      -- |The span name/type, e.g. a short string like "haskell.wai.server",
      -- "haskell.http.client". For SDK spans this is always "sdk", the actual
      -- name is then in span.data.sdk.name.
    , spanName        :: Text
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
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData        :: Value
    } deriving (Eq, Generic, Show)


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


-- |Add a value to the span's data section.
addData :: Value -> RootEntry -> RootEntry
addData newData rootEntry =
  let
    currentData = spanData rootEntry
    mergedData = AesonExtra.lodashMerge currentData newData
  in
  rootEntry { spanData = mergedData }

