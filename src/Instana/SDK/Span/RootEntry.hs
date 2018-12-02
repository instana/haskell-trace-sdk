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
      spanAndTraceId :: Id
      -- |The span name/type, e.g. a short string like "yesod", "servant",
    , spanName       :: Text
      -- |The time the span (and trace) started
    , timestamp      :: Int
      -- |The number of errors that occured during processing
    , errorCount     :: Int
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData       :: Value
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


-- |Add a value to the span's data section.
addData :: Value -> RootEntry -> RootEntry
addData newData rootEntry =
  let
    currentData = spanData rootEntry
    mergedData = AesonExtra.lodashMerge currentData newData
  in
  rootEntry { spanData = mergedData }

