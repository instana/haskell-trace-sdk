{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.RootEntry
Description : A root entry span
-}
module Instana.SDK.Span.RootEntry
  ( RootEntry(..)
  , spanId
  , traceId
  ) where


import           Data.Aeson              (Value)
import           Data.Text               (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id (Id)


-- |An entry span that is the root span of a trace.
data RootEntry =
  RootEntry
    {
      -- |The trace ID and span ID (those are identical for root spans)
      spanAndTraceId :: Id
      -- |The span type span, e.g. a short string like "yesod", "servant",
    , spanType       :: Text
      -- |The time the span (and trace) started
    , timestamp      :: Int
      -- |A free text label for the span
    , label          :: Text
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

