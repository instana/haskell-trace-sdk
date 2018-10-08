{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.NonRootEntry
Description : An entry span that is not the root of a trace
-}
module Instana.SDK.Span.NonRootEntry (NonRootEntry(..)) where


import           Data.Aeson              (Value)
import           Data.Text               (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id (Id)


-- |An entry span that is not the root span of a trace.
data NonRootEntry =
  NonRootEntry
    {
      -- |The trace ID
      traceId   :: Id
      -- |The span ID
    , spanId    :: Id
      -- |The ID of the parent span
    , parentId  :: Id
      -- |The span type span, e.g. a short string like "yesod", "servant",
      -- "rpc-server", ...
    , spanType  :: Text
      -- |The time the span started
    , timestamp :: Int
      -- |A free text label for the span
    , label     :: Text
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData  :: Value
    } deriving (Eq, Generic, Show)

