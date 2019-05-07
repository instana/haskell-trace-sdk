{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.ExitSpan
Description : An exit span
-}
module Instana.SDK.Span.ExitSpan
  ( ExitSpan(..)
  , parentId
  , traceId
  , addData
  , addToErrorCount
  , setServiceName
  ) where


import           Data.Aeson                 (Value)
import qualified Data.Aeson.Extra.Merge     as AesonExtra
import           Data.Text                  (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id    (Id)
import           Instana.SDK.Span.EntrySpan (EntrySpan)
import qualified Instana.SDK.Span.EntrySpan as EntrySpan


-- |An exit span.
data ExitSpan  =
  ExitSpan
    {
      -- |The parent span
      parentSpan  :: EntrySpan
      -- |The span ID
    , spanId      :: Id
      -- |The span name/type, e.g. a short string like "haskell.wai.server",
      -- "haskell.http.client". For SDK spans this is always "sdk", the actual
      -- name is then in span.data.sdk.name.
    , spanName    :: Text
      -- |The time the span started
    , timestamp   :: Int
      -- |An attribute for overriding the name of the service in Instana
    , serviceName :: Maybe Text
      -- |The number of errors that occured during processing
    , errorCount  :: Int
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData    :: Value
    } deriving (Eq, Generic, Show)


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


-- |Add a value to the span's data section.
addData :: Value -> ExitSpan -> ExitSpan
addData newData exitSpan =
  let
    currentData = spanData exitSpan
    mergedData = AesonExtra.lodashMerge currentData newData
  in
  exitSpan { spanData = mergedData }

