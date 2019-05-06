{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.NonRootEntry
Description : An entry span that is not the root of a trace
-}
module Instana.SDK.Span.NonRootEntry
  ( NonRootEntry(..)
  , addData
  , addToErrorCount
  ) where


import           Data.Aeson              (Value)
import qualified Data.Aeson.Extra.Merge  as AesonExtra
import           Data.Text               (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id (Id)


-- |An entry span that is not the root span of a trace.
data NonRootEntry =
  NonRootEntry
    {
      -- |The trace ID
      traceId    :: Id
      -- |The span ID
    , spanId     :: Id
      -- |The ID of the parent span
    , parentId   :: Id
      -- |The span name/type, e.g. a short string like "haskell.wai.server",
      -- "haskell.http.client". For SDK spans this is always "sdk", the actual
      -- name is then in span.data.sdk.name.
    , spanName   :: Text
      -- |The time the span started
    , timestamp  :: Int
      -- |The number of errors that occured during processing
    , errorCount :: Int
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData   :: Value
    } deriving (Eq, Generic, Show)


-- |Add to the error count.
addToErrorCount :: Int -> NonRootEntry -> NonRootEntry
addToErrorCount increment nonRootEntry =
  let
    ec = errorCount nonRootEntry
  in
  nonRootEntry { errorCount = ec + increment }


-- |Add a value to the span's data section.
addData :: Value -> NonRootEntry -> NonRootEntry
addData newData nonRootEntry =
  let
    currentData = spanData nonRootEntry
    mergedData = AesonExtra.lodashMerge currentData newData
  in
  nonRootEntry { spanData = mergedData }

