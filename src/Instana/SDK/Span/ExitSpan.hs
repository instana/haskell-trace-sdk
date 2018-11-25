{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Span.ExitSpan
Description : An exit span
-}
module Instana.SDK.Span.ExitSpan (ExitSpan(..)) where


import           Data.Aeson                 (Value)
import           Data.Text                  (Text)
import           GHC.Generics

import           Instana.SDK.Span.EntrySpan (EntrySpan)


-- |An exit span.
data ExitSpan  =
  ExitSpan
    {
      -- |The parent span
      parentSpan :: EntrySpan
      -- |The span name/type, e.g. a short string like "yesod", "servant",
      -- "rpc-server", ...
    , spanName   :: Text
      -- |The time the span started
    , timestamp  :: Int
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData   :: Value
    } deriving (Eq, Generic, Show)

