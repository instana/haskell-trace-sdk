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
      -- |The span type span, e.g. a short string like "yesod", "servant",
      -- "rpc-server", ...
    , spanType   :: Text
      -- |The time the span started
    , timestamp  :: Int
      -- |A free text label for the span
    , label      :: Text
      -- |Additional data for the span. Must be provided as an
      -- 'Data.Aeson.Value'.
    , spanData   :: Value
    } deriving (Eq, Generic, Show)

