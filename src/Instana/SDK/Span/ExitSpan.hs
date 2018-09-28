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


data ExitSpan  =
  ExitSpan
    { parentSpan :: EntrySpan
    , spanType   :: Text
    , timestamp  :: Int
    , label      :: Text
    , spanData   :: Value
    } deriving (Eq, Generic, Show)

