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


data RootEntry =
  RootEntry
    { spanAndTraceId :: Id
    , spanType       :: Text
    , timestamp      :: Int
    , label          :: Text
    , spanData       :: Value
    } deriving (Eq, Generic, Show)


traceId :: RootEntry -> Id
traceId = spanAndTraceId


spanId :: RootEntry -> Id
spanId = spanAndTraceId

