{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.SimpleSpan
Description : A representation for spans for external use.
-}
module Instana.SDK.Span.SimpleSpan
  ( SimpleSpan(..)
  , convert
  ) where


import qualified Data.Aeson              as Aeson
import qualified Data.Text               as T
import           GHC.Generics

import qualified Instana.SDK.Internal.Id as Id
import           Instana.SDK.Span.Span   (SpanKind (..))
import           Instana.SDK.Span.Span   (Span)
import qualified Instana.SDK.Span.Span   as Span


-- |A representation of the span fit for external use by clients of the SDK.
--
data SimpleSpan = SimpleSpan
  { traceId    :: String
  , spanId     :: String
  , parentId   :: Maybe String
  , spanName   :: String
  , timestamp  :: Int
  , kind       :: SpanKind
  , errorCount :: Int
  , spanData   :: Aeson.Value
  } deriving (Eq, Generic, Show)


convert :: Span -> SimpleSpan
convert span_ =
  SimpleSpan
    { traceId    = Id.toString $ Span.traceId span_
    , spanId     = Id.toString $ Span.spanId span_
    , parentId   = Id.toString <$> Span.parentId span_
    , spanName   = T.unpack $ Span.spanName span_
    , timestamp  = Span.timestamp span_
    , kind       = Span.spanKind span_
    , errorCount = Span.errorCount span_
    , spanData   = Span.spanData span_
    }

