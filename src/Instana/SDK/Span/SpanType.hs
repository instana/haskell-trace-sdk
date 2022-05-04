{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.SpanType
Description : Describes the type of a span, either an SDK span or a
              registered span.
-}
module Instana.SDK.Span.SpanType
  ( RegisteredSpanType (..)
  , SpanType (SdkSpan, RegisteredSpan)
  , spanName
  ) where


import           Data.String  (IsString (fromString))
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics


-- |Differentiates between SDK spans and registered spans (which receive
-- special treatment by Instana's processing pipeline.
data SpanType =
    SdkSpan Text
  | RegisteredSpan RegisteredSpanType
  deriving (Eq, Generic, Show)


-- |All registered spans that the Haskell trace SDK will produce.
data RegisteredSpanType =
    HaskellWaiServer
  | HaskellHttpClient
  deriving (Eq, Generic, Show)


-- |Returns the wire value of span.n for a SpanType value.
spanName    :: SpanType -> Text
spanName    (SdkSpan _)                 = "sdk"
spanName    (RegisteredSpan registered) = registeredSpanName registered


-- |Returns the wire value of span.n for a registered span.
registeredSpanName :: RegisteredSpanType -> Text
registeredSpanName HaskellWaiServer  = "haskell.wai.server"
registeredSpanName HaskellHttpClient = "haskell.http.client"


-- |Enables passing any string as the span type argument to SDK.startEntrySpan
-- etc. - this will be automatically converted to an SDK span.
instance IsString SpanType where
  fromString :: String -> SpanType
  fromString s = SdkSpan $ T.pack s

