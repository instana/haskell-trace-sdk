{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.SpanType
Description : Describes the type of a span, either an SDK span or a
              registered span.
-}
module Instana.SDK.Span.SpanType
  ( Registered (..)
  , SpanType (SdkSpan, RegisteredSpan)
  , spanName
  , initialData
  ) where


import           Data.Aeson            (Value, (.=))
import qualified Data.Aeson            as Aeson
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics

import           Instana.SDK.Span.Span (SpanKind (EntryKind, ExitKind, IntermediateKind))


data SpanType =
    SdkSpan Text
  | RegisteredSpan Registered
  deriving (Eq, Generic, Show)


data Registered =
    HaskellWaiServer
  | HaskellHttpClient
  deriving (Eq, Generic, Show)


spanName    :: SpanType -> Text
spanName    (SdkSpan _)                 = "sdk"
spanName    (RegisteredSpan registered) = registeredSpanName registered


registeredSpanName :: Registered -> Text
registeredSpanName HaskellWaiServer  = "haskell.wai.server"
registeredSpanName HaskellHttpClient = "haskell.http.client"


initialData :: SpanKind -> SpanType -> Value
initialData kind (SdkSpan s)     = initialSdkData kind s
initialData _ (RegisteredSpan _) = emptyValue


-- |Enables passing any string as the span type argument to SDK.startEntrySpan
-- etc. - this will be automatically converted to an SDK span.
instance IsString SpanType where
  fromString :: String -> SpanType
  fromString s = SdkSpan $ T.pack s


-- |Provides the initial data for an SDK span.
initialSdkData :: SpanKind -> Text -> Value
initialSdkData kind spanName_ =
  let
    sdkKind :: String
    sdkKind =
      case kind of
        EntryKind        -> "entry"
        ExitKind         -> "exit"
        IntermediateKind -> "intermediate"
  in
  (Aeson.object [ "sdk" .=
    Aeson.object
      [ "name" .= spanName_
      , "type" .= sdkKind
      ]
    ]
  )


-- |Provides an empty Aeson value.
emptyValue :: Value
emptyValue = Aeson.object []

