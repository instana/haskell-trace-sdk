{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Context
Description : An internal representation of a span with all values set
-}
module Instana.SDK.Internal.FullSpan
  ( FullSpan(..)
  , SpanKind(..)
  ) where


import           Data.Aeson              (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson              as Aeson
import           Data.Aeson.Types        (Parser)
import           Data.Text               (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id (Id)


-- |Direction of the call.
data SpanKind =
    -- |The monitored componenent receives a call.
    Entry
    -- |The monitored componenent calls something else.
  | Exit
    -- |An additional annotation that is added to the trace while a traced call
    -- is being processed.
  | Intermediate
  deriving (Eq, Generic, Show)


instance FromJSON SpanKind where
  parseJSON :: Value -> Parser SpanKind
  parseJSON = Aeson.withScientific "span kind string" $
    \k ->
      case k of
        -- (1=entry, 2=exit, 3=local/intermediate)
        1 -> return Entry
        2 -> return Exit
        3 -> return Intermediate
        _              ->
          fail "expected numeric span kind (1, 2, or 3)."


instance ToJSON SpanKind where
  toJSON :: SpanKind -> Value
  toJSON k =
    case k of
      Entry        -> Aeson.Number 1
      Exit         -> Aeson.Number 2
      Intermediate -> Aeson.Number 3


-- |A representation of the span with all its data. This will be send to the
-- agent later.
data FullSpan = FullSpan
  { traceId   :: Id
  , spanId    :: Id
  , parentId  :: Maybe Id
  , spanType  :: Text
  , timestamp :: Int
  , duration  :: Int
  , kind      :: SpanKind
  , label     :: Text
  , spanError :: Bool
  , spanData  :: Value
  } deriving (Eq, Generic, Show)


instance FromJSON FullSpan where
  parseJSON = Aeson.withObject "span" $
    \s ->
      FullSpan
        <$> s .: "t"     -- traceId
        <*> s .: "s"     -- spanId
        <*> s .: "p"     -- parentId
        <*> s .: "n"     -- type/name
        <*> s .: "ts"    -- timestamp
        <*> s .: "d"     -- duration
        <*> s .: "k"     -- kind (entry, exit, intermediate)
        <*> s .: "label" -- label
        <*> s .: "error" -- error
        <*> s .: "data"  -- data


instance ToJSON FullSpan where
  toJSON :: FullSpan -> Value
  toJSON s = Aeson.object
    [ "t"     .= traceId s
    , "s"     .= spanId s
    , "p"     .= parentId s
    , "n"     .= spanType s
    , "ts"    .= timestamp s
    , "d"     .= duration s
    , "k"     .= kind s
    , "label" .= label s
    , "error" .= spanError s
    , "data"  .= spanData s
    ]

