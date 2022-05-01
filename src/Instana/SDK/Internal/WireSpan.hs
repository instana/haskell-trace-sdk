{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Context
Description : Internal representations of a span with all values set, ready to
              be sent to the agent (over the wire, hence the name).
-}
module Instana.SDK.Internal.WireSpan
  ( QueuedSpan(..)
  , WireSpan(..)
  , SpanKind(..)
  ) where


import           Control.Applicative       ((<|>))
import           Data.Aeson                (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (Parser)
import           Data.Text                 (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id   (Id)
import qualified Instana.SDK.Internal.Id   as Id
import           Instana.SDK.Span.SpanData (SpanData)
import qualified Instana.SDK.Span.SpanData as SpanData


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


-- |The `from` part of the span.
data From = From
  { entityId :: String
  , hostId   :: Text
  } deriving (Eq, Generic, Show)


instance FromJSON From where
  parseJSON = Aeson.withObject "from" $
    \f ->
      From
        <$> f .: "e" -- entityId
        <*> f .: "h" -- host ID/agent UUID


instance ToJSON From where
  toJSON :: From -> Value
  toJSON f = Aeson.object
    [ "e" .= entityId f
    , "h" .= hostId f
    ]


-- |A representation of the span with all its data, except for attributes that
-- are constant per Haskell process (pid/entityId and agent UUID/host ID). This
-- is a preliminary representation of what will be send to the agent later
-- (after adding the aforementioned per-process/static attributes). Values of
-- this type are stored in the spanQueue in Instana.SDK.Internal.Context after
-- they have been completed.
data QueuedSpan = QueuedSpan
  { traceId         :: Id
  , spanId          :: Id
  , parentId        :: Maybe Id
  , spanName        :: Text
  , timestamp       :: Int
  , duration        :: Int
  , kind            :: SpanKind
  , errorCount      :: Int
  , serviceName     :: Maybe Text
  , correlationType :: Maybe Text
  , correlationId   :: Maybe Text
  , tpFlag          :: Maybe Bool
  , instanaAncestor :: Maybe (String, String)
  , synthetic       :: Maybe Bool
  , spanData        :: SpanData
  } deriving (Eq, Generic, Show)


-- |Combines the actual span data with static per-process data (PID,
-- agent UUID). This is the final value that will be sent to the agent.
data WireSpan = WireSpan
  { queuedSpan        :: QueuedSpan
  , pid               :: String
  , agentUuid         :: Text
  , serviceNameConfig :: Maybe Text
  } deriving (Eq, Generic, Show)


instance ToJSON WireSpan where
  toJSON :: WireSpan -> Value
  toJSON wireSpan =
    let
      tId = traceId span_
      longTraceId =
        case kind span_ of
          Entry -> Id.longTraceId tId
          _     -> Nothing
      span_ = queuedSpan wireSpan
      pid_ = pid wireSpan
      agentUuid_ = agentUuid wireSpan
      serviceNameConfig_ = serviceNameConfig wireSpan
      ia =
        case instanaAncestor span_ of
          Just (iaTId, iaPId) ->
            Just $ Aeson.object
              [ "t" .= iaTId
              , "p" .= iaPId
              ]
          Nothing ->
            Nothing

      spanData_ =
        case (serviceName span_ <|> serviceNameConfig_) of
          Just service ->
            SpanData.merge
              (SpanData.simpleAnnotation "service" service)
              (spanData span_)
          _ ->
            spanData span_
    in
    Aeson.object
      [ "t"     .= tId
      , "s"     .= spanId span_
      , "p"     .= parentId span_
      , "n"     .= spanName span_
      , "ts"    .= timestamp span_
      , "d"     .= duration span_
      , "k"     .= kind span_
      , "ec"    .= errorCount span_
      , "crtp"  .= correlationType span_
      , "crid"  .= correlationId span_
      , "sy"    .= synthetic span_
      , "lt"    .= longTraceId
      , "tp"    .= tpFlag span_
      , "ia"    .= ia
      , "data"  .= spanData_
      , "f"     .= From pid_ agentUuid_
      ]

