{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Context
Description : An internal representation of a span with all values set
-}
module Instana.SDK.Internal.FullSpan
  ( FullSpan(..)
  , FullSpanWithPid(..)
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


-- |The `from` part of the span.
data From = From
  { entityId :: String
  } deriving (Eq, Generic, Show)


instance FromJSON From where
  parseJSON = Aeson.withObject "from" $
    \f ->
      From
        <$> f .: "e" -- entityId


instance ToJSON From where
  toJSON :: From -> Value
  toJSON f = Aeson.object
    [ "e" .= entityId f ]


-- |A representation of the span with all its data. This will be send to the
-- agent later.
data FullSpan = FullSpan
  { traceId    :: Id
  , spanId     :: Id
  , parentId   :: Maybe Id
  , spanName   :: Text
  , timestamp  :: Int
  , duration   :: Int
  , kind       :: SpanKind
  , errorCount :: Int
  , spanData   :: Value
  } deriving (Eq, Generic, Show)


-- |Combines the actual span data with static per-process data (PID).
data FullSpanWithPid = FullSpanWithPid
  { fullSpan :: FullSpan
  , pid      :: String
  } deriving (Eq, Generic, Show)


-- Compare
-- https://github.com/instana/technical-documentation/blob/master/tracing/format.md
-- Registered and SDK spans use the same format, exact same attributes. The only
-- difference is the data, SDK spans should provide data.sdk (see below).
instance ToJSON FullSpanWithPid where
  toJSON :: FullSpanWithPid -> Value
  toJSON fullSpanWithPid =
    let
      s = fullSpan fullSpanWithPid
      p = pid fullSpanWithPid
    in
    Aeson.object
      [ "t"     .= traceId s
      , "s"     .= spanId s
      , "p"     .= parentId s
      , "n"     .= spanName s
      , "ts"    .= timestamp s
      , "ta"    .= ("haskell" :: String)
      , "d"     .= duration s
      , "k"     .= kind s
      , "ec"    .= errorCount s
      , "data"  .= spanData s
      , "f"     .= From p
      -- TODO - missing attributes:
      -- - data.service - should have dedicated functionality to be set (for SDK
      --   spans)
      -- - For SDK spans: Everything should be in data.sdk, structure is described in
      --   https://github.com/instana/technical-documentation/blob/master/tracing/format.md#json-format-for-sdk-spans
      -- - e: [{ # events that happened during this span, seems to be used mostly by EUM??
      --     t: <long> # timestamp of this event relative to start (0 .. d)
      --     v: <String> # type of this annotation (ttfb, dom-ready, etc)
      --   }],
      -- - f.h: AgentId/HostID (optional), specified in the language announce response body.
      --   },
      -- - b: { # batching data
      --     s: <long> # size; amount of batched spans
      --     d: <long> # duration in ms; more realistic time of time consumed by individual batched spans. (optional, regular duration taken if absent)
      --   },
      -- - stack: [{ # stack trace
      --     c: <String> # Class name
      --     m: <String> # Method name
      --     n: <String> # Line number
      --     f: <String> # File name (optional in Java)
      --   }],
      -- - deferred: <boolean> # whether the span is deferred (optional), ??
      ]

