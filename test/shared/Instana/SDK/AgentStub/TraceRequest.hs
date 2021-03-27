{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.TraceRequest where


import           Data.Aeson          (FromJSON, ToJSON, Value (Object), (.:),
                                      (.:?), (.=))
import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           GHC.Generics


type TraceRequest = [Span]


data From = From
  { entityId  :: String
  , agentUuid :: String
  } deriving (Eq, Generic, Show)


instance FromJSON From where
  parseJSON = Aeson.withObject "from" $
    \fr ->
      From
        <$> fr .: "e" -- entityId
        <*> fr .: "h" -- agent UUID/host ID


instance ToJSON From where
  toJSON :: From -> Value
  toJSON fr = Aeson.object
    [ "e" .= entityId fr
    , "h" .= agentUuid fr
    ]


data InstanaAncestor = InstanaAncestor
  { traceId  :: Maybe String
  , parentId :: Maybe String
  } deriving (Eq, Generic, Show)


instance FromJSON InstanaAncestor where
  parseJSON = Aeson.withObject "InstanaAncestor" $
    \obj ->
      InstanaAncestor
        <$> obj .: "t"
        <*> obj .: "p"


instance ToJSON InstanaAncestor where
  toJSON :: InstanaAncestor -> Value
  toJSON instanaAncestor = Aeson.object
    [ "t" .= traceId instanaAncestor
    , "p" .= parentId instanaAncestor
    ]


data Span =
  Span
    { t        :: String       -- traceId
    , s        :: String       -- spanId
    , p        :: Maybe String -- parentId
    , n        :: Text         -- spanName
    , ts       :: Int          -- timestamp
    , d        :: Int          -- duration
    , k        :: Int          -- kind
    , ec       :: Int          -- errorCount
    , ia       :: Maybe InstanaAncestor -- instana ancestor
    , tp       :: Maybe Bool   -- traceparent has been used for trace continuity
    , lt       :: Maybe String -- long trace ID
    , crtp     :: Maybe String -- correlation type
    , crid     :: Maybe String -- correlation id
    , sy       :: Maybe Bool   -- synthetic
    , spanData :: Aeson.Value  -- spanData
    , f        :: Maybe From   -- from
    } deriving (Eq, Show, Generic)


instance FromJSON Span where
  parseJSON = Aeson.withObject "span" $
    \decodedObject ->
      Span
        <$> decodedObject .:  "t"
        <*> decodedObject .:  "s"
        <*> decodedObject .:  "p"
        <*> decodedObject .:  "n"
        <*> decodedObject .:  "ts"
        <*> decodedObject .:  "d"
        <*> decodedObject .:  "k"
        <*> decodedObject .:  "ec"
        <*> decodedObject .:? "ia"
        <*> decodedObject .:? "tp"
        <*> decodedObject .:? "lt"
        <*> decodedObject .:  "crtp"
        <*> decodedObject .:  "crid"
        <*> decodedObject .:? "sy"
        <*> decodedObject .:  "data"
        <*> decodedObject .:  "f"


instance ToJSON Span where
  toJSON sp = Aeson.object
    [ "t"    .= t sp
    , "s"    .= s sp
    , "p"    .= p sp
    , "n"    .= n sp
    , "ts"   .= ts sp
    , "d"    .= d sp
    , "k"    .= k sp
    , "ec"   .= ec sp
    , "ia"   .= ia sp
    , "tp"   .= tp sp
    , "lt"   .= lt sp
    , "crtp" .= crtp sp
    , "crid" .= crid sp
    , "sy"   .= sy sp
    , "data" .= spanData sp
    , "f"    .= f sp
    ]


readSdkName :: Span -> Maybe Text
readSdkName span_ =
  let
    value = extractProperty ["sdk", "name"] (spanData span_)
  in
    case value of
      Just (Aeson.String sdkName) -> Just sdkName
      _                           -> Nothing


extractProperty :: [Text] -> Value -> Maybe Value
extractProperty [] value              = Just value
extractProperty (key:keys) (Object o) = HM.lookup key o >>= extractProperty keys
extractProperty _      _              = Nothing

