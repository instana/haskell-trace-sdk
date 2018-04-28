{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.AgentStub.TraceRequest where


import           Data.Aeson   (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson   as Aeson
import           Data.Text    (Text)
import           GHC.Generics


type TraceRequest = [Span]


data Span =
  Span
    { t         :: String       -- traceId
    , s         :: String       -- spanId
    , p         :: Maybe String -- parentId
    , n         :: Text         -- spanType
    , ts        :: Int          -- timestamp
    , d         :: Int          -- duration
    , k         :: Int          -- kind
    , label     :: Text         -- label
    , spanError :: Bool         -- spanError
    , spanData  :: Aeson.Value  -- spanData
    } deriving (Eq, Show, Generic)


instance FromJSON Span where
  parseJSON = Aeson.withObject "span" $
    \decodedObject ->
      Span
        <$> decodedObject .: "t"
        <*> decodedObject .: "s"
        <*> decodedObject .: "p"
        <*> decodedObject .: "n"
        <*> decodedObject .: "ts"
        <*> decodedObject .: "d"
        <*> decodedObject .: "k"
        <*> decodedObject .: "label"
        <*> decodedObject .: "error"
        <*> decodedObject .: "data"


instance ToJSON Span where
  toJSON sp = Aeson.object
    [ "t"     .= t sp
    , "s"     .= s sp
    , "p"     .= p sp
    , "n"     .= n sp
    , "ts"    .= ts sp
    , "d"     .= d sp
    , "k"     .= k sp
    , "label" .= label sp
    , "error" .= spanError sp
    , "data"  .= spanData sp
    ]

