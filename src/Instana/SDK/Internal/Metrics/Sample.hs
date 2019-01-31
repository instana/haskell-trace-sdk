{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Metrics.Sample
Description : Instana's internal representation of metrics values and samples.
-}
module Instana.SDK.Internal.Metrics.Sample
  ( InstanaSample
  , InstanaMetricValue(..)
  , SampleJson(..)
  , TimedSample(..)
  , ValueJson(..)
  , ekgSampleToInstanaSample
  , ekgValueToInstanaValue
  , empty
  , encodeSample
  , encodeValue
  , isMarkedForReset
  , markForReset
  , mkTimedSample
  , timedSampleFromEkgSample
  ) where


import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Types    as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import qualified System.Metrics      as Metrics


-- |A collection of metric values.
type InstanaSample = HashMap Text InstanaMetricValue


-- |A single metric value.
data InstanaMetricValue =
  -- |A string metric value.
    StringValue     Text
  -- |An integral metric value.
  | IntegralValue   Int
  -- |A fractional metric value.
  | FractionalValue Double
  deriving (Eq, Generic, Show)


-- |Converts an ekg-core sample into an Instana sample.
ekgSampleToInstanaSample :: Metrics.Sample -> InstanaSample
ekgSampleToInstanaSample =
  HashMap.map ekgValueToInstanaValue


-- |Converts an ekg-core metric value into an Instana metric value.
ekgValueToInstanaValue :: Metrics.Value -> InstanaMetricValue
ekgValueToInstanaValue ekgValue =
  case ekgValue of
    Metrics.Label text     -> StringValue text
    Metrics.Counter int64  -> IntegralValue (fromIntegral int64)
    Metrics.Gauge int64    -> IntegralValue (fromIntegral int64)
    Metrics.Distribution _ -> StringValue "distribution"


-- |A metrics sample with timestamp
data TimedSample =
  TimedSample
    {
      -- |The metrics sample
      sample    :: InstanaSample
      -- |The timestamp
    , timestamp :: Int
    , resetNext :: Bool
    } deriving (Eq, Generic, Show)


-- |Creates an empty sample with a timestamp.
empty :: Int -> TimedSample
empty t =
  TimedSample {
    sample    = HashMap.empty
  , timestamp = t
  , resetNext = False
  }


-- |Creates a sample with a timestamp.
mkTimedSample :: InstanaSample -> Int -> TimedSample
mkTimedSample sampledMetrics t =
  TimedSample {
    sample    = sampledMetrics
  , timestamp = t
  , resetNext = False
  }


-- |Creates a sample with a timestamp from an ekg-core sample.
timedSampleFromEkgSample :: Metrics.Sample -> Int -> TimedSample
timedSampleFromEkgSample sampledMetrics =
  mkTimedSample (ekgSampleToInstanaSample sampledMetrics)


-- |Marks the sample for a reset on the next metric collection tick.
markForReset :: TimedSample -> TimedSample
markForReset timedSample =
  timedSample { resetNext = True }


-- |Checks if the sample is marked for reset.
isMarkedForReset :: TimedSample -> Bool
isMarkedForReset = resetNext


-- |Encodes a sample to JSON.
encodeSample :: InstanaSample -> A.Value
encodeSample metrics =
    buildOne metrics $ A.emptyObject
  where
    buildOne :: HashMap T.Text InstanaMetricValue -> A.Value -> A.Value
    buildOne m o = HashMap.foldlWithKey' build o m

    build :: A.Value -> T.Text -> InstanaMetricValue -> A.Value
    build m name val = go m (T.splitOn "." name) val

    go :: A.Value -> [T.Text] -> InstanaMetricValue -> A.Value
    go (A.Object m) [str] val      = A.Object $ HashMap.insert str metric m
      where metric = encodeValue val
    go (A.Object m) (str:rest) val = case HashMap.lookup str m of
        Nothing -> A.Object $ HashMap.insert str (go A.emptyObject rest val) m
        Just m' -> A.Object $ HashMap.insert str (go m' rest val) m
    go v _ _                        = typeMismatch "Object" v

typeMismatch :: String   -- ^ The expected type
             -> A.Value  -- ^ The actual value encountered
             -> a
typeMismatch expected actual =
    error $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
    " instead"
  where
    name = case actual of
        A.Object _ -> "Object"
        A.Array _  -> "Array"
        A.String _ -> "String"
        A.Number _ -> "Number"
        A.Bool _   -> "Boolean"
        A.Null     -> "Null"


-- |Encodes a single metric value to JSON
encodeValue :: InstanaMetricValue -> A.Value
encodeValue (IntegralValue   n) = Aeson.toJSON n
encodeValue (FractionalValue f) = Aeson.toJSON f
encodeValue (StringValue     s) = Aeson.toJSON s


-- |A type wrapper to convert a sample to JSON.
newtype SampleJson = SampleJson InstanaSample
    deriving Show

instance A.ToJSON SampleJson where
    toJSON (SampleJson s) = encodeSample s


-- |A type wrapper to convert a metric value to JSON.
newtype ValueJson = ValueJson InstanaMetricValue
    deriving Show

instance A.ToJSON ValueJson where
    toJSON (ValueJson v) = encodeValue v

