{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Modified version of
-- https://github.com/tibbe/ekg-json/blob/484a4ed107d925086027fe4c66ff103de06003a3/System/Metrics/Json.hs
--
-- Copyright (c) 2015, Johan Tibell
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Johan Tibell nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-|
Module      : Instana.SDK.Internal.Metrics.Json
Description : ToJSON instances for System.Metrics values
-}
module Instana.SDK.Internal.Metrics.Json
  ( encodeSample
  , encodeValue
  , Sample(..)
  , Value(..)
  ) where

import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Types            as A
import qualified Data.HashMap.Strict         as M
import qualified Data.Text                   as T
import qualified System.Metrics              as Metrics
import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * Converting metrics to JSON values


-- | Encode metrics as nested JSON objects. Each "." in the metric
-- name introduces a new level of nesting. For example, the metrics
-- @[("foo.bar", 10), ("foo.baz", "label")]@ are encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": {
-- >       "type:", "c",
-- >       "val": 10
-- >     },
-- >     "baz": {
-- >       "type": "l",
-- >       "val": "label"
-- >     }
-- >   }
-- > }
--
encodeSample :: Metrics.Sample -> A.Value
encodeSample metrics =
    buildOne metrics $ A.emptyObject
  where
    buildOne :: M.HashMap T.Text Metrics.Value -> A.Value -> A.Value
    buildOne m o = M.foldlWithKey' build o m

    build :: A.Value -> T.Text -> Metrics.Value -> A.Value
    build m name val = go m (T.splitOn "." name) val

    go :: A.Value -> [T.Text] -> Metrics.Value -> A.Value
    go (A.Object m) [str] val      = A.Object $ M.insert str metric m
      where metric = encodeValue val
    go (A.Object m) (str:rest) val = case M.lookup str m of
        Nothing -> A.Object $ M.insert str (go A.emptyObject rest val) m
        Just m' -> A.Object $ M.insert str (go m' rest val) m
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


-- | Encodes a single metric as a JSON object.
encodeValue :: Metrics.Value -> A.Value
encodeValue (Metrics.Counter n)      = Aeson.toJSON n
encodeValue (Metrics.Gauge n)        = Aeson.toJSON n
encodeValue (Metrics.Label l)        = Aeson.toJSON l
encodeValue (Metrics.Distribution l) = distrubtionToJson l


-- | Convert a distribution to a JSON value (we do not use distributions,
-- but for completeness sake we at least provide means to convert them to Json).
distrubtionToJson :: Distribution.Stats -> A.Value
distrubtionToJson stats = A.object
    [ "mean" .= Distribution.mean stats
    , "variance" .= Distribution.variance stats
    , "count" .= Distribution.count stats
    , "sum" .= Distribution.sum stats
    , "min" .= Distribution.min stats
    , "max" .= Distribution.max stats
    ]

------------------------------------------------------------------------
-- ** Newtype wrappers with instances

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Sample' without creating an orphan instance.
newtype Sample = Sample Metrics.Sample
    deriving Show

-- | Uses 'encodeSample'.
instance A.ToJSON Sample where
    toJSON (Sample s) = encodeSample s

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Value' without creating an orphan instance.
newtype Value = Value Metrics.Value
    deriving Show

-- | Uses 'encodeValue'.
instance A.ToJSON Value where
    toJSON (Value v) = encodeValue v

