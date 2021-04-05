{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Internal.Id
Description : A module for working with trace IDs and span IDs
-}
module Instana.SDK.Internal.Id
   ( Id
   , generate
   , fromString
   , fromText
   , longOrShortTraceId
   , longTraceId
   , toByteString
   , toByteStringUnshortened
   , toString
   , toStringUnshortened
   , toText
   -- exposed for testing purposes
   , createFromIntsForTest
   )
   where


import           Control.Monad             (replicateM)
import           Data.Aeson                (FromJSON, ToJSON, Value)
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (Parser)
import qualified Data.ByteString.Char8     as BSC8
import qualified Data.String               (IsString (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import           Numeric                   (showHex)
import qualified System.Random             as Random

import           Instana.SDK.Internal.Util (leftPad)


-- |Represents an ID (trace ID, span ID).
data Id =
    -- |a representation of a 64 bit ID with just enough Int components to
    -- reach 64 bits (used when generating new random IDs)
    IntComponents [Int]
    -- |a representation of a 64 bit ID as a plain string (used when
    -- deserializing IDs, for example when reading HTTP headers)
  | IdString String
    -- |a 16 character/64 bit ID that has been shortened from a longer original.
    -- The first component is the shortened ID, the second the is original ID.
  | ShortenedId String String
  deriving (Eq, Generic, Show)


instance FromJSON Id where
  parseJSON :: Value -> Parser Id
  parseJSON = Aeson.withText "Id string" $
    \string -> return $ fromString $ (T.unpack string)


instance ToJSON Id where
  toJSON :: Id -> Value
  toJSON =
    Aeson.String . toText


instance Data.String.IsString Id where
  fromString = fromString


appendAsHex :: Int -> String -> Int -> String
appendAsHex noOfComponents accumulator intValue =
  appendPaddedHex accumulator intValue
  where
    toHex = (flip showHex) "" . abs
    padding = 64 `div` noOfComponents `div` 4
    toPaddedHex = leftPad padding . toHex
    appendPaddedHex = flip ((++) . toPaddedHex)


-- |Generates a new random ID.
generate :: IO Id
generate = do
  -- The number of bits used for an Haskell Int depends on the GHC
  -- implementation. It is guaranteed to cover the range from -2^29 to 2^29 - 1.
  -- On modern systems it is often -2^63 to 2^63 - 1.
  --
  -- We need 64 bits, so we actually need to generate multiple Ints (usually
  -- two) and stitch them together during JSON decoding.
  let
    requiredNumberOfIntComponents = 64 `div` bitsPerInt
  (randomInts :: [Int]) <-
    replicateM requiredNumberOfIntComponents Random.randomIO
  return $ IntComponents $ randomInts


bitsPerInt :: Int
bitsPerInt =
  floor $ logBase (2 :: Double) $ fromIntegral (maxBound :: Int)


-- |Converts an ID into a String
toString :: Id -> String
toString theId =
  case theId of
    IntComponents intComponents ->
      let
        noOfComponents = length intComponents
      in
      foldl
        (appendAsHex noOfComponents)
        ""
        (reverse intComponents)
    IdString string ->
      string
    ShortenedId string _ ->
      string


-- |Converts an ID into a String, using the unshortened value (in case this is a
-- ShortenedId).
toStringUnshortened :: Id -> String
toStringUnshortened theId =
  case theId of
    ShortenedId _ unshortened ->
      unshortened
    _ ->
      toString theId


-- |Retrieves the original long trace ID as a string in case this ID has been
-- created by shortening a 128 bit trace ID, or Nothing otherwise.
longTraceId :: Id -> Maybe String
longTraceId theId =
  case theId of
    ShortenedId _ original -> Just original
    _                      -> Nothing


-- |Retrieves the original long trace ID as a string in case this ID has been
-- created by shortening a 128 bit trace ID, or the full ID as a string
-- otherwise.
longOrShortTraceId :: Id -> String
longOrShortTraceId theId =
  case theId of
    ShortenedId _ original -> original
    _                      -> toString theId


--- |Converts a string into an ID.
fromString :: String -> Id
fromString =
  fromText . T.pack


--- |Converts a Text into an ID.
fromText :: Text -> Id
fromText t =
  if T.length t > 16 then
    ShortenedId (T.unpack $ T.takeEnd 16 t) (T.unpack t)
  else
    IdString $ T.unpack t


-- |Converts an ID into a Text
toText :: Id -> Text
toText =
  T.pack . toString


-- |Converts an ID into a ByteString
toByteString :: Id -> BSC8.ByteString
toByteString =
  BSC8.pack . toString


-- |Converts an ID into a ByteString, providing the unshortened value (this
-- only makes a difference when this is a ShortenedId).
toByteStringUnshortened :: Id -> BSC8.ByteString
toByteStringUnshortened =
  BSC8.pack . toStringUnshortened


-- |Only exposed for testing, do not use this.
createFromIntsForTest :: [Int] -> Id
createFromIntsForTest = IntComponents

