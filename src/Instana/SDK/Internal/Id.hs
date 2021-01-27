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
   , toByteString
   , toText
   -- exposed for testing purposes
   , createFromIntsForTest
   )
   where


import           Control.Monad         (replicateM)
import           Data.Aeson            (FromJSON, ToJSON, Value)
import qualified Data.Aeson            as Aeson
import           Data.Aeson.Types      (Parser)
import qualified Data.ByteString.Char8 as BSC8
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics
import           Numeric               (showHex)
import qualified System.Random         as Random


-- |Represents an ID (trace ID, span ID).
data Id =
    -- |a representation of a 64 bit ID with just enough Int components to
    -- reach 64 bits (used when generating new random IDs)
    IntComponents [Int]
    -- |a representation of a 64 bit ID as a plain string (used when
    -- deserializing IDs, for example when reading HTTP headers)
  | IdString String
  deriving (Eq, Generic, Show)


instance FromJSON Id where
  parseJSON :: Value -> Parser Id
  parseJSON = Aeson.withText "Id string" $
    \string -> return $ IdString $ (T.unpack string)


instance ToJSON Id where
  toJSON :: Id -> Value
  toJSON =
    Aeson.String . toText


appendAsHex :: Int -> String -> Int -> String
appendAsHex noOfComponents accumulator intValue =
  appendPaddedHex accumulator intValue
  where
    toHex = (flip showHex) "" . abs
    padding = 64 `div` noOfComponents `div` 4
    toPaddedHex = leftPad padding . toHex
    appendPaddedHex = flip ((++) . toPaddedHex)


leftPad :: Int -> String -> String
leftPad digits s
  | length s < digits = replicate (digits - length s) '0' ++ s
  | otherwise         = s


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


-- |Converts a string into an ID.
fromString :: String -> Id
fromString = IdString


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


-- |Converts an ID into a Text
toText :: Id -> Text
toText =
  T.pack . toString


-- |Converts an ID into a ByteString
toByteString :: Id -> BSC8.ByteString
toByteString =
  BSC8.pack . toString


-- |Only exposed for testing, do not use this.
createFromIntsForTest :: [Int] -> Id
createFromIntsForTest = IntComponents

