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
   -- exposed for testing purposes
   , createFromIntsForTest
   )
   where


import           Control.Monad    (replicateM)
import           Data.Aeson       (FromJSON, ToJSON, Value)
import qualified Data.Aeson       as Aeson
import           Data.Aeson.Types (Parser)
import           Data.List        (foldl)
import qualified Data.Text        as T
import           GHC.Generics
import           Numeric          (showHex)
import qualified System.Random    as Random


data Id =
    -- | a representation of a 128 bit ID with just enough Int components to
    -- reach 128 bits (used when generating new random IDs)
    IntComponents [Int]
    -- | a representation of a 128 bit ID as a plain string (used when
    -- deserializing IDs, for example when reading HTTP headers)
  | IdString String
  deriving (Eq, Generic, Show)


instance FromJSON Id where
  parseJSON :: Value -> Parser Id
  parseJSON = Aeson.withText "Id string" $
    \string -> return $ IdString $ (T.unpack string)


instance ToJSON Id where
  toJSON :: Id -> Value
  toJSON theId =
    case theId of
      IntComponents intComponents ->
        Aeson.String . T.pack $ concatenated
          where
            noOfComponents = length intComponents
            concatenated =
              foldl
                (appendAsHex noOfComponents)
                ""
                (reverse intComponents)
      IdString string ->
        Aeson.String . T.pack $ string


appendAsHex :: Int -> String -> Int -> String
appendAsHex noOfComponents accumulator intValue =
  appendPaddedHex accumulator intValue
  where
    toHex = (flip showHex) "" . abs
    padding = 128 `div` noOfComponents `div` 4
    toPaddedHex = leftPad padding . toHex
    appendPaddedHex = flip ((++) . toPaddedHex)


leftPad :: Int -> String -> String
leftPad digits s
  | length s < digits = replicate (digits - length s) '0' ++ s
  | otherwise         = s


generate :: IO Id
generate = do
  -- The number of bits used for an Haskell Int depends on the GHC
  -- implementation. It is guaranteed to cover the range from -2^29 to 2^29 - 1.
  -- On modern systems it is often -2^63 to 2^63 - 1.
  --
  -- We need 128 bits, so we actually need to generate multiple Ints (usually
  -- two) and stitch them together during JSON decoding.
  let
    requiredNumberOfIntComponents = 128 `div` bitsPerInt
  (randomInts :: [Int]) <-
    replicateM requiredNumberOfIntComponents Random.randomIO
  return $ IntComponents $ randomInts


bitsPerInt :: Int
bitsPerInt =
  floor $ logBase (2 :: Double) $ fromIntegral (maxBound :: Int)


fromString :: String -> Id
fromString = IdString


createFromIntsForTest :: [Int] -> Id
createFromIntsForTest = IntComponents

