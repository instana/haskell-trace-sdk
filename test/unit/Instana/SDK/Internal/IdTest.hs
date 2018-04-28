{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.IdTest (allTests) where


import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Test.HUnit

import           Instana.SDK.Internal.Id    (Id)
import qualified Instana.SDK.Internal.Id    as Id


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldEncodeIntComponents" shouldEncodeIntComponents
    , TestLabel "shouldEncodeIntComponents2" shouldEncodeIntComponents2
    , TestLabel "shouldEncodeStringId" shouldEncodeStringId
    , TestLabel "shouldDecodeStringId " shouldDecodeStringId
    , TestLabel "shouldGenerate" shouldGenerate
    ]

shouldEncodeIntComponents :: Test
shouldEncodeIntComponents =
  let
    idFromInts = Id.createFromIntsForTest [12345, 67890]
    encoded = BS.unpack . Aeson.encode $ idFromInts
  in
    TestCase $
      assertEqual "encoded" "\"00000000000030390000000000010932\"" encoded


shouldEncodeIntComponents2 :: Test
shouldEncodeIntComponents2 =
  let
    -- minimum for Int type as per
    -- http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Int.html
    -- is from -2^29 to 29-1 -> -536870912 to 536870911
    idFromInts = Id.createFromIntsForTest [-536870912, 536870911, 536870911, 536870911]
    encoded = BS.unpack . Aeson.encode $ idFromInts
  in
    TestCase $
      assertEqual "encoded" "\"200000001fffffff1fffffff1fffffff\"" encoded


shouldEncodeStringId :: Test
shouldEncodeStringId =
  let
    idFromString = Id.fromString "some string"
    encoded = BS.unpack . Aeson.encode $ idFromString
  in
    TestCase $
      assertEqual "encoded" "\"some string\"" encoded


shouldDecodeStringId :: Test
shouldDecodeStringId =
  let
    expected = Id.fromString "some string"
    maybeId = Aeson.decode (BS.pack  "\"some string\"") :: Maybe Id
    Just decoded = maybeId
  in
    TestCase $ assertEqual "decoded" expected decoded


shouldGenerate :: Test
shouldGenerate =
  TestCase $
    do
      generated <- Id.generate
      let
        encoded = BS.unpack . Aeson.encode $ generated
      -- every encoded ID must be 32 chars wide + 2 chars for leading and
      -- trailing "
      assertEqual "generated" 34 (length encoded)

