{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.IdTest (allTests) where


import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import           Test.HUnit

import           Instana.SDK.Internal.Id    (Id)
import qualified Instana.SDK.Internal.Id    as Id


allTests :: Test
allTests =
  TestList
    [ TestLabel
        "shouldConvertIntComponentsToString"
        shouldConvertIntComponentsToString
    , TestLabel "shouldEncodeIntComponents" shouldEncodeIntComponents
    , TestLabel
        "shouldConvertIntComponentsToString2"
        shouldConvertIntComponentsToString2
    , TestLabel "shouldEncodeIntComponents2" shouldEncodeIntComponents2
    , TestLabel "shouldEncodeStringId" shouldEncodeStringId
    , TestLabel "shouldDecodeStringId " shouldDecodeStringId
    , TestLabel "shouldGenerate" shouldGenerate
    ]


shouldConvertIntComponentsToString :: Test
shouldConvertIntComponentsToString =
  let
    asString = Id.toString $ Id.createFromIntsForTest [12345, 67890]
  in
    TestCase $
      assertEqual "as string" "0000303900010932" asString


shouldEncodeIntComponents :: Test
shouldEncodeIntComponents =
  let
    idFromInts = Id.createFromIntsForTest [12345, 67890]
    encoded = LBSC8.unpack . Aeson.encode $ idFromInts
  in
    TestCase $
      assertEqual "as string" "\"0000303900010932\"" encoded


shouldConvertIntComponentsToString2 :: Test
shouldConvertIntComponentsToString2 =
  let
    -- minimum for Int type as per
    -- http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Int.html
    -- is from -2^29 to 29-1 -> -536870912 to 536870911
    asString =
      Id.toString $ Id.createFromIntsForTest [-536870912, 536870911]
  in
    TestCase $
      assertEqual "as string" "200000001fffffff" asString


shouldEncodeIntComponents2 :: Test
shouldEncodeIntComponents2 =
  let
    idFromInts = Id.createFromIntsForTest [-536870912, 536870911]
    encoded = LBSC8.unpack . Aeson.encode $ idFromInts
  in
    TestCase $
      assertEqual "encoded" "\"200000001fffffff\"" encoded


shouldEncodeStringId :: Test
shouldEncodeStringId =
  let
    idFromString = ("some string" :: Id)
    encoded = LBSC8.unpack . Aeson.encode $ idFromString
  in
    TestCase $
      assertEqual "encoded" "\"some string\"" encoded


shouldDecodeStringId :: Test
shouldDecodeStringId =
  let
    expected = ("some string" :: Id)
    maybeId = Aeson.decode (LBSC8.pack  "\"some string\"") :: Maybe Id
    Just decoded = maybeId
  in
    TestCase $ assertEqual "decoded" expected decoded


shouldGenerate :: Test
shouldGenerate =
  TestCase $
    do
      generated <- Id.generate
      let
        encoded = LBSC8.unpack . Aeson.encode $ generated
      -- every encoded ID must be 16 chars wide + 2 chars for leading and
      -- trailing "
      assertEqual "generated" 18 (length encoded)

