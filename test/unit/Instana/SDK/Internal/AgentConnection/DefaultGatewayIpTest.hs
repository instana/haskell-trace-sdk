{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.Internal.AgentConnection.DefaultGatewayIpTest (allTests) where


import           Test.HUnit

import           Instana.SDK.Internal.AgentConnection.DefaultGatewayIp (extractDefaultGatewayIp)


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldExtractDefaultGatewayIpFromFile" shouldExtractDefaultGatewayIpFromFile
    , TestLabel "shouldNotExtractFromEmptyFile" shouldNotExtractFromEmptyFile
    , TestLabel "shouldNotExtractFromLinesWithTooFewFields" shouldNotExtractFromLinesWithTooFewFields
    , TestLabel "shouldNotExtractIfNoLineHas8Zeroes" shouldNotExtractIfNoLineHas8Zeroes
    , TestLabel "shouldNotExtractIfThirdFieldHasWrongLength" shouldNotExtractIfThirdFieldHasWrongLength
    , TestLabel "shouldCopeWithThirdFieldNotAHexString" shouldCopeWithThirdFieldNotAHexString
    ]


shouldExtractDefaultGatewayIpFromFile :: Test
shouldExtractDefaultGatewayIpFromFile =
  let
    defaultGatewayIp =
      extractDefaultGatewayIp $
        "Iface\tDestination\tGateway\tFlags\tRefCnt\tUse\tMetricMask\tMTU\tWindow\tIRTT\n" ++
        "eth0\t0000F1AC\t00000000\t0001\t0\t0\t0\t0000FFFF0\t0\t0\n" ++
        "eth0\t00000000\t010011AC\t0003\t0\t0\t0\t000000000\t0\t0\n" ++
        "eth0\t000011AC\t00000000\t0001\t0\t0\t0\t0000FFFF0\t0\t0"
  in
  TestCase $
    assertEqual "default gateway IP" (Just "172.17.0.1") defaultGatewayIp


shouldNotExtractFromEmptyFile :: Test
shouldNotExtractFromEmptyFile =
  let
    defaultGatewayIp = extractDefaultGatewayIp ""
  in
  TestCase $
    assertEqual "default gateway IP" Nothing defaultGatewayIp


shouldNotExtractFromLinesWithTooFewFields :: Test
shouldNotExtractFromLinesWithTooFewFields =
  let
    defaultGatewayIp =
      extractDefaultGatewayIp $
        "Iface\tDestination\n" ++
        "eth0\t00000000"
  in
  TestCase $
    assertEqual "default gateway IP" Nothing defaultGatewayIp


shouldNotExtractIfNoLineHas8Zeroes :: Test
shouldNotExtractIfNoLineHas8Zeroes =
  let
    defaultGatewayIp =
      extractDefaultGatewayIp $
        "eth0\t00009000\t010011AC\t0003\t0\t0\t0\t000000000\t0\t0"
  in
  TestCase $
    assertEqual "default gateway IP" Nothing defaultGatewayIp


shouldNotExtractIfThirdFieldHasWrongLength :: Test
shouldNotExtractIfThirdFieldHasWrongLength =
  let
    defaultGatewayIp =
      extractDefaultGatewayIp $
        "eth0\t00000000\t010011A\t0003\t0\t0\t0\t000000000\t0\t0"
  in
  TestCase $
    assertEqual "default gateway IP" Nothing defaultGatewayIp


shouldCopeWithThirdFieldNotAHexString :: Test
shouldCopeWithThirdFieldNotAHexString =
  let
    defaultGatewayIp =
      extractDefaultGatewayIp $
        "eth0\t00000000\tghijklmn\t0003\t0\t0\t0\t000000000\t0\t0"
  in
  TestCase $
    assertEqual "default gateway IP" (Just "0.0.0.0") defaultGatewayIp
