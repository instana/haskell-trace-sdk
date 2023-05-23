{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.DefaultGatewayIp
Description : Extracts the default gateway IP from the content of /proc/self/net/route.
-}
module Instana.SDK.Internal.AgentConnection.DefaultGatewayIp
    ( extractDefaultGatewayIp
    ) where

import           Control.Monad (join)
import qualified Data.List     as List
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Numeric       (readHex)
import           Safe          (atMay)


-- |Parses the content of /proc/self/net/route to retrieve the IP of the default
-- gateway.
extractDefaultGatewayIp :: String -> Maybe String
extractDefaultGatewayIp routeFileContent =
  let
    allLines = lines routeFileContent
    linesAsFields = map (\l -> T.splitOn "\t" $ T.pack l) allLines
    lineWithDefaultGateway = List.find isDefaultGatewayLine linesAsFields
    ip = join $ fmap extractIpFromFields lineWithDefaultGateway
  in
  fmap T.unpack ip


isDefaultGatewayLine :: [Text] -> Bool
isDefaultGatewayLine fields =
  let
    field2 = atMay fields 1
    lenField3 = fmap T.length $ atMay fields 2
  in
  length fields >= 3 &&
    field2 == Just "00000000" &&
    lenField3 == Just 8


extractIpFromFields :: [Text] -> Maybe Text
extractIpFromFields fields =
  fmap convertHexStringToIp $ atMay fields 2


convertHexStringToIp :: Text -> Text
convertHexStringToIp hexstring =
  let
    o1 = substringLength2 6 hexstring
    o2 = substringLength2 4 hexstring
    o3 = substringLength2 2 hexstring
    o4 = T.take 2 hexstring
  in
  T.intercalate "." $ map hexstringToDecimal [o1, o2, o3, o4]


substringLength2 :: Int -> Text -> Text
substringLength2 start =
  T.take 2 . T.drop start


hexstringToDecimal :: Text -> Text
hexstringToDecimal hexstring =
  let
    result = readHex $ T.unpack hexstring
  in
  case result of
      (x,_):_ -> T.pack $ show (x :: Int)
      _       -> "0"
