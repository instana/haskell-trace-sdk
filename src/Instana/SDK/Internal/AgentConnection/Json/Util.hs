{-|
Module      : Instana.SDK.Internal.AgentConnection.Json.Util
Description : A utility for JSON encoding/decoding.
-}
module Instana.SDK.Internal.AgentConnection.Json.Util where


import           Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Client  as HTTP


-- |Creates a decoder for HTTP responses without a body.
emptyResponseDecoder :: HTTP.Response ByteString -> IO Bool
emptyResponseDecoder _ =
  return True
