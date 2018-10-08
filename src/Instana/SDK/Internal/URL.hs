{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.URL
Description : Representation of an URL
-}
module Instana.SDK.Internal.URL
  ( URL
  , mkHttp
  , mkHttps
  , mkUrl
  ) where


import           GHC.Generics


data Protocol = HTTP | HTTPS
  deriving (Eq, Generic)


instance Show Protocol where
  show HTTP  = "http://"
  show HTTPS = "https://"


-- |Represents a URL.
data URL = URL
  { protocol :: Protocol
  , host     :: String
  , port     :: Int
  , path     :: String
  } deriving (Eq, Generic)


instance Show URL where
  show url =
    (show $ protocol url)  ++
    (host url) ++ ":" ++
    (show $ port url) ++ "/" ++
    (path url)


-- |Creates a URL.
mkUrl ::
  Protocol
  -> String
  -> Int
  -> String
  -> URL
mkUrl _protocol _host _port _path =
  URL
  { protocol = _protocol
  , host = _host
  , port = _port
  , path = _path
  }


-- |Creates a HTTP URL.
mkHttp ::
  String
  -> Int
  -> String
  -> URL
mkHttp = mkUrl HTTP


-- |Creates a HTTPS URL.
mkHttps ::
  String
  -> Int
  -> String
  -> URL
mkHttps = mkUrl HTTPS

