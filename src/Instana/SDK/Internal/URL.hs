{-# LANGUAGE DeriveGeneric #-}
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


mkHttp ::
  String
  -> Int
  -> String
  -> URL
mkHttp = mkUrl HTTP


mkHttps ::
  String
  -> Int
  -> String
  -> URL
mkHttps = mkUrl HTTPS

