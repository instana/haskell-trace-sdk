{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.ProcessInfo
Description : Holds meta data about an OS process.
-}
module Instana.SDK.Internal.AgentConnection.ProcessInfo
    ( ProcessInfo(..)
    ) where


import           Data.Text    (Text)
import           GHC.Generics


-- |Holds meta data about an OS process.
data ProcessInfo =
  ProcessInfo
    { pidString         :: String
    , programName       :: String
    , executablePath    :: String
    , arguments         :: [String]
    , cpuSetFileContent :: Maybe Text
    , parentNsPid       :: Maybe String
    } deriving (Eq, Generic, Show)

