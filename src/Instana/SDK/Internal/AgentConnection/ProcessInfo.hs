{-|
Module      : Instana.SDK.Internal.AgentConnection.ProcessInfo
Description : Holds meta data about an OS process.
-}
module Instana.SDK.Internal.AgentConnection.ProcessInfo
    ( ProcessInfo(..)
    ) where


-- |Holds meta data about an OS process.
data ProcessInfo =
  ProcessInfo
    { pidString   :: String
    , programName :: String
    , arguments   :: [String]
    }

