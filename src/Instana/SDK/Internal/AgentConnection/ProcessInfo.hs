module Instana.SDK.Internal.AgentConnection.ProcessInfo
    ( ProcessInfo(..)
    ) where


data ProcessInfo =
  ProcessInfo
    { pidString   :: String
    , programName :: String
    , arguments   :: [String]
    }

