{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Internal.AgentConnection.SchedFile
Description : Parses the content of the sched file.
-}
module Instana.SDK.Internal.AgentConnection.SchedFile
    ( parsePidFromSchedFile
    ) where


import qualified Text.Regex.Base.RegexLike as RegexBase
import qualified Text.Regex.TDFA           as Regex
import           Text.Regex.TDFA.String    (Regex)
import qualified Text.Regex.TDFA.String    as RegexString


parsePidFromSchedFile :: String -> Maybe String
parsePidFromSchedFile schedFileContent =
  let
    allMatches :: [[String]]
    allMatches = RegexBase.match schedFilePidPattern schedFileContent
  in
  if null allMatches then
    Nothing
  else do
    let
      (firstMatchWithCaptures :: [String]) = head allMatches
    if (length firstMatchWithCaptures) < 2 then
      Nothing
    else
      Just $ head $ tail $ firstMatchWithCaptures


schedFilePidPattern :: Regex
schedFilePidPattern =
  let
    compiledPattern =
      RegexString.compile
        (Regex.defaultCompOpt
          { Regex.caseSensitive = False
          , Regex.multiline = True
          }
        )
        Regex.defaultExecOpt
        "^[^(]+\\(([0-9]+),"
    Right pattern = compiledPattern
  in
  pattern

