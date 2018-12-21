{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Internal.Secrets
Description : Secrets scrubbing
-}
module Instana.SDK.Internal.Secrets
    ( MatcherMode(..)
    , SecretsMatcher(..)
    , defaultSecretsMatcher
    , isSecret
    ) where

import           Data.Aeson                (FromJSON, Value, (.:))
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (Parser)
import qualified Data.Either               as Either
import qualified Data.List                 as List
import qualified Data.Maybe                as Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import qualified Text.Regex.Base.RegexLike as RegexBase
import qualified Text.Regex.TDFA           as Regex
import           Text.Regex.TDFA.String    (Regex)
import qualified Text.Regex.TDFA.String    as RegexString

import           Instana.SDK.Internal.Util ((|>))


data MatcherMode =
    Equals
  | EqualsIgnoreCase
  | Contains
  | ContainsIgnoreCase
  | Regex
  | None
  deriving (Eq, Show, Generic)


instance FromJSON MatcherMode where
  parseJSON :: Value -> Parser MatcherMode
  parseJSON = Aeson.withText "secrets matcher mode string" $
    \matcherModeText ->
      case matcherModeText of
        "equals-ignore-case"   -> return EqualsIgnoreCase
        "equals"               -> return Equals
        "contains-ignore-case" -> return ContainsIgnoreCase
        "contains"             -> return Contains
        "regex"                -> return Regex
        "none"                 -> return None
        _                      ->
          fail $ "unknown secrets matcher mode: " ++ (T.unpack matcherModeText)


data SecretsMatcher =
    EqualsMatcher [Text]
  | EqualsIgnoreCaseMatcher [Text]
  | ContainsMatcher [Text]
  | ContainsIgnoreCaseMatcher [Text]
  | RegexMatcher [Regex]
  | NoneMatcher


instance FromJSON SecretsMatcher where
  parseJSON = Aeson.withObject "SecretsMatcher" $ parseSecretsConfig


instance Eq SecretsMatcher where
  (==) :: SecretsMatcher -> SecretsMatcher -> Bool
  s1 == s2 =
    case (s1, s2) of
      (RegexMatcher _, _) -> False
      (_, RegexMatcher _) -> False
      _                   -> s1 == s2


instance Show SecretsMatcher where
  show :: SecretsMatcher -> String
  show s =
    case s of
      RegexMatcher _ -> "RegexMatcher"
      _              -> show s


parseSecretsConfig :: Aeson.Object -> Parser SecretsMatcher
parseSecretsConfig object =
  (object .: "matcher") >>=
    (\matcherMode ->
      (object .: "list") >>= postProcessList matcherMode
    )
  where
    postProcessList :: MatcherMode -> [Text] -> Parser SecretsMatcher
    postProcessList matcherMode secretsList =
      case matcherMode of
        Equals   ->
          return $ EqualsMatcher secretsList
        EqualsIgnoreCase   ->
          return $ EqualsIgnoreCaseMatcher $ List.map T.toLower secretsList
        Contains ->
          return $ ContainsMatcher secretsList
        ContainsIgnoreCase ->
          return $ ContainsIgnoreCaseMatcher $ List.map T.toLower secretsList
        Regex ->
          return $ RegexMatcher $
            List.map preProcessRegexPattern secretsList
            |> Either.rights
        None ->
          return $ NoneMatcher


defaultSecretsMatcher :: SecretsMatcher
defaultSecretsMatcher =
  ContainsIgnoreCaseMatcher ["key", "pass", "secret"]


isSecret :: SecretsMatcher -> Text -> Bool
isSecret (EqualsMatcher secretsList) potentialSecret =
  elem potentialSecret secretsList
isSecret (EqualsIgnoreCaseMatcher secretsList) potentialSecret =
  elem (T.toLower potentialSecret) secretsList
isSecret (ContainsMatcher secretsList) potentialSecret =
  List.find (flip T.isInfixOf potentialSecret) secretsList |> Maybe.isJust
isSecret (ContainsIgnoreCaseMatcher secretsList) potentialSecret =
  let
    potentialSecret' = T.toLower potentialSecret
  in
  List.find (flip T.isInfixOf potentialSecret') secretsList |> Maybe.isJust
isSecret (RegexMatcher patterns) potentialSecret =
  let
    potentialSecret' = T.unpack potentialSecret
  in
  List.find
    (\pattern -> RegexBase.match pattern potentialSecret') patterns
    |> Maybe.isJust
isSecret (NoneMatcher) _ =
  False


preProcessRegexPattern :: Text -> Either String Regex
preProcessRegexPattern pattern =
  -- The Java regex matcher only matches if the whole string is a match,
  -- Haskell regexes RegExp.test matches if the regex is found as a substring.
  -- To achieve parity with the Java functionality, we enclose the regex in
  -- '^' and '$'.
  pattern
    |> prependCaret
    |> appendDollar
    |> T.unpack
    |> RegexString.compile
         Regex.defaultCompOpt
         Regex.defaultExecOpt

prependCaret :: Text -> Text
prependCaret t =
  if T.null t then t
  else if (T.head t == '^') then t
  else T.cons '^' t


appendDollar :: Text -> Text
appendDollar t =
  if T.null t then t
  else if (T.last t == '$') then t
  else T.snoc t '$'

