{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.Internal.SecretsTest (allTests) where


import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy.Char8   as LBSC8
import           Test.HUnit

import           Instana.SDK.Internal.Secrets (SecretsMatcher)
import qualified Instana.SDK.Internal.Secrets as Secrets
import           Instana.SDK.Internal.Util    ((|>))


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldMatchEquals" shouldMatchEquals
    , TestLabel "shouldNotMatchEquals" shouldNotMatchEquals
    , TestLabel "shouldMatchEqualsIgnoreCase" shouldMatchEqualsIgnoreCase
    , TestLabel "shouldNotMatchEqualsIgnoreCase" shouldNotMatchEqualsIgnoreCase
    , TestLabel "shouldMatchContains" shouldMatchContains
    , TestLabel "shouldNotMatchContains" shouldNotMatchContains
    , TestLabel "shouldMatchContainsIgnoreCase" shouldMatchContainsIgnoreCase
    , TestLabel
        "shouldNotMatchContainsIgnoreCase"
        shouldNotMatchContainsIgnoreCase
    , TestLabel "shouldMatchRegex" shouldMatchRegex
    , TestLabel "shouldMatchRegex2" shouldMatchRegex2
    , TestLabel "shouldNotMatchRegex" shouldNotMatchRegex
    , TestLabel
        "shouldNotMatchRegexCaseInsensitive"
        shouldNotMatchRegexCaseInsensitive
    , TestLabel
        "shouldNotMatchWhenRegexMatchesASubstring"
        shouldNotMatchWhenRegexMatchesASubstring
    , TestLabel
        "shouldHandleStartAndEndOfLineSpecialChars"
        shouldHandleStartAndEndOfLineSpecialChars
    , TestLabel
        "shouldHandleStartAndEndOfLineSpecialChars2"
        shouldHandleStartAndEndOfLineSpecialChars2
    , TestLabel
        "shouldHandleStartAndEndOfLineSpecialChars3"
        shouldHandleStartAndEndOfLineSpecialChars3
    ]


shouldMatchEquals :: Test
shouldMatchEquals =
  let
    config = createConfig "equals"
  in
  TestCase $
    assertBool "equals should match" $ Secrets.isSecret config "PASS"


shouldNotMatchEquals :: Test
shouldNotMatchEquals =
  let
    config = createConfig "equals"
  in
  TestCase $
    assertBool "equals should not match" $
      not $ Secrets.isSecret config "pass"


shouldMatchEqualsIgnoreCase :: Test
shouldMatchEqualsIgnoreCase =
  let
    config = createConfig "equals-ignore-case"
  in
  TestCase $
    assertBool "equals ignore case should match" $
      Secrets.isSecret config "pAsS"


shouldNotMatchEqualsIgnoreCase :: Test
shouldNotMatchEqualsIgnoreCase =
  let
    config = createConfig "equals-ignore-case"
  in
  TestCase $
    assertBool "equals ignore case should not match" $
      not $ Secrets.isSecret config "XpAssX"


shouldMatchContains :: Test
shouldMatchContains =
  let
    config = createConfig "contains"
  in
  TestCase $
    assertBool "contains should match" $ Secrets.isSecret config "ABCPASSDEF"


shouldNotMatchContains :: Test
shouldNotMatchContains =
  let
    config = createConfig "contains"
  in
  TestCase $
    assertBool "contains should not match" $
      not $ Secrets.isSecret config "abcpassdef"


shouldMatchContainsIgnoreCase :: Test
shouldMatchContainsIgnoreCase =
  let
    config = createConfig "contains-ignore-case"
  in
  TestCase $
    assertBool "contains ignore case should match" $
      Secrets.isSecret config "abcpAsSdef"


shouldNotMatchContainsIgnoreCase :: Test
shouldNotMatchContainsIgnoreCase =
  let
    config = createConfig "contains-ignore-case"
  in
  TestCase $
    assertBool "contains ignore case should not match" $
      not $ Secrets.isSecret config "abPp4SSdef"


shouldMatchRegex :: Test
shouldMatchRegex =
  TestCase $
    assertBool "regex should match" $
      Secrets.isSecret regexConfig "abcWHATEVERxyz"


shouldMatchRegex2 :: Test
shouldMatchRegex2 =
  TestCase $
    assertBool "regex should match 2" $
      Secrets.isSecret regexConfig "123"


shouldNotMatchRegex :: Test
shouldNotMatchRegex =
  TestCase $
    assertBool "regex should not match" $
      not $ Secrets.isSecret regexConfig "efgXuvw"


shouldNotMatchRegexCaseInsensitive :: Test
shouldNotMatchRegexCaseInsensitive =
  TestCase $
    assertBool "regex should not match case insensitive" $
      not $ Secrets.isSecret regexConfig "ABCWHATEVERXYZ"


shouldNotMatchWhenRegexMatchesASubstring :: Test
shouldNotMatchWhenRegexMatchesASubstring =
  TestCase $
    assertBool "regex should not match substring" $
      not $ Secrets.isSecret regexConfig "ZZZabcXXXxyzZZZ"


shouldHandleStartAndEndOfLineSpecialChars :: Test
shouldHandleStartAndEndOfLineSpecialChars =
  TestCase $
    assertBool "regex should handle ^ and $" $
      Secrets.isSecret (regexConfigFor "^regex$") "regex"


shouldHandleStartAndEndOfLineSpecialChars2 :: Test
shouldHandleStartAndEndOfLineSpecialChars2 =
  TestCase $
    assertBool "regex should handle ^ and $" $
      not $ Secrets.isSecret (regexConfigFor "^regex$") "xregex"


shouldHandleStartAndEndOfLineSpecialChars3 :: Test
shouldHandleStartAndEndOfLineSpecialChars3 =
  TestCase $
    assertBool "regex should handle ^ and $" $
      not $ Secrets.isSecret (regexConfigFor "^regex$") "regexx"

--   describe('none', function() {
--     var matcher;
--
--     beforeEach(function() {
--       matcher = secrets.matchers.none(['secret-key', 'anotherKey', 'whatever']);
--     });
--
--     it('should not match anything', function() {
--       expect(matcher('secret-key')).to.be.false;
--       expect(matcher('key')).to.be.false;
--       expect(matcher('pass')).to.be.false;
--     });
--   });
-- });


createConfig :: String -> SecretsMatcher
createConfig matcherMode =
  -- The JSON decoding applies preprocessing on the secrets list (for example,
  -- it lower cases all strings when the matcher is an ignore-case matcher), so
  -- it is part of the actual matching logic and needs to be included in the
  -- tests, so we create a SecretsMatcher by parsing some JSON.
  let
    maybeConfig :: Maybe SecretsMatcher
    maybeConfig =
      ("{ \"matcher\": \"" ++ matcherMode ++ "\", " ++
      "\"list\": [\"KEY\", \"PASS\", \"SECRET\"] }")
        |> LBSC8.pack
        |> Aeson.decode
    Just config = maybeConfig
  in
  config


regexConfig :: SecretsMatcher
regexConfig =
  let
    maybeConfig :: Maybe SecretsMatcher
    maybeConfig =
      ("{ \"matcher\": \"regex\", " ++
      "\"list\": [\"abc.*xyz\", \"[0-9]{1,3}\"] }")
        |> LBSC8.pack
        |> Aeson.decode
    Just config = maybeConfig
  in
  config


regexConfigFor :: String -> SecretsMatcher
regexConfigFor singleRegex =
  let
    maybeConfig :: Maybe SecretsMatcher
    maybeConfig =
      ("{ \"matcher\": \"regex\", " ++
      "\"list\": [\"" ++ singleRegex ++ "\"] }")
        |> LBSC8.pack
        |> Aeson.decode
    Just config = maybeConfig
  in
  config

