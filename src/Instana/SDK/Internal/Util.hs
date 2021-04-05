{-|
Module      : Instana.SDK.Internal.Util
Description : Utilities
-}
module Instana.SDK.Internal.Util
  ( (|>)
  , leftPad
  , leftPadAndLimit
  , limit
  ) where


import qualified Data.Text as T


-- |Elm-style function application.
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)


-- |Prepends the character "0" to this string until it has the given length.
-- Otherwise the input is returned unmodified.
leftPad :: Int -> String -> String
leftPad digits s
  | length s < digits = replicate (digits - length s) '0' ++ s
  | otherwise         = s


-- |If the given string is shorten than the requested length, the character "0"
-- is prepended to this string. If the given string is longer than the requested
-- length, the leading characters will be discarded to limit it to the requested
-- length. Otherwise the input is returned unmodified.
leftPadAndLimit :: Int -> String -> String
leftPadAndLimit digits s =
  leftPad digits s
  |> limit digits


-- |If the given string is longer than the requested length, the leading
-- characters will be discarded to limit it to the requested length. Otherwise
-- the input is returned unmodified.
limit :: Int -> String -> String
limit digits s
  | length s > digits = T.unpack $ T.takeEnd digits $ T.pack s
  | otherwise         = s

