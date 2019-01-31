{-|
Module      : Instana.SDK.Internal.Util
Description : Utilities
-}
module Instana.SDK.Internal.Util
  ( (|>)
  ) where


-- |Elm-style function application.
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)
