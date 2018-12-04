{-|
Module      : Instana.SDK.Internal.Util
Description : Utilities
-}
module Instana.SDK.Internal.Util
  ( (|>)
  ) where


(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)
