{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.Command
Description : Commands that can be send to the worker
-}
module Instana.SDK.Internal.Command
  ( Command(..)
  ) where


import           Instana.SDK.Span.EntrySpan (EntrySpan)
import           Instana.SDK.Span.ExitSpan  (ExitSpan)


-- |A command that can be send to the worker.
data Command =
  -- |CompleteEntry entrySpan
  CompleteEntry EntrySpan
  -- |CompleteExit exitSpan
  | CompleteExit ExitSpan
  deriving (Show)

