{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.Command
Description : Commands that can be send to the worker
-}
module Instana.SDK.Internal.Command
  ( Command(..)
  ) where


import           Data.Aeson                 (Value)

import           Instana.SDK.Span.EntrySpan (EntrySpan)
import           Instana.SDK.Span.ExitSpan  (ExitSpan)


-- |A command that can be send to the worker.
data Command =
  -- |CompleteEntry entrySpan errorCount
  CompleteEntry EntrySpan Int
  -- |CompleteEntryWithData entrySpan errorCount spanData
  | CompleteEntryWithData EntrySpan Int Value
  -- |CompleteExit exitSpan errorCount
  | CompleteExit ExitSpan Int
  -- |CompleteExitWithData exitSpan errorCount spanData
  | CompleteExitWithData ExitSpan Int Value
  deriving (Show)

