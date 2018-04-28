{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.Internal.Command
  ( Command(..)
  ) where


import           Data.Aeson                 (Value)

import           Instana.SDK.Span.EntrySpan (EntrySpan)
import           Instana.SDK.Span.ExitSpan  (ExitSpan)


data Command =
  -- | CompleteEntry entrySpan spanError
  CompleteEntry EntrySpan Bool
  -- | CompleteEntryWithData entrySpan spanError spanData
  | CompleteEntryWithData EntrySpan Bool Value
  -- | CompleteExit exitSpan spanError
  | CompleteExit ExitSpan Bool
  -- | CompleteExitWithData exitSpan spanError spanData
  | CompleteExitWithData ExitSpan Bool Value
  deriving (Show)

