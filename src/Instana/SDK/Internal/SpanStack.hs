{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Instana.SDK.Internal.SpanStack
Description : Keeps the current spans of a thread.
-}
module Instana.SDK.Internal.SpanStack
  ( SpanStack
  , empty
  , entry
  , isEmpty
  , isSuppressed
  , mapTop
  , peek
  , pop
  , popWhenMatches
  , push
  , pushSuppress
  , suppress
  ) where

import           GHC.Generics

import           Instana.SDK.Internal.Util  ((|>))
import           Instana.SDK.Span.EntrySpan (EntrySpan)
import           Instana.SDK.Span.ExitSpan  (ExitSpan)
import           Instana.SDK.Span.Span      (Span (..), SpanKind (..))

--
-- Implementation Note
-- ===================
--
-- This implementation currently heavily relies on the assumption that the
-- monitored application does not employ context switches in a thread (like
-- doing actual async IO, for example). Since Haskell's standard vehicle for
-- concurrency (Control.Concurrent#forkIO and friends) uses green threads (and
-- not OS level threads) doing multiple things in one thread at the same time is
-- not very common, in fact I haven't seen it in the wild yet.
--
-- Under this assumptions there can be at most two current spans per thread, an
-- entry and an exit. A new exit can only be started once the IO action related
-- to the last exit has completed. The same holds for any entry span. Thus, the
-- spans in one thread do not form a tree (as async contexts and their spans do
-- in Node.js for example) but only a stack with maximal depth 2, like this:
-- * no current span, currently not tracing
-- * an active entry span but not exit
-- * a non-active entry and an active exit


{-|The stack of currently open spans in one thread.
-}
data SpanStack =
    -- |Indicates that we are currently not processing any request.
    None
    -- |Indicates that we are currently processing a request that had
    -- X-INSTANA-L=0 set and that should not record any spans.
  | Suppressed
    -- |Indicates that we are currently processing an entry.
  | EntryOnly EntrySpan
    -- |Indicates that currently an exit is in progress.
  | EntryAndExit EntrySpan ExitSpan
  deriving (Eq, Generic, Show)


{-|Creates an empty span stack.
-}
empty :: SpanStack
empty =
  None


{-|Initializes a span stack with one entry span.
-}
entry :: EntrySpan -> SpanStack
entry entrySpan =
  empty
    |> push (Entry entrySpan)


{-|Creates an span stack with a suppressed marker.
-}
suppress :: SpanStack
suppress =
  Suppressed


{-|Checks if the span stack is empty.
-}
isEmpty :: SpanStack -> Bool
isEmpty t =
  t == None


{-|Checks if tracing is currently suppressed.
-}
isSuppressed :: SpanStack -> Bool
isSuppressed t =
  t == Suppressed


{-|Pushes a span onto the stack. Invalid calls are ignored (like pushing an
exit onto an empty span or an entry span onto an already existing entry span.
-}
push :: Span -> SpanStack -> SpanStack
push (Entry entrySpan) None =
  EntryOnly entrySpan
-- a new incoming entry can lift the suppression, an exit can't
push (Entry entrySpan) Suppressed =
  EntryOnly entrySpan
push (Exit exitSpan) (EntryOnly entrySpan) =
  EntryAndExit entrySpan exitSpan
-- ignore invalid calls/invalid state
push _ current =
  current


{-|Pushes a suppressed marker onto the stack. This is only valid if the span
stack is currently empty, otherwise the span stack is returned unmodified.
-}
pushSuppress :: SpanStack -> SpanStack
pushSuppress None =
  Suppressed
pushSuppress Suppressed =
  Suppressed
-- ignore invalid calls/invalid state
pushSuppress current =
  current


{-|Pops the top element, returns a tuple of the top element and the remaining
stack after poppint the top element.
-}
pop :: SpanStack -> (SpanStack, Maybe Span)
pop None =
  (None, Nothing)
pop Suppressed =
  (None, Nothing)
pop (EntryOnly entrySpan) =
  (None, Just $ Entry entrySpan)
pop (EntryAndExit entrySpan exitSpan) =
  (EntryOnly entrySpan, Just $ Exit exitSpan)


{-|Pops the top element, but only if the top element is of the expected kind.
If so, a tuple of the top element and the remaining stack after popping the top
element is returned. If not, Nothing and an unmodified stack is returned. The
last part of the 3-tuple is an error message that is only provided if there is
a mismatch between the expected span kind and the actual span kind on the top of
the stack.
-}
popWhenMatches :: SpanKind -> SpanStack -> (SpanStack, Maybe Span, Maybe String)
popWhenMatches _ None =
  (None, Nothing, Nothing)
popWhenMatches EntryKind Suppressed =
  -- This effectively unsuppresses - we started an entry that was suppressed and
  -- now we are asked to complete this very entry, so the suppression is lifted
  -- and we are back to a pristine state, ready to start the next entry when the
  -- next request comes in.
  (None, Nothing, Nothing)
popWhenMatches _ Suppressed =
  (Suppressed, Nothing, Nothing)
popWhenMatches expectedKind stack =
  case (expectedKind, peek stack) of
    (EntryKind, Just (Entry _)) ->
      (st, sp, Nothing)
      where
        (st, sp) = pop stack
    (ExitKind, Just (Exit _)) ->
      (st, sp, Nothing)
      where
        (st, sp) = pop stack
    (_, actualTopElement) ->
      ( stack
      , Nothing
      , Just $ "Cannot pop \"" ++ (show expectedKind) ++
        " from span stack. Current top element: " ++ show actualTopElement
      )


{-|Returns the top element without modifying the stack.
-}
peek :: SpanStack -> Maybe Span
peek None =
  Nothing
peek Suppressed =
  Nothing
peek (EntryOnly entrySpan) =
  Just $ Entry entrySpan
peek (EntryAndExit _ exitSpan) =
  Just $ Exit exitSpan


{-|Modifies the top element in place by applying the given function to it. This
is a no op if the span stack is empty.
-}
mapTop :: (Span -> Span) -> SpanStack -> SpanStack
mapTop _ None =
  None
mapTop _ Suppressed =
  Suppressed
mapTop fn stack =
  let
    (remainder, Just oldTop) = pop stack
    newTop = fn oldTop
  in
  push newTop remainder

