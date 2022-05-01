{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.SpanStackTest (allTests) where

import           Test.HUnit

import qualified Instana.SDK.Internal.Id              as Id
import           Instana.SDK.Internal.SpanStack       (SpanStack)
import qualified Instana.SDK.Internal.SpanStack       as SpanStack
import           Instana.SDK.Internal.W3CTraceContext (Flags (..),
                                                       TraceParent (..),
                                                       TraceState (..),
                                                       W3CTraceContext (..))
import           Instana.SDK.Span.EntrySpan           (EntrySpan (RootEntrySpan))
import           Instana.SDK.Span.ExitSpan            (ExitSpan (ExitSpan))
import qualified Instana.SDK.Span.ExitSpan            as ExitSpan
import           Instana.SDK.Span.RootEntry           (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry           as RootEntry
import           Instana.SDK.Span.Span                (Span (..), SpanKind (..))
import qualified Instana.SDK.Span.Span                as Span
import qualified Instana.SDK.Span.SpanData            as SpanData


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldCreateEmpty" shouldCreateEmpty
    , TestLabel "popShouldReturnNothingOnEmpty" popShouldReturnNothingOnEmpty
    , TestLabel "popWhenMatchesShouldReturnNothingOnEmpty"
        popWhenMatchesShouldReturnNothingOnEmpty
    , TestLabel "peekShouldReturnNothingOnEmpty" peekShouldReturnNothingOnEmpty
    , TestLabel "popEmptyShouldLeaveEmpty" popEmptyShouldLeaveEmpty
    , TestLabel "popWhenMatchesEmptyShouldLeaveEmpty"
        popWhenMatchesEmptyShouldLeaveEmpty
    , TestLabel "shouldPushEntry" shouldPushEntry
    , TestLabel "shouldNotPushExitOnEmpty" shouldNotPushExitOnEmpty
    , TestLabel "popShouldReturnEntry" popShouldReturnEntry
    , TestLabel "popWhenMatchesShouldReturnEntry"
        popWhenMatchesShouldReturnEntry
    , TestLabel "popWhenNotMatchesEntryShouldReturnNothing"
        popWhenNotMatchesEntryShouldReturnNothing
    , TestLabel "peekShouldReturnEntry" peekShouldReturnEntry
    , TestLabel "popEntryShouldLeaveEmpty" popEntryShouldLeaveEmpty
    , TestLabel "popEntryWhenMatchesShouldLeaveEmpty"
        popEntryWhenMatchesShouldLeaveEmpty
    , TestLabel "popEntryWhenNotMatchesEntryShouldLeaveStackUnchanged"
        popEntryWhenNotMatchesEntryShouldLeaveStackUnchanged
    , TestLabel "shouldPushExit" shouldPushExit
    , TestLabel "shouldNotPushEntryOnEntry" shouldNotPushEntryOnEntry
    , TestLabel "popShouldReturnExit" popShouldReturnExit
    , TestLabel "popWhenMatchesShouldReturnExit" popWhenMatchesShouldReturnExit
    , TestLabel "popWhenNotMatchesExitShouldReturnNothing"
        popWhenNotMatchesExitShouldReturnNothing
    , TestLabel "peekShouldReturnExit" peekShouldReturnExit
    , TestLabel "popExitShouldLeaveNotEmpty" popExitShouldLeaveNotEmpty
    , TestLabel "popExitWhenMatcheShouldLeaveNotEmpty"
        popExitWhenMatcheShouldLeaveNotEmpty
    , TestLabel "popExitWhenNotMatcheExitShouldLeaveNotEmpty"
        popExitWhenNotMatcheExitShouldLeaveNotEmpty
    , TestLabel "popPopShouldReturnEntry" popPopShouldReturnEntry
    , TestLabel "popPeekShouldReturnEntry" popPeekShouldReturnEntry
    , TestLabel "popPopShouldLeaveEmpty" popPopShouldLeaveEmpty
    , TestLabel "mapTopShouldMapEmptyToEmpty" mapTopShouldMapEmptyToEmpty
    , TestLabel "mapTopShouldMapSuppressedToSuppressed"
        mapTopShouldMapSuppressedToSuppressed
    , TestLabel "mapTopShouldApplyFnToEntry" mapTopShouldApplyFnToEntry
    , TestLabel "mapTopShouldApplyFnToExit" mapTopShouldApplyFnToExit
    , TestLabel "mapEntryShouldMapEmptyToEmpty" mapEntryShouldMapEmptyToEmpty
    , TestLabel "mapEntryShouldMapSuppressedToSuppressed"
        mapEntryShouldMapSuppressedToSuppressed
    , TestLabel "mapEntryShouldApplyFnToEntry" mapEntryShouldApplyFnToEntry
    , TestLabel "mapEntryShouldApplyFnToExit" mapEntryShouldApplyFnToExit
    ]


shouldCreateEmpty :: Test
shouldCreateEmpty =
  TestCase $
    assertBool "empty" $ SpanStack.isEmpty empty


popShouldReturnNothingOnEmpty :: Test
popShouldReturnNothingOnEmpty =
  TestCase $
    assertEqual "empty#pop" Nothing (snd $ SpanStack.pop empty)


popWhenMatchesShouldReturnNothingOnEmpty :: Test
popWhenMatchesShouldReturnNothingOnEmpty =
  TestCase $
    assertEqual "empty#popWhenMatches" Nothing (snd3 $ SpanStack.popWhenMatches EntryKind empty)


peekShouldReturnNothingOnEmpty :: Test
peekShouldReturnNothingOnEmpty =
  TestCase $
    assertEqual "empty#peek" Nothing (SpanStack.peek empty)


popEmptyShouldLeaveEmpty :: Test
popEmptyShouldLeaveEmpty =
  TestCase $
    assertBool "empty#pop leaves empty" $
      SpanStack.isEmpty $ (fst $ SpanStack.pop empty)


popWhenMatchesEmptyShouldLeaveEmpty :: Test
popWhenMatchesEmptyShouldLeaveEmpty =
  TestCase $
    assertBool "empty#popWhenMatches leaves empty" $
      SpanStack.isEmpty $ (fst3 $ SpanStack.popWhenMatches EntryKind empty)


shouldPushEntry :: Test
shouldPushEntry =
  TestCase $
    assertBool "push entry" $ not $ SpanStack.isEmpty entryOnly


shouldNotPushExitOnEmpty :: Test
shouldNotPushExitOnEmpty =
  let
    stack = SpanStack.push (Exit exitSpan) $ SpanStack.empty
  in
  TestCase $
    assertBool "push exit on emtpy" $ SpanStack.isEmpty stack


popShouldReturnEntry :: Test
popShouldReturnEntry =
  TestCase $
    assertEqual "push/pop"
      (Just $ Entry entrySpan)
      (snd $ SpanStack.pop entryOnly)


popWhenMatchesShouldReturnEntry :: Test
popWhenMatchesShouldReturnEntry =
  TestCase $
    assertEqual "push/popWhenMatches"
      (Just $ Entry entrySpan)
      (snd3 $ SpanStack.popWhenMatches EntryKind entryOnly)


popWhenNotMatchesEntryShouldReturnNothing :: Test
popWhenNotMatchesEntryShouldReturnNothing =
  TestCase $
    assertEqual "push/popWhenNotMatches"
      (Nothing)
      (snd3 $ SpanStack.popWhenMatches ExitKind entryOnly)


peekShouldReturnEntry :: Test
peekShouldReturnEntry =
  TestCase $
    assertEqual "push/peek"
      (Just $ Entry entrySpan)
      (SpanStack.peek entryOnly)


popEntryShouldLeaveEmpty :: Test
popEntryShouldLeaveEmpty =
  TestCase $
    assertBool "push/pop leaves empty" $
      SpanStack.isEmpty $ (fst $ SpanStack.pop entryOnly)


popEntryWhenMatchesShouldLeaveEmpty :: Test
popEntryWhenMatchesShouldLeaveEmpty =
  TestCase $
    assertBool "push/popWhenMatches leaves empty" $
      SpanStack.isEmpty $ (fst3 $ SpanStack.popWhenMatches EntryKind entryOnly)


popEntryWhenNotMatchesEntryShouldLeaveStackUnchanged :: Test
popEntryWhenNotMatchesEntryShouldLeaveStackUnchanged =
  let
    newStack = fst3 $ SpanStack.popWhenMatches ExitKind entryOnly
    top = SpanStack.peek newStack
  in
  TestCase $
    assertEqual "push/popWhenNotMatches leaves stack unchanged"
      (Just $ Entry entrySpan)
      top


shouldPushExit :: Test
shouldPushExit =
  TestCase $
    assertBool "push exit" $ not $ SpanStack.isEmpty entryAndExit


shouldNotPushEntryOnEntry :: Test
shouldNotPushEntryOnEntry =
  let
    stack = SpanStack.push (Entry entrySpan) $ entryOnly
    top = SpanStack.peek stack
  in
  TestCase $
    assertEqual "push entry on entry"
      (Just $ Entry entrySpan)
      top


popShouldReturnExit :: Test
popShouldReturnExit =
  TestCase $
    assertEqual "push/push/pop"
      (Just $ Exit exitSpan)
      (snd $ SpanStack.pop entryAndExit)


popWhenMatchesShouldReturnExit :: Test
popWhenMatchesShouldReturnExit =
  TestCase $
    assertEqual "push/push/popWhenMatches"
      (Just $ Exit exitSpan)
      (snd3 $ SpanStack.popWhenMatches ExitKind entryAndExit)


popWhenNotMatchesExitShouldReturnNothing :: Test
popWhenNotMatchesExitShouldReturnNothing =
  let
  in
  TestCase $
    assertEqual "push/push/popWhenNotMatches"
      Nothing
      (snd3 $ SpanStack.popWhenMatches EntryKind entryAndExit)


peekShouldReturnExit :: Test
peekShouldReturnExit =
  TestCase $
    assertEqual "push/push/peek"
      (Just $ Exit exitSpan)
      (SpanStack.peek entryAndExit)


popExitShouldLeaveNotEmpty :: Test
popExitShouldLeaveNotEmpty =
  TestCase $
    assertBool "push/push/pop leaves not empty" $
      not $ SpanStack.isEmpty $ (fst $ SpanStack.pop entryAndExit)


popExitWhenMatcheShouldLeaveNotEmpty :: Test
popExitWhenMatcheShouldLeaveNotEmpty =
  TestCase $
    assertBool "push/push/popWhenMatches leaves not empty" $
      not $ SpanStack.isEmpty $
        (fst3 $ SpanStack.popWhenMatches ExitKind entryAndExit)


popExitWhenNotMatcheExitShouldLeaveNotEmpty :: Test
popExitWhenNotMatcheExitShouldLeaveNotEmpty =
  let
    newStack = fst3 $ SpanStack.popWhenMatches EntryKind entryAndExit
    top = SpanStack.peek newStack
  in
  TestCase $
    assertEqual "push/push/popWhenNotMatches leaves stack unchanged"
      (Just $ Exit exitSpan)
      top


popPopShouldReturnEntry :: Test
popPopShouldReturnEntry =
  TestCase $
    assertEqual "push/push/pop/pop"
      (Just $ Entry entrySpan)
      (snd $ SpanStack.pop $ fst $ SpanStack.pop entryAndExit)


popPeekShouldReturnEntry :: Test
popPeekShouldReturnEntry =
  TestCase $
    assertEqual "push/push/pop/peek"
      (Just $ Entry entrySpan)
      (SpanStack.peek $ fst $ SpanStack.pop entryAndExit)


popPopShouldLeaveEmpty :: Test
popPopShouldLeaveEmpty =
  TestCase $
    assertBool "push/push/pop/pop leaves empty" $
      SpanStack.isEmpty $
      fst $ SpanStack.pop $
      fst $ SpanStack.pop entryAndExit


mapTopShouldMapEmptyToEmpty :: Test
mapTopShouldMapEmptyToEmpty =
  TestCase $
    assertBool "mapTop maps empty to empty" $
      SpanStack.isEmpty $
        SpanStack.mapTop increaseEc empty


mapTopShouldMapSuppressedToSuppressed :: Test
mapTopShouldMapSuppressedToSuppressed =
  TestCase $
    assertBool "mapTop maps suppressd to suppressd" $
      SpanStack.isSuppressed $
        SpanStack.mapTop increaseEc suppressed


mapTopShouldApplyFnToEntry :: Test
mapTopShouldApplyFnToEntry =
  TestCase $
    assertEqual "mapTop should apply the function to the entry"
      (SpanStack.push
        (Entry (RootEntrySpan (rootEntry { RootEntry.errorCount = 1 })))
        empty)
      (SpanStack.mapTop increaseEc entryOnly)


mapTopShouldApplyFnToExit :: Test
mapTopShouldApplyFnToExit =
  TestCase $
    assertEqual "mapTop should apply the function to the exit"
      (SpanStack.push
        (Exit (exitSpan { ExitSpan.errorCount = 1 }))
        entryOnly
      )
      (SpanStack.mapTop increaseEc entryAndExit)


mapEntryShouldMapEmptyToEmpty :: Test
mapEntryShouldMapEmptyToEmpty =
  TestCase $
    assertBool "mapEntry maps empty to empty" $
      SpanStack.isEmpty $
        SpanStack.mapEntry increaseEc empty


mapEntryShouldMapSuppressedToSuppressed :: Test
mapEntryShouldMapSuppressedToSuppressed =
  TestCase $
    assertBool "mapEntry maps suppressd to suppressd" $
      SpanStack.isSuppressed $
        SpanStack.mapEntry increaseEc suppressed


mapEntryShouldApplyFnToEntry :: Test
mapEntryShouldApplyFnToEntry =
  TestCase $
    assertEqual "mapEntry should apply the function to the entry"
      (SpanStack.push
        (Entry (RootEntrySpan (rootEntry { RootEntry.errorCount = 1 })))
        empty)
      (SpanStack.mapEntry increaseEc entryOnly)


mapEntryShouldApplyFnToExit :: Test
mapEntryShouldApplyFnToExit =
  TestCase $
    assertEqual
      "mapEntry should apply the function to the entry when an exit is present"
      (SpanStack.push
        (Exit exitSpan)
        (SpanStack.push
          (Entry (RootEntrySpan (rootEntry { RootEntry.errorCount = 1 })))
          empty)
        )
      (SpanStack.mapEntry increaseEc entryAndExit)


empty :: SpanStack
empty =
  SpanStack.empty


suppressed :: SpanStack
suppressed =
  SpanStack.suppress dummyW3cTraceContext


entryOnly :: SpanStack
entryOnly =
  SpanStack.push (Entry entrySpan) $ empty


entryAndExit :: SpanStack
entryAndExit =
  SpanStack.push (Exit exitSpan) $ entryOnly


entrySpan :: EntrySpan
entrySpan =
  RootEntrySpan rootEntry


rootEntry :: RootEntry
rootEntry =
    RootEntry
      { RootEntry.spanAndTraceId  = Id.fromString "traceId"
      , RootEntry.spanName        = "test.entry"
      , RootEntry.timestamp       = 1514761200000
      , RootEntry.errorCount      = 0
      , RootEntry.serviceName     = Nothing
      , RootEntry.synthetic       = False
      , RootEntry.correlationType = Nothing
      , RootEntry.correlationId   = Nothing
      , RootEntry.spanData        = SpanData.empty
      , RootEntry.w3cTraceContext = Nothing
      }


exitSpan :: ExitSpan
exitSpan =
  ExitSpan
    { ExitSpan.parentSpan      = entrySpan
    , ExitSpan.spanId          = Id.fromString "spanId"
    , ExitSpan.spanName        = "test.exit"
    , ExitSpan.timestamp       = 1514761201000
    , ExitSpan.errorCount      = 0
    , ExitSpan.serviceName     = Nothing
    , ExitSpan.spanData        = SpanData.empty
    , ExitSpan.w3cTraceContext = dummyW3cTraceContext
    }


dummyW3cTraceContext :: W3CTraceContext
dummyW3cTraceContext =
  W3CTraceContext
  { traceParent = TraceParent
    { version  = 0
    , traceId  = "trace-id"
    , parentId = "span-id"
    , flags    = Flags
      { sampled = False
      }
    }
  , traceState = TraceState
    { traceStateHead      = Nothing
    , instanaKeyValuePair = Nothing
    , traceStateTail      = Nothing
    }
  }


increaseEc :: Span -> Span
increaseEc =
  Span.addToErrorCount 1


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

