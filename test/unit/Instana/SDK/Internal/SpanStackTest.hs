{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.SpanStackTest (allTests) where

import           Data.Aeson                     (Value)
import qualified Data.Aeson                     as Aeson
import           Test.HUnit

import qualified Instana.SDK.Internal.Id        as Id
import           Instana.SDK.Internal.SpanStack (SpanStack)
import qualified Instana.SDK.Internal.SpanStack as SpanStack
import           Instana.SDK.Span.EntrySpan     (EntrySpan (RootEntrySpan))
import           Instana.SDK.Span.ExitSpan      (ExitSpan (ExitSpan))
import qualified Instana.SDK.Span.ExitSpan      as ExitSpan
import           Instana.SDK.Span.RootEntry     (RootEntry (RootEntry))
import qualified Instana.SDK.Span.RootEntry     as RootEntry
import           Instana.SDK.Span.Span          (Span (..), SpanKind (..))


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
    assertEqual "empty#popWhenMatches" Nothing (snd $ SpanStack.popWhenMatches EntryKind empty)


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
      SpanStack.isEmpty $ (fst $ SpanStack.popWhenMatches EntryKind empty)


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
      (snd $ SpanStack.popWhenMatches EntryKind entryOnly)


popWhenNotMatchesEntryShouldReturnNothing :: Test
popWhenNotMatchesEntryShouldReturnNothing =
  TestCase $
    assertEqual "push/popWhenNotMatches"
      (Nothing)
      (snd $ SpanStack.popWhenMatches ExitKind entryOnly)


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
      SpanStack.isEmpty $ (fst $ SpanStack.popWhenMatches EntryKind entryOnly)


popEntryWhenNotMatchesEntryShouldLeaveStackUnchanged :: Test
popEntryWhenNotMatchesEntryShouldLeaveStackUnchanged =
  let
    newStack = fst $ SpanStack.popWhenMatches ExitKind entryOnly
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
    assertEqual "push/push/popWhenMatches"
      (Just $ Exit exitSpan)
      (snd $ SpanStack.pop entryAndExit)


popWhenMatchesShouldReturnExit :: Test
popWhenMatchesShouldReturnExit =
  TestCase $
    assertEqual "push/push/popWhenMatches"
      (Just $ Exit exitSpan)
      (snd $ SpanStack.popWhenMatches ExitKind entryAndExit)


popWhenNotMatchesExitShouldReturnNothing :: Test
popWhenNotMatchesExitShouldReturnNothing =
  let
  in
  TestCase $
    assertEqual "push/push/popWhenNotMatches"
      Nothing
      (snd $ SpanStack.popWhenMatches EntryKind entryAndExit)


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
        (fst $ SpanStack.popWhenMatches ExitKind entryAndExit)


popExitWhenNotMatcheExitShouldLeaveNotEmpty :: Test
popExitWhenNotMatcheExitShouldLeaveNotEmpty =
  let
    newStack = fst $ SpanStack.popWhenMatches EntryKind entryAndExit
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


empty :: SpanStack
empty =
  SpanStack.empty


entryOnly :: SpanStack
entryOnly =
  SpanStack.push (Entry entrySpan) $ empty


entryAndExit :: SpanStack
entryAndExit =
  SpanStack.push (Exit exitSpan) $ entryOnly


entrySpan :: EntrySpan
entrySpan =
  RootEntrySpan $
    RootEntry
      { RootEntry.spanAndTraceId = Id.fromString "traceId"
      , RootEntry.spanName       = "test.entry"
      , RootEntry.timestamp      = 1514761200000
      , RootEntry.spanData       = emptyValue
      , RootEntry.errorCount     = 0
      }


exitSpan :: ExitSpan
exitSpan =
  ExitSpan
    { ExitSpan.parentSpan = entrySpan
    , ExitSpan.spanId     = Id.fromString "spanId"
    , ExitSpan.spanName   = "test.exit"
    , ExitSpan.timestamp  = 1514761201000
    , ExitSpan.spanData   = emptyValue
    , ExitSpan.errorCount = 0
    }


emptyValue :: Value
emptyValue = Aeson.object []

