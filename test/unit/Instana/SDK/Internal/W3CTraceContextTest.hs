{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.Internal.W3CTraceContextTest (allTests) where


import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Test.HUnit

import qualified Instana.SDK.Internal.Id              as Id
import           Instana.SDK.Internal.W3CTraceContext (Flags (..),
                                                       InstanaKeyValuePair (..),
                                                       TraceParent (..),
                                                       TraceState (..),
                                                       W3CTraceContext (..))
import qualified Instana.SDK.Internal.W3CTraceContext as W3CTraceContext


allTests :: Test
allTests =
  TestList
    [ TestLabel
        "shouldDecodeEmptyTraceParentHeaderToNothing"
        shouldDecodeEmptyTraceParentHeaderToNothing
    , TestLabel
        "shouldDecodeWhitespaceTraceParentHeaderToNothing"
        shouldDecodeWhitespaceTraceParentHeaderToNothing
    , TestLabel
        "shouldDecodeMalformedTraceParentHeaderToNothing"
        shouldDecodeMalformedTraceParentHeaderToNothing
    , TestLabel
        "shouldDecodeWrongComponentLengthToNothing"
        shouldDecodeWrongComponentLengthToNothing
    , TestLabel
        "shouldDecodeInvalidVersionContentToNothing"
        shouldDecodeInvalidVersionContentToNothing
    , TestLabel
        "shouldDecodeInvalidVersionContentToNothing2"
        shouldDecodeInvalidVersionContentToNothing2
    , TestLabel
        "shouldDecodeInvalidTraceIdContentToNothing"
        shouldDecodeInvalidTraceIdContentToNothing
    , TestLabel
        "shouldDecodeInvalidTraceIdContentToNothing2"
        shouldDecodeInvalidTraceIdContentToNothing2
    , TestLabel
        "shouldDecodeInvalidParentIdContentToNothing"
        shouldDecodeInvalidParentIdContentToNothing
    , TestLabel
        "shouldDecodeInvalidParentIdContentToNothing2"
        shouldDecodeInvalidParentIdContentToNothing2
    , TestLabel
        "shouldDecodeInvalidFlagsContentToNothing"
        shouldDecodeInvalidFlagsContentToNothing
    , TestLabel
        "shouldDecodeValidTraceParentWithoutRandomTraceId"
        shouldDecodeValidTraceParentWithoutRandomTraceId
    , TestLabel
        "shouldDecodeValidTraceParentWithRandomTraceId"
        shouldDecodeValidTraceParentWithRandomTraceId
    , TestLabel
        "shouldDecodeUnsampledTraceParentWithoutRandomTraceId"
        shouldDecodeUnsampledTraceParentWithoutRandomTraceId
    , TestLabel
        "shouldDecodeUnsampledTraceParentWithRandomTraceId"
        shouldDecodeUnsampledTraceParentWithRandomTraceId
    , TestLabel
        "shouldDecodeKnownPartsFromHigherVersionTraceParent"
        shouldDecodeKnownPartsFromHigherVersionTraceParent
    , TestLabel
        "shouldDecodeAbsentTraceState"
        shouldDecodeAbsentTraceState
    , TestLabel
        "shouldDecodeEmptyTraceState"
        shouldDecodeEmptyTraceState
    , TestLabel
        "shouldDecodeWhitespaceTraceState"
        shouldDecodeWhitespaceTraceState
    , TestLabel
        "shouldDecodeTraceStateWithoutInstanaKeyValuePair"
        shouldDecodeTraceStateWithoutInstanaKeyValuePair
    , TestLabel
        "shouldDecodeTraceStateWithInstanaKeyValuePairAtBeginning"
        shouldDecodeTraceStateWithInstanaKeyValuePairAtBeginning
    , TestLabel
        "shouldDecodeTraceStateWithInstanaKeyValuePairInTheMiddle"
        shouldDecodeTraceStateWithInstanaKeyValuePairInTheMiddle
    , TestLabel
        "shouldDecodeTraceStateWithInstanaKeyValuePairAtTheEnd"
        shouldDecodeTraceStateWithInstanaKeyValuePairAtTheEnd
    , TestLabel
        "shouldDecodeTraceStateWithWhitespace"
        shouldDecodeTraceStateWithWhitespace
    , TestLabel
        "shouldDiscardExcessTraceStateListMembersWithoutInstanaKeyValuePair"
        shouldDiscardExcessTraceStateListMembersWithoutInstanaKeyValuePair
    , TestLabel
        "shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePair"
        shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePair
    , TestLabel
        "shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt32"
        shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt32
    , TestLabel
        "shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt33"
        shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt33
    , TestLabel
        "shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt66"
        shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt66
    , TestLabel
        "shouldDiscardMalformedInstanaKeyValuePair"
        shouldDiscardMalformedInstanaKeyValuePair
    , TestLabel
        "shouldInherit"
        shouldInherit
    , TestLabel
        "shouldInheritUnsetRandomTraceIdFlag"
        shouldInheritUnsetRandomTraceIdFlag
    , TestLabel
        "shouldInheritForSuppressed"
        shouldInheritForSuppressed
    , TestLabel
        "shouldCreateExitSpanContextFromIds"
        shouldCreateExitSpanContextFromIds
    , TestLabel
        "shouldCreateExitForSuppressed"
        shouldCreateExitForSuppressed
    , TestLabel
        "shouldEncodeToHeadersSampledWithRandomTraceId"
        shouldEncodeToHeadersSampledWithRandomTraceId
    , TestLabel
        "shouldEncodeToHeadersUnsampledWithRandomTraceId"
        shouldEncodeToHeadersUnsampledWithRandomTraceId
    , TestLabel
        "shouldEncodeToHeadersSampledWithoutRandomTraceId"
        shouldEncodeToHeadersSampledWithoutRandomTraceId
    , TestLabel
        "shouldEncodeToHeadersUnsampledWithoutRandomTraceId"
        shouldEncodeToHeadersUnsampledWithoutRandomTraceId
    , TestLabel
        "shouldPadShortIds"
        shouldPadShortIds
    , TestLabel
        "shouldLimitLongIds"
        shouldLimitLongIds
    , TestLabel
        "shouldDecodeInheritAndEncode"
        shouldDecodeInheritAndEncode
    ]


shouldDecodeEmptyTraceParentHeaderToNothing :: Test
shouldDecodeEmptyTraceParentHeaderToNothing = testInvalidTraceParent ""


shouldDecodeWhitespaceTraceParentHeaderToNothing :: Test
shouldDecodeWhitespaceTraceParentHeaderToNothing =
  testInvalidTraceParent "  \t \r\n "


shouldDecodeMalformedTraceParentHeaderToNothing :: Test
shouldDecodeMalformedTraceParentHeaderToNothing = testInvalidTraceParent "foo"


shouldDecodeWrongComponentLengthToNothing :: Test
shouldDecodeWrongComponentLengthToNothing =
  testInvalidTraceParent "beep-bop-beep-bop"


shouldDecodeInvalidVersionContentToNothing :: Test
shouldDecodeInvalidVersionContentToNothing =
  testInvalidTraceParent
    "xx-1234567890abcdeffedcba0987654321-24680bdf13579abc-01"


shouldDecodeInvalidVersionContentToNothing2 :: Test
shouldDecodeInvalidVersionContentToNothing2 =
  testInvalidTraceParent
    "ff-1234567890abcdeffedcba0987654321-24680bdf13579abc-01"


shouldDecodeInvalidTraceIdContentToNothing :: Test
shouldDecodeInvalidTraceIdContentToNothing =
  testInvalidTraceParent
    "00-ABCD567890abcdeffedcba0987654321-24680bdf13579abc-01"


shouldDecodeInvalidTraceIdContentToNothing2 :: Test
shouldDecodeInvalidTraceIdContentToNothing2 =
  testInvalidTraceParent
    "00-00000000000000000000000000000000-24680bdf13579abc-01"


shouldDecodeInvalidParentIdContentToNothing :: Test
shouldDecodeInvalidParentIdContentToNothing =
  testInvalidTraceParent
    "00-1234567890abcdeffedcba0987654321-hijklmnopq579abc-01"


shouldDecodeInvalidParentIdContentToNothing2 :: Test
shouldDecodeInvalidParentIdContentToNothing2 =
  testInvalidTraceParent
    "00-1234567890abcdeffedcba0987654321-0000000000000000-01"


shouldDecodeInvalidFlagsContentToNothing :: Test
shouldDecodeInvalidFlagsContentToNothing =
  testInvalidTraceParent
    "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-XX"


shouldDecodeValidTraceParentWithoutRandomTraceId :: Test
shouldDecodeValidTraceParentWithoutRandomTraceId =
  let
    actual =
      W3CTraceContext.decode
        "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-01"
        Nothing
    expected =
      sampledNoRandomTraceIdWithoutTraceState
        "1234567890abcdeffedcba0987654321"
        "24680bdf13579abc"
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldDecodeValidTraceParentWithRandomTraceId :: Test
shouldDecodeValidTraceParentWithRandomTraceId =
  let
    actual =
      W3CTraceContext.decode
        "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-03"
        Nothing
    expected =
      sampledWithRandomTraceIdWithoutTraceState
        "1234567890abcdeffedcba0987654321"
        "24680bdf13579abc"
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldDecodeUnsampledTraceParentWithoutRandomTraceId :: Test
shouldDecodeUnsampledTraceParentWithoutRandomTraceId =
  let
    actual =
      W3CTraceContext.decode
        "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-00"
        Nothing
    expected =
      unsampledNoRandomTraceIdWithoutTraceState
        "1234567890abcdeffedcba0987654321"
        "24680bdf13579abc"
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldDecodeUnsampledTraceParentWithRandomTraceId :: Test
shouldDecodeUnsampledTraceParentWithRandomTraceId =
  let
    actual =
      W3CTraceContext.decode
        "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-02"
        Nothing
    expected =
      unsampledWithRandomTraceIdWithoutTraceState
        "1234567890abcdeffedcba0987654321"
        "24680bdf13579abc"
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldDecodeKnownPartsFromHigherVersionTraceParent :: Test
shouldDecodeKnownPartsFromHigherVersionTraceParent =
  let
    actual =
      W3CTraceContext.decode
        "01-1234567890abcdeffedcba0987654321-24680bdf13579abc-01-beep-boop"
        Nothing
    expected =
      sampledNoRandomTraceIdWithoutTraceState
        "1234567890abcdeffedcba0987654321"
        "24680bdf13579abc"
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


testInvalidTraceParent :: String -> Test
testInvalidTraceParent tpContext =
  let
    w3cTraceContext = W3CTraceContext.decode tpContext Nothing
  in
  TestCase $ do
    assertEqual "W3C Trace Context"
      Nothing
      w3cTraceContext


shouldDecodeAbsentTraceState :: Test
shouldDecodeAbsentTraceState =
  let
    actual = parseAndExtractTraceState Nothing
    expected = traceState $
      withTraceState defaultTraceParent Nothing Nothing Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeEmptyTraceState :: Test
shouldDecodeEmptyTraceState =
  let
    actual = parseAndExtractTraceState (Just "")
    expected = traceState $
      withTraceState defaultTraceParent Nothing Nothing Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeWhitespaceTraceState :: Test
shouldDecodeWhitespaceTraceState =
  let
    actual = parseAndExtractTraceState (Just "  \t  \r\n  ")
    expected = traceState $
      withTraceState defaultTraceParent Nothing Nothing Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeTraceStateWithoutInstanaKeyValuePair :: Test
shouldDecodeTraceStateWithoutInstanaKeyValuePair =
  let
    actual =
      parseAndExtractTraceState $ Just
        "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7"
    expected = traceState $ withTraceState
      defaultTraceParent
      (Just $ T.pack "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7")
      Nothing
      Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeTraceStateWithInstanaKeyValuePairAtBeginning :: Test
shouldDecodeTraceStateWithInstanaKeyValuePairAtBeginning =
  let
    actual =
      parseAndExtractTraceState $ Just
        "in=fa2375d711a4ca0f;02468acefdb97531,congo=ucfJifl5GOE,rojo=00f067aa0ba902b7"
    expected =
      traceState $
        withTraceState
          defaultTraceParent
          Nothing
          (Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
          )
          (Just $ T.pack "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7")
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeTraceStateWithInstanaKeyValuePairInTheMiddle :: Test
shouldDecodeTraceStateWithInstanaKeyValuePairInTheMiddle =
  let
    actual =
      parseAndExtractTraceState $ Just
        "congo=ucfJifl5GOE,in=fa2375d711a4ca0f;02468acefdb97531,rojo=00f067aa0ba902b7"
    expected =
      traceState $
        withTraceState
          defaultTraceParent
          (Just $ T.pack "congo=ucfJifl5GOE")
          (Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
          )
          (Just $ T.pack "rojo=00f067aa0ba902b7")
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeTraceStateWithInstanaKeyValuePairAtTheEnd :: Test
shouldDecodeTraceStateWithInstanaKeyValuePairAtTheEnd =
  let
    actual =
      parseAndExtractTraceState $ Just
        "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7,in=fa2375d711a4ca0f;02468acefdb97531"
    expected =
      traceState $
        withTraceState
          defaultTraceParent
          (Just $ T.pack "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7")
          (Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
          )
          Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDecodeTraceStateWithWhitespace :: Test
shouldDecodeTraceStateWithWhitespace =
  let
    actual =
      parseAndExtractTraceState $ Just
        "  congo = ucfJifl5GOE ,  in   =   fa2375d711a4ca0f  ; 02468acefdb97531   ,  rojo  =  00f067aa0ba902b7  "
    expected =
      traceState $
        withTraceState
          defaultTraceParent
          (Just $ T.pack "congo = ucfJifl5GOE")
          (Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
          )
          (Just $ T.pack "rojo  =  00f067aa0ba902b7")
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDiscardExcessTraceStateListMembersWithoutInstanaKeyValuePair :: Test
shouldDiscardExcessTraceStateListMembersWithoutInstanaKeyValuePair =
  let
    actual =
      parseAndExtractTraceState $ Just $ T.unpack $ longTraceStateList 1 34
    expected = traceState $
      withTraceState
        defaultTraceParent
        ( Just $ longTraceStateList 1 32 )
        Nothing
        Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePair :: Test
shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePair =
  let
    actual =
      parseAndExtractTraceState $ Just $
        (T.unpack $ longTraceStateList 1 10) ++
        ",in=fa2375d711a4ca0f;02468acefdb97531," ++
        (T.unpack $ longTraceStateList 12 34)
    expected = traceState $
      withTraceState
        defaultTraceParent
        ( Just $ longTraceStateList 1 10 )
        ( Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
        )
        ( Just $ longTraceStateList 12 32 )
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt32 :: Test
shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt32 =
  let
    actual =
      parseAndExtractTraceState $ Just $
        (T.unpack $ longTraceStateList 1 31) ++
        ",in=fa2375d711a4ca0f;02468acefdb97531," ++
        (T.unpack $ longTraceStateList 33 34)
    expected = traceState $
      withTraceState
        defaultTraceParent
        ( Just $ longTraceStateList 1 31 )
        ( Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
        )
        Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt33 :: Test
shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt33 =
  let
    actual =
      parseAndExtractTraceState $ Just $
        (T.unpack $ longTraceStateList 1 32) ++
        ",in=fa2375d711a4ca0f;02468acefdb97531," ++
        (T.unpack $ longTraceStateList 34 35)
    expected = traceState $
      withTraceState
        defaultTraceParent
        ( Just $ longTraceStateList 1 31 )
        ( Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
        )
        Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt66 :: Test
shouldDiscardExcessTraceStateListMembersWithInstanaKeyValuePairAt66 =
  let
    actual =
      parseAndExtractTraceState $ Just $
        (T.unpack $ longTraceStateList 1 65) ++
        ",in=fa2375d711a4ca0f;02468acefdb97531," ++
        (T.unpack $ longTraceStateList 67 99)
    expected = traceState $
      withTraceState
        defaultTraceParent
        ( Just $ longTraceStateList 1 31 )
        ( Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
        )
        Nothing
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


longTraceStateList :: Int -> Int -> Text
longTraceStateList from to =
  T.intercalate
    ","
    [ T.pack $
        "member" ++ show i ++ "=" ++ show i | i <- [from..to] ]


shouldDiscardMalformedInstanaKeyValuePair :: Test
shouldDiscardMalformedInstanaKeyValuePair =
  let
    actual =
      parseAndExtractTraceState $ Just
        "congo=ucfJifl5GOE,in=malformed,rojo=00f067aa0ba902b7"
    expected =
      traceState $
        withTraceState
          defaultTraceParent
          (Just $ T.pack "congo=ucfJifl5GOE")
          Nothing
          (Just $ T.pack "rojo=00f067aa0ba902b7")
  in
  TestCase $ do
    assertEqual "Trace State" expected actual


shouldInherit :: Test
shouldInherit =
  let
    initialW3cCtx =
      withTraceState
        defaultTraceParent
        (Just $ T.pack "congo=ucfJifl5GOE")
        (Just $ InstanaKeyValuePair
           { instanaTraceId = "fa2375d711a4ca0f"
           , instanaParentId = "02468acefdb97531"
           }
        )
        (Just $ T.pack "rojo=00f067aa0ba902b7")

    actual =
      W3CTraceContext.inheritFrom
        initialW3cCtx
        "5a6b7c8d9ef13402"
        "234123567890abcd"

    expected =
      W3CTraceContext
        { traceParent = TraceParent
          { version  = 0
          , traceId  = "1234567890abcdeffedcba0987654321"
          , parentId = "234123567890abcd"
          , flags    = Flags
            { sampled = True
            , randomTraceId = True
            }
          }
        , traceState = TraceState
         { traceStateHead = Nothing
          , instanaKeyValuePair = Just InstanaKeyValuePair
            { instanaTraceId = "5a6b7c8d9ef13402"
            , instanaParentId = "234123567890abcd"
            }
          , traceStateTail = Just "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7"
          }
        }
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldInheritUnsetRandomTraceIdFlag :: Test
shouldInheritUnsetRandomTraceIdFlag =
  let
    initialW3cCtx = withoutRandomTraceId defaultW3cCtx

    actualW3CTraceContext =
      W3CTraceContext.inheritFrom
        initialW3cCtx
        "5a6b7c8d9ef13402"
        "234123567890abcd"
    actualTraceParent = W3CTraceContext.traceParent actualW3CTraceContext

    expectedTraceParent =
       TraceParent
       { version  = 0
       , traceId  = "1234567890abcdeffedcba0987654321"
       , parentId = "234123567890abcd"
       , flags    = Flags
         { sampled = True
         , randomTraceId = False
         }
       }
  in
  TestCase $ do
    assertEqual "traceparent" expectedTraceParent actualTraceParent


shouldInheritForSuppressed :: Test
shouldInheritForSuppressed =
  let
    initialW3cCtx =
      withTraceState
        defaultTraceParent
        (Just $ T.pack "congo=ucfJifl5GOE")
        (Just $ InstanaKeyValuePair
           { instanaTraceId = "fa2375d711a4ca0f"
           , instanaParentId = "02468acefdb97531"
           }
        )
        (Just $ T.pack "rojo=00f067aa0ba902b7")

    actual =
      W3CTraceContext.inheritFromForSuppressed
        initialW3cCtx
        "234123567890abcd"

    expected =
      W3CTraceContext
        { traceParent = TraceParent
          { version  = 0
          , traceId  = "1234567890abcdeffedcba0987654321"
          , parentId = "234123567890abcd"
          , flags    = Flags
            { sampled = False
            , randomTraceId = True
            }
          }
        , traceState = TraceState
          { traceStateHead = Just "congo=ucfJifl5GOE"
          , instanaKeyValuePair = Nothing
          , traceStateTail = Just "rojo=00f067aa0ba902b7"
          }
        }
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldCreateExitSpanContextFromIds :: Test
shouldCreateExitSpanContextFromIds =
  let
    actual =
      W3CTraceContext.exitSpanContextFromIds
        "274193a6f8c03721"
        "345fcb9140c8a6b9"

    expected =
      W3CTraceContext
        { traceParent = TraceParent
          { version  = 0
          , traceId  = "274193a6f8c03721"
          , parentId = "345fcb9140c8a6b9"
          , flags    = Flags
            { sampled = True
            , randomTraceId = True
            }
          }
        , traceState = TraceState
          { traceStateHead = Nothing
          , instanaKeyValuePair = Just InstanaKeyValuePair
            { instanaTraceId  = "274193a6f8c03721"
            , instanaParentId = "345fcb9140c8a6b9"
            }
          , traceStateTail = Nothing
          }
        }
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldCreateExitForSuppressed :: Test
shouldCreateExitForSuppressed =
  let
    actual =
      W3CTraceContext.createExitContextForSuppressed
        "274193a6f8c03721"
        "345fcb9140c8a6b9"

    expected =
      W3CTraceContext
        { traceParent = TraceParent
          { version  = 0
          , traceId  = "274193a6f8c03721"
          , parentId = "345fcb9140c8a6b9"
          , flags    = Flags
            { sampled = False
            , randomTraceId = True
            }
          }
        , traceState = TraceState
          { traceStateHead = Nothing
          , instanaKeyValuePair = Nothing
          , traceStateTail = Nothing
          }
        }
  in
  TestCase $ do
    assertEqual "W3C Trace Context" expected actual


shouldEncodeToHeadersSampledWithRandomTraceId :: Test
shouldEncodeToHeadersSampledWithRandomTraceId =
  let
    actual =
      W3CTraceContext.toHeaders $
        withTraceState
          defaultTraceParent
          (Just $ T.pack "congo=ucfJifl5GOE")
          (Just $ InstanaKeyValuePair
             { instanaTraceId = "fa2375d711a4ca0f"
             , instanaParentId = "02468acefdb97531"
             }
          )
          (Just $ T.pack "rojo=00f067aa0ba902b7")

    expected =
      [ ( "traceparent"
        , "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-03"
        )
      , ( "tracestate"
        , "in=fa2375d711a4ca0f;02468acefdb97531,congo=ucfJifl5GOE,rojo=00f067aa0ba902b7"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


shouldEncodeToHeadersUnsampledWithRandomTraceId :: Test
shouldEncodeToHeadersUnsampledWithRandomTraceId =
  let
    actual = W3CTraceContext.toHeaders $ unsampled defaultW3cCtx
    expected =
      [ ( "traceparent"
        , "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-02"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


shouldEncodeToHeadersSampledWithoutRandomTraceId :: Test
shouldEncodeToHeadersSampledWithoutRandomTraceId =
  let
    actual = W3CTraceContext.toHeaders $ withoutRandomTraceId defaultW3cCtx
    expected =
      [ ( "traceparent"
        , "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-01"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


shouldEncodeToHeadersUnsampledWithoutRandomTraceId :: Test
shouldEncodeToHeadersUnsampledWithoutRandomTraceId =
  let
    actual = W3CTraceContext.toHeaders $
      unsampled $
        withoutRandomTraceId defaultW3cCtx
    expected =
      [ ( "traceparent"
        , "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-00"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


shouldPadShortIds :: Test
shouldPadShortIds =
  let
    Just w3cCtx =
      (sampledNoRandomTraceIdWithoutTraceState
        "1234"
        "5678")
    actual =
      W3CTraceContext.toHeaders $
        withTraceState
          (traceParent w3cCtx)
          (Just $ T.pack "congo=ucfJifl5GOE")
          (Just $ InstanaKeyValuePair
             { instanaTraceId = "9abc"
             , instanaParentId = "defg"
             }
          )
          (Just $ T.pack "rojo=00f067aa0ba902b7")

    expected =
      [ ( "traceparent"
        , "00-00000000000000000000000000001234-0000000000005678-01"
        )
      , ( "tracestate"
        , "in=0000000000009abc;000000000000defg,congo=ucfJifl5GOE,rojo=00f067aa0ba902b7"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


shouldLimitLongIds :: Test
shouldLimitLongIds =
  let
    Just w3cCtx =
      (sampledNoRandomTraceIdWithoutTraceState
        "fedcba9876543210abcdef0123456789fffff"
        "112233445566778899")
    actual =
      W3CTraceContext.toHeaders w3cCtx

    expected =
      [ ( "traceparent"
        , "00-a9876543210abcdef0123456789fffff-2233445566778899-01"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


shouldDecodeInheritAndEncode :: Test
shouldDecodeInheritAndEncode =
  let
    Just decoded =
      W3CTraceContext.decode
        "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-01"
        (Just "congo=ucfJifl5GOE,in=fa2375d711a4ca0f;02468acefdb97531,rojo=00f067aa0ba902b7")

    childCtx =
      W3CTraceContext.inheritFrom
        decoded
        "5a6b7c8d9ef13402"
        "234123567890abcd"

    actual =
      W3CTraceContext.toHeaders childCtx

    expected =
      [ ( "traceparent"
        , "00-1234567890abcdeffedcba0987654321-234123567890abcd-01"
        )
      , ( "tracestate"
        , "in=5a6b7c8d9ef13402;234123567890abcd,congo=ucfJifl5GOE,rojo=00f067aa0ba902b7"
        )
      ]
  in
  TestCase $ do
    assertEqual "W3C Trace Context Headers" expected actual


sampledWithRandomTraceIdWithoutTraceState :: String -> String -> Maybe W3CTraceContext
sampledWithRandomTraceIdWithoutTraceState tId pId =
  Just $
    W3CTraceContext
      { traceParent = TraceParent
        { version  = 0
        , traceId  = Id.fromString tId
        , parentId = Id.fromString pId
        , flags    = Flags
          { sampled = True
          , randomTraceId = True
          }
        }
      , traceState = TraceState
        { traceStateHead = Nothing
        , instanaKeyValuePair = Nothing
        , traceStateTail = Nothing
        }
      }


sampledNoRandomTraceIdWithoutTraceState :: String -> String -> Maybe W3CTraceContext
sampledNoRandomTraceIdWithoutTraceState tId pId =
  withoutRandomTraceIdM $ sampledWithRandomTraceIdWithoutTraceState tId pId


unsampledWithRandomTraceIdWithoutTraceState :: String -> String -> Maybe W3CTraceContext
unsampledWithRandomTraceIdWithoutTraceState tId pId =
  unsampledM $ sampledWithRandomTraceIdWithoutTraceState tId pId


unsampledNoRandomTraceIdWithoutTraceState :: String -> String -> Maybe W3CTraceContext
unsampledNoRandomTraceIdWithoutTraceState tId pId =
  unsampledM $ sampledNoRandomTraceIdWithoutTraceState tId pId


unsampledM :: Maybe W3CTraceContext -> Maybe W3CTraceContext
unsampledM mW3cCtx =
  let
    Just w3cCtx = mW3cCtx
  in
  Just $ unsampled w3cCtx


unsampled :: W3CTraceContext -> W3CTraceContext
unsampled w3cCtx =
  let
    tp = traceParent w3cCtx
    fl = flags tp
    unsampledTp = tp { flags = fl { sampled = False } }
  in
  w3cCtx { traceParent = unsampledTp }


withoutRandomTraceIdM :: Maybe W3CTraceContext -> Maybe W3CTraceContext
withoutRandomTraceIdM mW3cCtx =
  let
    Just w3cCtx = mW3cCtx
  in
  Just $ withoutRandomTraceId w3cCtx


withoutRandomTraceId :: W3CTraceContext -> W3CTraceContext
withoutRandomTraceId w3cCtx =
  let
    tp = traceParent w3cCtx
    fl = flags tp
    noRandomTraceIdTp = tp { flags = fl { randomTraceId = False } }
  in
  w3cCtx { traceParent = noRandomTraceIdTp }


parseAndExtractTraceState :: Maybe String -> TraceState
parseAndExtractTraceState traceStateHeader =
  let
    Just w3cCtx =
        W3CTraceContext.decode
          "00-1234567890abcdeffedcba0987654321-24680bdf13579abc-01"
          traceStateHeader
  in
  traceState $ w3cCtx


withTraceState ::
  TraceParent
  -> Maybe Text
  -> Maybe InstanaKeyValuePair
  -> Maybe Text
  -> W3CTraceContext
withTraceState tp tsHead inKvPair tsTail =
  W3CTraceContext
    { traceParent = tp
    , traceState = TraceState
      { traceStateHead = tsHead
      , instanaKeyValuePair = inKvPair
      , traceStateTail = tsTail
      }
    }


defaultTraceParent :: TraceParent
defaultTraceParent =
  TraceParent
    { version  = 0
    , traceId  = "1234567890abcdeffedcba0987654321"
    , parentId = "24680bdf13579abc"
    , flags    = Flags
      { sampled = True
      , randomTraceId = True
      }
    }


defaultW3cCtx :: W3CTraceContext
defaultW3cCtx =
  W3CTraceContext
    { traceParent = defaultTraceParent
    , traceState = W3CTraceContext.emptyTraceState
    }

