{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Instana.SDK.Span.W3CTraceContext
Description : Utility module for decoding and encoding the W3C Trace Context
              headers.
-}
module Instana.SDK.Internal.W3CTraceContext
    ( Flags(..)
    , InstanaKeyValuePair(..)
    , TraceParent(..)
    , TraceState(..)
    , W3CTraceContext(..)
    , createExitContextForSuppressed
    , decode
    , exitSpanContextFromIds
    , inheritFrom
    , inheritFromForSuppressed
    , initBogusContextForSuppressedRequest
    , toHeaders
    ) where


import qualified Data.Bits                  as Bits
import qualified Data.ByteString.Char8      as BSC8
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import qualified Network.HTTP.Types         as HTTPTypes
import           Numeric                    (readHex)

import           Instana.SDK.Internal.Id    (Id)
import qualified Instana.SDK.Internal.Id    as Id
import           Instana.SDK.Internal.Util  (leftPad, leftPadAndLimit)
import qualified Instana.SDK.TracingHeaders as TracingHeaders


-- |A representation of the W3C trace context headers traceparent and
-- tracestate.
data W3CTraceContext = W3CTraceContext
  { traceParent :: TraceParent
  , traceState  :: TraceState
  } deriving (Eq, Generic, Show)


-- |A representation of the W3C trace context header traceparent.
data TraceParent = TraceParent
  { version  :: Int
  , traceId  :: Id
  , parentId :: Id
  , flags    :: Flags
  } deriving (Eq, Generic, Show)


-- |A representation of the flags part of the W3C trace context header
-- traceparent.
data Flags = Flags
  { sampled  :: Bool
  } deriving (Eq, Generic, Show)


-- |A representation of the W3C trace context header tracestate.
data TraceState = TraceState
  { traceStateHead      :: Maybe Text
  , instanaKeyValuePair :: Maybe InstanaKeyValuePair
  , traceStateTail      :: Maybe Text
  } deriving (Eq, Generic, Show)


-- |A representation of the Instana key-value pair W3C contained in the
-- tracestate header.
data InstanaKeyValuePair = InstanaKeyValuePair
  { instanaTraceId  :: Id
  , instanaParentId :: Id
  } deriving (Eq, Generic, Show)


-- |Decodes the raw values of traceparent and tracestate to the parsed
-- representation of the W3C trace context. If the traceparent value is invalid,
-- Nothing will be returned.
decode :: String -> Maybe String -> Maybe W3CTraceContext
decode traceparentHeader tracestateHeader =
  let
    maybeTraceParent = decodeTraceParent traceparentHeader
  in
  case maybeTraceParent of
    Just tp ->
      Just $
        W3CTraceContext
        { traceParent = tp
        , traceState = decodeTraceState tracestateHeader
        }
    Nothing ->
      Nothing


-- |Decodes the raw traceparent value. If the traceparent value is invalid,
-- Nothing will be returned.
decodeTraceParent :: String -> Maybe TraceParent
decodeTraceParent traceParentString =
  let
    traceParentText = T.pack traceParentString
    components = T.splitOn "-" traceParentText
  in
  if length components < 4
    then
      Nothing
    else
      decodeTraceParentComponents components


-- |Decodes the individual traceparent fields. If any of them is invalid,
-- Nothing will be returned.
decodeTraceParentComponents :: [Text] -> Maybe TraceParent
decodeTraceParentComponents components =
  let
    rawVersion = components !! 0
    rawTraceId = components !! 1
    rawParentId = components !! 2
    rawFlags = components !! 3
  in
  case ( validVersion rawVersion
       , validTraceId rawTraceId
       , validParentId rawParentId
       , validFlags rawFlags) of
    (True, True, True, True) ->
      let
        tId = Id.fromText $ rawTraceId
        pId = Id.fromText $ rawParentId
        flagsReadResult = readHex $ T.unpack $ rawFlags
        flgs :: Maybe Integer
        flgs = Maybe.listToMaybe . map fst $ flagsReadResult
        smpld :: Bool
        smpld = case flgs of
          Just fl ->
            Bits.testBit fl 0
          Nothing ->
            False
      in
      Just $ TraceParent
        { version  = 0
        , traceId  = tId
        , parentId = pId
        , flags    = Flags
          { sampled = smpld
          }
        }
    _ ->
      Nothing


-- |Checks if the version field of traceparent is valid.
validVersion :: Text -> Bool
validVersion rawVersion =
  T.length rawVersion == 2 &&
  onlyLowerCaseHex rawVersion &&
  not (T.all (\c -> c == 'f') rawVersion)


-- |Checks if the trace ID field of traceparent is valid.
validTraceId :: Text -> Bool
validTraceId rawTraceId =
  T.length rawTraceId == 32 &&
  onlyLowerCaseHex rawTraceId &&
  not (T.all (\c -> c == '0') rawTraceId)


-- |Checks if the parent ID field of traceparent is valid.
validParentId :: Text -> Bool
validParentId rawParentId =
  T.length rawParentId == 16 &&
  onlyLowerCaseHex rawParentId &&
  not (T.all (\c -> c == '0') rawParentId)


-- |Checks if the flags field of traceparent is valid.
validFlags :: Text -> Bool
validFlags rawFlags =
  T.length rawFlags == 2 &&
  onlyLowerCaseHex rawFlags


-- |Checks if the given text contains only lower case hex strings (0-9, a-f.
onlyLowerCaseHex :: Text -> Bool
onlyLowerCaseHex t =
  T.all (\c -> c `elem` (['0'..'9'] ++ ['a'..'f'])) t


-- |Decodes the raw tracestate value.
decodeTraceState :: Maybe String -> TraceState
decodeTraceState maybeTraceStateString =
  case maybeTraceStateString of

    Just traceStateString ->
      decodeTraceState' traceStateString

    Nothing ->
      emptyTraceState


-- |Decodes the raw tracestate value.
decodeTraceState' :: String -> TraceState
decodeTraceState' traceStateString =
  let
    traceStateText = T.strip $ T.pack traceStateString
  in
  if T.length traceStateText == 0
    then
      emptyTraceState
    else
      decodeNonEmptyTraceState traceStateText


-- |Decodes the raw tracestate value.
decodeNonEmptyTraceState :: Text -> TraceState
decodeNonEmptyTraceState traceStateText =
  let
    keyValuePairs = map T.strip $ T.splitOn "," traceStateText
    inKvPairIndex =
      List.findIndex (\kvPairString ->
        let
          key = T.strip $ fst $ T.breakOn "=" kvPairString
        in
        key == "in"
      ) keyValuePairs

    (tsHead, inKvPair, tsTail) =
      case inKvPairIndex of
        Just idx ->
          let
            preIdx = take idx keyValuePairs
            tsHd =
              if null preIdx
                then Nothing
                else Just $ T.intercalate "," preIdx
            tsTl =
              if null postIdx
                then Nothing
                else Just $ T.intercalate "," postIdx
            postIdx = drop (idx + 1) keyValuePairs
          in
          ( tsHd
          , decodeInKeyValuePair $ keyValuePairs !! idx
          , tsTl
          )
        Nothing ->
          ( Just $ T.intercalate "," keyValuePairs
          , Nothing
          , Nothing
          )

  in
  TraceState
  { traceStateHead      = tsHead
  , instanaKeyValuePair = inKvPair
  , traceStateTail      = tsTail
  }


-- |Decodes the Instana key value pair from the raw tracestate value.
decodeInKeyValuePair :: Text -> Maybe InstanaKeyValuePair
decodeInKeyValuePair inKvPairText =
  let
    value = T.strip $ T.drop 1 $ snd $ T.breakOn "=" inKvPairText
    (tIdRaw, pIdRaw) = T.breakOn ";" value
    (tId, pId) =
      ( T.strip tIdRaw
      , T.strip $ T.drop 1 $ pIdRaw
      )
  in
  if (T.length tId >= 16 && T.length pId >= 16) then
    Just InstanaKeyValuePair
      { instanaTraceId  = Id.fromText $ tId
      , instanaParentId = Id.fromText $ pId
      }
  else
    Nothing


-- |Creates an empty trace state value.
emptyTraceState :: TraceState
emptyTraceState =
  TraceState
  { traceStateHead      = Nothing
  , instanaKeyValuePair = Nothing
  , traceStateTail      = Nothing
  }


-- |Tests whether the given trace state represents an empty trace state.
isEmpty :: TraceState -> Bool
isEmpty ts =
  ts == TraceState
        { traceStateHead      = Nothing
        , instanaKeyValuePair = Nothing
        , traceStateTail      = Nothing
        }


-- |Creates a new W3C trace context value for an outgoing HTTP request by
-- inheriting from the given parent context.
inheritFrom :: W3CTraceContext -> Id -> Id -> W3CTraceContext
inheritFrom parentW3cTraceContext exitSpanTraceId exitSpanSpanId =
  let
    parentTp = traceParent $ parentW3cTraceContext
    parentTs :: TraceState
    parentTs = traceState $ parentW3cTraceContext
    (parentTsHead, parentTsTail) =
      ( traceStateHead parentTs
      , traceStateTail parentTs
      )
  in
  W3CTraceContext
  { traceParent = TraceParent
    { version  = 0
    , traceId  = traceId $ parentTp
    , parentId = exitSpanSpanId
    , flags    = Flags
      { sampled = True
      }
    }
  , traceState = TraceState
    { traceStateHead      = Nothing
    , instanaKeyValuePair = Just InstanaKeyValuePair
      { instanaTraceId  = exitSpanTraceId
      , instanaParentId = exitSpanSpanId
      }
    , traceStateTail =
        case (parentTsHead, parentTsTail) of
          (Nothing, Nothing) ->
            Nothing
          _ ->
            Just $ T.intercalate "," $
              Maybe.catMaybes
               [ parentTsHead
               , parentTsTail
               ]
    }
  }


-- |Creates a new W3C trace context value for an outgoing HTTP request when
-- tracing is suppressed.
inheritFromForSuppressed :: W3CTraceContext -> Id -> W3CTraceContext
inheritFromForSuppressed parentW3cTraceContext exitSpanSpanId =
  let
    parentTp = traceParent $ parentW3cTraceContext
    parentTs :: TraceState
    parentTs = traceState $ parentW3cTraceContext
    parentTsHead = traceStateHead parentTs
    parentTsTail = traceStateTail parentTs
  in
  W3CTraceContext
  { traceParent = TraceParent
    { version  = 0
    , traceId  = traceId $ parentTp
    , parentId = exitSpanSpanId
    , flags    = Flags
      { sampled = False
      }
    }
  , traceState = TraceState
    { traceStateHead      = parentTsHead
    , instanaKeyValuePair = Nothing
    , traceStateTail      = parentTsTail
    }
  }


-- |Creates a new W3C trace context value for an outgoing HTTP request from the
-- given trace ID and span ID, without inheriting from an existing context.
exitSpanContextFromIds :: Id -> Id -> W3CTraceContext
exitSpanContextFromIds exitSpanTraceId exitSpanSpanId =
  W3CTraceContext
  { traceParent = TraceParent
    { version  = 0
    , traceId  = exitSpanTraceId
    , parentId = exitSpanSpanId
    , flags    = Flags
      { sampled = True
      }
    }
  , traceState = TraceState
    { traceStateHead      = Nothing
    , instanaKeyValuePair = Just InstanaKeyValuePair
      { instanaTraceId  = exitSpanTraceId
      , instanaParentId = exitSpanSpanId
      }
    , traceStateTail      = Nothing
    }
  }


initBogusContextForSuppressedRequest :: IO W3CTraceContext
initBogusContextForSuppressedRequest = do
  bogusId <- Id.generate
  return $ createExitContextForSuppressed bogusId bogusId


-- |When tracing is suppressed but no W3C trace context is incoming , we still
-- need to send down W3C trace context headers to signal sampled=false to
-- downstream services.
createExitContextForSuppressed :: Id -> Id -> W3CTraceContext
createExitContextForSuppressed bogusTraceId bogusParentId =
  W3CTraceContext
  { traceParent = TraceParent
    { version  = 0
    , traceId  = bogusTraceId
    , parentId = bogusParentId
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


-- |Serializes the given W3C trace context to a pair of HTTP headers.
toHeaders :: W3CTraceContext -> [HTTPTypes.Header]
toHeaders w3cTraceContext =
  let
    tp = traceParent w3cTraceContext
    traceparentHeader =
      Just
        ( TracingHeaders.traceparentHeaderName
        , encodeTraceParent tp
        )
    ts = traceState w3cTraceContext
    tracestateHeader =
      if isEmpty ts then
        Nothing
      else
        Just
          ( TracingHeaders.tracestateHeaderName
          , encodeTraceState ts
          )
  in
  Maybe.catMaybes [traceparentHeader, tracestateHeader]


-- |Encodes the traceparent header value.
encodeTraceParent :: TraceParent -> BSC8.ByteString
encodeTraceParent tp =
  BSC8.concat
    [ BSC8.pack $ leftPad 2 $ show $ version tp
    , "-"
    , BSC8.pack $ leftPadAndLimit 32 $ Id.toStringUnshortened $ traceId tp
    , "-"
    , BSC8.pack $ leftPadAndLimit 16 $ Id.toString $ parentId tp
    , "-"
    , encodeFlags $ flags tp
    ]


-- |Encodes the traceparent flag field.
encodeFlags :: Flags -> BSC8.ByteString
encodeFlags fl =
  if sampled fl then "01"
  else "00"


-- |Encodes the tracestate header value.
encodeTraceState :: TraceState -> BSC8.ByteString
encodeTraceState ts =
  BSC8.intercalate "," $
    Maybe.catMaybes $
      -- We only encode for outgoing headers and for those we always move the
      -- Instana key value pair to the front in compliance with the W3C
      -- trace context specification
      ([ encodeInstanaKeyValuePair ts
       , (BSC8.pack . T.unpack) <$> traceStateHead ts
       , (BSC8.pack . T.unpack) <$> traceStateTail ts
      ])


-- |Encodes the Instana key-value pair for the tracestate header value.
encodeInstanaKeyValuePair :: TraceState -> Maybe BSC8.ByteString
encodeInstanaKeyValuePair ts =
  let
    inKvPair = instanaKeyValuePair ts
    inTId = instanaTraceId <$> inKvPair
    inPId = instanaParentId <$> inKvPair
  in
  case (inTId, inPId) of
    (Just t, Just p) ->
      Just $
        BSC8.concat
          [ "in="
          , BSC8.pack $ leftPad 16 $ Id.toString t
          , ";"
          , BSC8.pack $ leftPad 16 $ Id.toString p
          ]

    _ ->
      Nothing

