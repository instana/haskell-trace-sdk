{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instana.SDK.IntegrationTest.SpecCompliance
  ( allTestsW3cCorrelationOn
  , allTestsW3cCorrelationOff
  ) where


import           Control.Concurrent                     (threadDelay)
import           Data.Aeson                             ((.:), (.:?))
import qualified Data.Aeson                             as Aeson
import           Data.Aeson.Types                       (FromJSON)
import qualified Data.Aeson.Types                       as AesonTypes
import           Data.Array                             ((!))
import qualified Data.Array                             as Array
import qualified Data.ByteString.Char8                  as BSC8
import qualified Data.ByteString.Lazy.Char8             as LBSC8
import qualified Data.CaseInsensitive                   as CI
import           Data.Either                            (isLeft)
import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HashMap
import           Data.Maybe                             (catMaybes, isNothing,
                                                         listToMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Vector                            as Vector
import           Instana.SDK.AgentStub.TraceRequest     (From (..),
                                                         InstanaAncestor, Span)
import qualified Instana.SDK.AgentStub.TraceRequest     as TraceRequest
import           Instana.SDK.IntegrationTest.HUnitExtra (applyLabel,
                                                         assertAllIO, failIO)
import qualified Instana.SDK.IntegrationTest.HttpHelper as HttpHelper
import           Instana.SDK.IntegrationTest.Suite      (AppUnderTest)
import qualified Instana.SDK.IntegrationTest.TestHelper as TestHelper
import qualified Network.HTTP.Client                    as HTTP
import           Network.HTTP.Types                     (Header)
import qualified Network.HTTP.Types.Header
import           Test.HUnit
import qualified Text.Regex.Base.RegexLike              as RegexBase
import           Text.Regex.TDFA.String                 (Regex)


data RunSubsetOfCases =
    RunAll
  | RunSubset [Int]


-- replace RunAll with RunSubset[...] to run test cases selectively
testCasesToRun :: RunSubsetOfCases
testCasesToRun = RunAll


allTestsW3cCorrelationOn ::
  Aeson.Array
  -> AppUnderTest
  -> String
  -> String
  -> [IO Test]
allTestsW3cCorrelationOn
    specificationComplianceTestCases
    appUnderTest
    route
    pid = do
  jsonToTestCases
    appUnderTest
    False
    route
    pid
    specificationComplianceTestCases


allTestsW3cCorrelationOff ::
  Aeson.Array
  -> AppUnderTest
  -> String
  -> String
  -> [IO Test]
allTestsW3cCorrelationOff
    specificationComplianceTestCases
    appUnderTest
    route
    pid =
  jsonToTestCases
    appUnderTest
    True
    route
    pid
    specificationComplianceTestCases


data SpecTestCase = SpecTestCase
  { index                 :: Int
  , scenario              :: String
  , whatToDo              :: String
  , disableW3cCorrelation :: Maybe Text
  , xInstanaTIn           :: Maybe String
  , xInstanaSIn           :: Maybe String
  , xInstanaLIn           :: Maybe String
  , xInstanaSyntheticIn   :: Maybe String
  , traceparentIn         :: Maybe String
  , tracestateIn          :: Maybe String
  , queryIn               :: Maybe String
  , requestHeadersIn      :: Maybe String
  , serverTiming          :: Maybe String
  , entrySpanT            :: Maybe String
  , entrySpanP            :: Maybe String
  , entrySpanS            :: Maybe String
  , entrySpanIa           :: Maybe InstanaAncestor
  , entrySpanTp           :: Maybe Bool
  , entrySpanLt           :: Maybe String
  , entrySpanCrid         :: Maybe String
  , entrySpanCrtp         :: Maybe String
  , entrySpanSy           :: Maybe Bool
  , entrySpanParams       :: Maybe String
  , entrySpanHeaders      :: Maybe String
  , entrySpanService      :: Maybe String
  , exitSpanT             :: Maybe String
  , exitSpanP             :: Maybe String
  , exitSpanS             :: Maybe String
  , exitSpanIa            :: Maybe InstanaAncestor
  , exitSpanTp            :: Maybe Bool
  , exitSpanLt            :: Maybe String
  , exitSpanCrid          :: Maybe String
  , exitSpanCrtp          :: Maybe String
  , exitSpanSy            :: Maybe Bool
  , exitSpanParams        :: Maybe String
  , exitSpanHeaders       :: Maybe String
  , exitSpanService       :: Maybe String
  , xInstanaTOut          :: Maybe String
  , xInstanaSOut          :: Maybe String
  , xInstanaLOut          :: Maybe String
  , traceparentOut        :: Maybe String
  , tracestateOut         :: Maybe String
  }
  deriving (Show)


instance FromJSON SpecTestCase where
  parseJSON = Aeson.withObject "Spec Test Case" $
    \obj ->
      SpecTestCase
        <$> obj .: "index"
        <*> obj .: "Scenario"
        <*> obj .: "What to do?"
        <*> obj .:? "INSTANA_DISABLE_W3C_TRACE_CORRELATION"
        <*> obj .:? "X-INSTANA-T in"
        <*> obj .:? "X-INSTANA-S in"
        <*> obj .:? "X-INSTANA-L in"
        <*> obj .:? "X-INSTANA-SYNTHETIC in"
        <*> obj .:? "traceparent in"
        <*> obj .:? "tracestate in"
        <*> obj .:? "query in"
        <*> obj .:? "request headers in"
        <*> obj .:? "Server-Timing"
        <*> obj .:? "entrySpan.t"
        <*> obj .:? "entrySpan.p"
        <*> obj .:? "entrySpan.s"
        <*> obj .:? "entrySpan.ia"
        <*> obj .:? "entrySpan.tp"
        <*> obj .:? "entrySpan.lt"
        <*> obj .:? "entrySpan.crid"
        <*> obj .:? "entrySpan.crtp"
        <*> obj .:? "entrySpan.sy"
        <*> obj .:? "entrySpan.params"
        <*> obj .:? "entrySpan.headers"
        <*> obj .:? "entrySpan.data.service"
        <*> obj .:? "exitSpan.t"
        <*> obj .:? "exitSpan.p"
        <*> obj .:? "exitSpan.s"
        <*> obj .:? "exitSpan.ia"
        <*> obj .:? "exitSpan.tp"
        <*> obj .:? "exitSpan.lt"
        <*> obj .:? "exitSpan.crid"
        <*> obj .:? "exitSpan.crtp"
        <*> obj .:? "exitSpan.sy"
        <*> obj .:? "exitSpan.params"
        <*> obj .:? "exitSpan.headers"
        <*> obj .:? "exitSpan.data.service"
        <*> obj .:? "X-INSTANA-T out"
        <*> obj .:? "X-INSTANA-S out"
        <*> obj .:? "X-INSTANA-L out"
        <*> obj .:? "traceparent out"
        <*> obj .:? "tracestate out"


data SpanData = SpanData
  { httpAnnotations :: HttpAnnotations
  }
  deriving (Show)


instance FromJSON SpanData where
  parseJSON = Aeson.withObject "Span Annotations" $
    \obj ->
      SpanData
        <$> obj .: "http"


data HttpAnnotations = HttpAnnotations
  { method :: Maybe String
  , host   :: Maybe String
  , url    :: Maybe String
  , params :: Maybe String
  , status :: Maybe Int
  }
  deriving (Show)


instance FromJSON HttpAnnotations where
  parseJSON = Aeson.withObject "HTTP Annotations" $
    \obj ->
      HttpAnnotations
        <$> obj .:? "method"
        <*> obj .:? "host"
        <*> obj .:? "url"
        <*> obj .:? "params"
        <*> obj .:? "status"


type ValueForPlaceholder = (Text, Text)


type ValuesForPlaceholders = [ValueForPlaceholder]


type TestContext = ([Assertion], ValuesForPlaceholders)


addAssertion :: Assertion -> TestContext -> TestContext
addAssertion newAssertion (existingAssertions, valuesForPlaceholders) =
  (existingAssertions ++ [newAssertion], valuesForPlaceholders)


addAssertions :: [Assertion] -> TestContext -> TestContext
addAssertions newAssertions (existingAssertions, valuesForPlaceholders) =
  (existingAssertions ++ newAssertions, valuesForPlaceholders)


addPlaceholderValue :: ValueForPlaceholder -> TestContext -> TestContext
addPlaceholderValue newValue (assertions, valuesForPlaceholders) =
  (assertions, valuesForPlaceholders ++ [newValue])


data ExpectedActual =
    S String
  | MS (Maybe String)
  | MIA (Maybe InstanaAncestor)
  | MB (Maybe Bool)


instance Eq ExpectedActual where
  S s1 == S s2 =
    s1 == s2
  S s1 == MS (Just s2) =
    s1 == s2
  S _ == MS Nothing =
    False
  MS ms1 == S s2 =
    S s2 == MS ms1
  S _ == MIA _ =
    False
  MIA _ == S _ =
    False
  S _ == MB _ =
    False
  MB _ == S _ =
    False
  MS ms1 == MS ms2 =
    ms1 == ms2
  MS _ == MIA _ =
    False
  MIA _ == MS _ =
    False
  MS _ == MB _ =
    False
  MB _ == MS _ =
    False
  MIA mia1 == MIA mia2 =
    mia1 == mia2
  MIA _ == MB _ =
    False
  MB _ == MIA _ =
    False
  MB mb1 == MB mb2 =
    mb1 == mb2


instance Show ExpectedActual where
  show (S s)    = show s
  show (MS ms)  = show ms
  show (MIA ia) = show ia
  show (MB mb)  = show mb


unpack :: ExpectedActual -> String
unpack (S s)         = s
unpack (MS (Just s)) = s
unpack (MS Nothing)  = "Nothing"
unpack (MIA ia)      = show ia
unpack (MB mb)       = show mb


jsonToTestCases ::
  AppUnderTest
  -> Bool
  -> String
  -> String
  -> Aeson.Array
  -> [IO Test]
jsonToTestCases
    appUnderTest
    w3cCorrelationDisabled
    route
    pid
    allTestCaseAsJson = do
  let
    testCasesDefinitionAesonValues = Vector.toList allTestCaseAsJson
    testCasesDefinitionEitherValues :: [Either String SpecTestCase]
    testCasesDefinitionEitherValues =
      map jsonToTestCaseDefinition testCasesDefinitionAesonValues
    testCasesDefinitions :: [SpecTestCase]
    testCasesDefinitions =
      map
        (\testCaseDefinitionEither ->
          case testCaseDefinitionEither of
            Left errorMessage ->
              -- Could not parse JSON to test case definition.
              -- Because we are lazy, we just let the test suite crash :-)
              error errorMessage
            Right testCaseDefinition ->
              testCaseDefinition
        )
        testCasesDefinitionEitherValues
    testCaseDefinitionsForW3cDisabledSetting :: [SpecTestCase]
    testCaseDefinitionsForW3cDisabledSetting =
      filter
        (\testCaseDefinition ->
          if w3cCorrelationDisabled then
            case disableW3cCorrelation testCaseDefinition of
              Just value ->
                not $ T.null value
              Nothing ->
                False
          else
            case disableW3cCorrelation testCaseDefinition of
              Just value ->
                T.null value
              Nothing ->
                True
        )
        testCasesDefinitions
    testCaseDefinitionsFiltered :: [SpecTestCase]
    testCaseDefinitionsFiltered =
      case testCasesToRun of
        RunAll -> testCaseDefinitionsForW3cDisabledSetting
        RunSubset subset ->
          filter
            (\testCaseDefinition ->
              elem (index testCaseDefinition) subset
            )
            testCaseDefinitionsForW3cDisabledSetting
  map
    (testCaseDefinitionToTest appUnderTest route pid)
    testCaseDefinitionsFiltered


jsonToTestCaseDefinition :: Aeson.Value -> Either String SpecTestCase
jsonToTestCaseDefinition testCaseAsJson =
  AesonTypes.parseEither
    Aeson.parseJSON
    testCaseAsJson :: Either String SpecTestCase


testCaseDefinitionToTest ::
  AppUnderTest
  -> String
  -> String
  -> SpecTestCase
  -> IO Test
testCaseDefinitionToTest appUnderTest route pid testCaseDefinition = do
  let
    label =
      (show $ index testCaseDefinition) ++ ": " ++
      scenario testCaseDefinition ++ " -> " ++
      whatToDo testCaseDefinition
    headers = testCaseDefinitionToHeaders testCaseDefinition
    routeWithQuery =
       case queryIn testCaseDefinition of
         Just query -> route ++ "?" ++ query
         Nothing    -> route
  putStrLn $ "Creating test: " ++ label ++ "\nwith headers:\n" ++ show headers
  putStrLn $ "TEST CASE: " ++ show testCaseDefinition
  applyLabel label $
    runSpecTestCase
      appUnderTest
      route
      routeWithQuery
      pid
      headers
      testCaseDefinition


testCaseDefinitionToHeaders :: SpecTestCase -> [Header]
testCaseDefinitionToHeaders testCaseDefinition =
  catMaybes $
    map toHeader
      [ ("X-INSTANA-T", xInstanaTIn testCaseDefinition)
      , ("X-INSTANA-S", xInstanaSIn testCaseDefinition)
      , ("X-INSTANA-L", xInstanaLIn testCaseDefinition)
      , ("X-INSTANA-SYNTHETIC", xInstanaSyntheticIn testCaseDefinition)
      , ("traceparent", traceparentIn testCaseDefinition)
      , ("tracestate", tracestateIn testCaseDefinition)
      ]
  where
    toHeader (_, Nothing)       = Nothing
    toHeader (name, Just value) = Just (name, BSC8.pack value)


runSpecTestCase ::
  AppUnderTest
  -> String
  -> String
  -> String
  -> [Header]
  -> SpecTestCase
  -> IO Test
runSpecTestCase
  appUnderTest
  expectedEntryUrl
  routeWithQuery
  pid
  headers
  testCaseDefinition = do
  let
    suppressionHeader =
      filter
        (\(name, value) ->
          (name == (CI.mk (BSC8.pack "X-INSTANA-L"))) &&
          (value == BSC8.pack "0")
        )
        headers
    suppressed =
      (length suppressionHeader) >= 1
  executeRequestAndVerify
    appUnderTest
    testCaseDefinition
    expectedEntryUrl
    routeWithQuery
    pid
    suppressed
    headers


executeRequestAndVerify ::
  AppUnderTest
  -> SpecTestCase
  -> String
  -> String
  -> String
  -> Bool
  -> [Header]
  -> IO Test
executeRequestAndVerify
  appUnderTest
  testCaseDefinition
  expectedEntryUrl
  routeWithQuery
  pid
  suppressed
  headers = do
  response <-
    HttpHelper.doAppRequest
      appUnderTest
      routeWithQuery
      "GET"
      headers
  let
    initialTestContext = ([], [])

    testContextAfterServerTimingCheck =
       verifyServerTimingHeader
         testCaseDefinition
         initialTestContext
         response

    eitherResponseBody =
      Aeson.eitherDecode' $ HTTP.responseBody response

    from = Just $ From pid "agent-stub-id"

  case eitherResponseBody of
    Right responseBody -> do
      let
        testContextAfterDownstreamHeaderCheck =
           verifyHttpHeadersOnDownstreamRequest
             testCaseDefinition
             testContextAfterServerTimingCheck
             responseBody

      case suppressed of

        False ->
          verifySpans
            testCaseDefinition
            testContextAfterDownstreamHeaderCheck
            expectedEntryUrl
            responseBody
            from

        True ->
          verifySuppression testContextAfterDownstreamHeaderCheck

    Left decodeError ->
      failIO $ "Could decode HTTP response body: " ++ decodeError


verifyServerTimingHeader ::
  SpecTestCase
  -> TestContext
  -> HTTP.Response LBSC8.ByteString
  -> TestContext
verifyServerTimingHeader testCaseDefinition testContext response =
  let
    responseHeaders = HTTP.responseHeaders response
    serverTimingTuple ::
      Maybe (Network.HTTP.Types.Header.HeaderName, BSC8.ByteString)
    serverTimingTuple =
      listToMaybe $
        filter
          (\ (headerName, _) -> headerName == "Server-Timing")
          responseHeaders
    actualServerTimingValueM = BSC8.unpack <$> (snd <$> serverTimingTuple)
    expectedServerTimingValueM = serverTiming testCaseDefinition
  in
  case (expectedServerTimingValueM, actualServerTimingValueM) of
    (Just expectedServerTimingValue, Just actualServerTimingValue) ->
      if containsPlaceholder expectedServerTimingValue then
        parseForPlaceholders
          testContext
          "Server-Timing header"
          expectedServerTimingValue
          actualServerTimingValue
      else
        addAssertion
          (assertEqual "Server-Timing header"
             expectedServerTimingValue
             actualServerTimingValue
          )
          testContext

    (Just expectedServerTimingValue, Nothing)  ->
      addAssertion
        (assertFailure $ "Expected Server-Timing header " ++
           expectedServerTimingValue ++ ", but got nothing."
        )
        testContext

    (Nothing, Just actualServerTimingValue)  ->
      addAssertion
        (assertFailure $ "Expected no Server-Timing header, but got " ++
            actualServerTimingValue ++ "."
        )
        testContext

    (Nothing, Nothing)  ->
      testContext


verifyHttpHeadersOnDownstreamRequest ::
  SpecTestCase
  -> TestContext
  -> HashMap Text AesonTypes.Value
  -> TestContext
verifyHttpHeadersOnDownstreamRequest
  testCaseDefinition
  testContext
  responseBody =
  let
    testContextAfterXInstanaLCheck =
      addAssertion
        (xInstanaLAssertion responseBody testCaseDefinition)
        testContext
  in
  foldr
    (\(header, accessor) currentTestContext ->
      let
        message =
          "value for downstream HTTP header " ++ (T.unpack header)
        expectedValueM :: Maybe String
        expectedValueM = accessor testCaseDefinition
      in
      case expectedValueM of

        Just expectedValue ->
          if containsPlaceholder expectedValue then
            parseForPlaceholderJsonValue
              currentTestContext
              message
              expectedValue
              (HashMap.lookup header responseBody)
          else
            addAssertion
              (assertEqualInMap message expectedValue header responseBody)
              currentTestContext
        Nothing ->
          addAssertion
            (assertNotInMap message header responseBody)
            currentTestContext
    )
    testContextAfterXInstanaLCheck
    [ ("X-INSTANA-T", xInstanaTOut)
    , ("X-INSTANA-S", xInstanaSOut)
    -- X-INSTANA-L is checked separately via xInstanaLAssertion
    , ("traceparent", traceparentOut)
    , ("tracestate", tracestateOut)
    ]


xInstanaLAssertion :: HashMap Text AesonTypes.Value -> SpecTestCase -> Assertion
xInstanaLAssertion responseBody testCaseDefinition =
  case xInstanaLOut testCaseDefinition of
    Just "1" ->
      let
        actualValueM = HashMap.lookup "X-INSTANA-L" responseBody
      in
      assertBool "Expected X-INSTANA-L to be either absent or \"1\"" $
        (actualValueM == Nothing) || (actualValueM == (Just $ Aeson.String "1"))

    Just "0" ->
      assertEqualInMap
        "value for downstream HTTP header X-INSTANA-L"
        "0"
        "X-INSTANA-L"
        responseBody

    _ ->
      fail $
        "Unexpected expectation value for downstream X-INSTANA-L in test " ++
        "case defintion: " ++
        (show $ xInstanaLOut testCaseDefinition)


verifySpans ::
  SpecTestCase
  -> TestContext
  -> String
  -> HashMap Text AesonTypes.Value
  -> Maybe From
  -> IO Test
verifySpans
  testCaseDefinition
  testContext
  expectedEntryUrl
  responseBody
  from = do
  spansResults <-
    TestHelper.waitForRegisteredSpansMatching
      [ "haskell.wai.server", "haskell.http.client" ]
  case spansResults of
    Left failure ->
      failIO $ "Could not load recorded spans from agent stub: " ++ failure
    Right spans -> do
      let
        maybeEntrySpan =
          TestHelper.getSpanByRegisteredName "haskell.wai.server" spans
        maybeExitSpan =
          TestHelper.getSpanByRegisteredName "haskell.http.client" spans
      if isNothing maybeEntrySpan || isNothing maybeExitSpan
        then
          failIO "expected spans have not been recorded"
        else do
          let
            Just entrySpan = maybeEntrySpan
            entrySpanDataAeson = (TraceRequest.spanData entrySpan)
            entryHttpAnnotationsEither :: Either String HttpAnnotations
            entryHttpAnnotationsEither =
              fmap httpAnnotations $
                (AesonTypes.parseEither
                 Aeson.parseJSON
                 entrySpanDataAeson :: Either String SpanData)
            Just exitSpan = maybeExitSpan
            exitSpanDataAeson = (TraceRequest.spanData exitSpan)
            exitHttpAnnotationsEither :: Either String HttpAnnotations
            exitHttpAnnotationsEither =
              fmap httpAnnotations $
                (AesonTypes.parseEither
                 Aeson.parseJSON
                 exitSpanDataAeson :: Either String SpanData)
          putStrLn $ "ENTRY " ++ show entrySpan
          putStrLn $ "EXIT " ++ show exitSpan
          putStrLn $ "RESPONSE " ++ show responseBody
          if isLeft entryHttpAnnotationsEither then do
            let
              Left msg = entryHttpAnnotationsEither
            failIO $ "Could not parse annotations of HTTP entry span: " ++ msg
          else if isLeft exitHttpAnnotationsEither then do
            let
              Left msg = exitHttpAnnotationsEither
            failIO $ "Could not parse annotations of HTTP entry span: " ++ msg
          else do
            let
              Right entryHttpAnnotations = entryHttpAnnotationsEither
              Right exitHttpAnnotations = exitHttpAnnotationsEither
              (assertions, _) =
                (spanAssertions
                  testCaseDefinition
                  testContext
                  expectedEntryUrl
                  entrySpan
                  entryHttpAnnotations
                  exitSpan
                  exitHttpAnnotations
                  from
                )
            assertAllIO $ assertions


spanAssertions ::
  SpecTestCase
  -> TestContext
  -> String
  -> Span
  -> HttpAnnotations
  -> Span
  -> HttpAnnotations
  -> Maybe From
  -> TestContext
spanAssertions
  testCaseDefinition
  testContext
  expectedEntryUrl
  entrySpan
  entryHttpAnnotations
  exitSpan
  exitHttpAnnotations
  from =
  addAssertions
    (fixedSpanAssertions
        entrySpan
        exitSpan
        from
    ++
    httpAnnotationAssertions
        testCaseDefinition
        expectedEntryUrl
        entryHttpAnnotations
        exitHttpAnnotations
    )
    (spanAssertionsFromTestCaseDefinition
        testCaseDefinition
        testContext
        entrySpan
        exitSpan
    )


fixedSpanAssertions ::
  Span
  -> Span
  -> Maybe From
  -> [Assertion]
fixedSpanAssertions
  entrySpan
  exitSpan
  from =
  [ assertBool "entry timestamp" $ TraceRequest.ts entrySpan > 0
  , assertBool "entry duration" $ TraceRequest.d entrySpan > 0
  , assertEqual "entry kind" 1 (TraceRequest.k entrySpan)
  , assertEqual "entry error count" 0 (TraceRequest.ec entrySpan)
  , assertEqual "entry from" from $ TraceRequest.f entrySpan
  , assertBool "exit timestamp" $ TraceRequest.ts exitSpan > 0
  , assertBool "exit duration" $ TraceRequest.d exitSpan > 0
  , assertEqual "exit kind" 2 (TraceRequest.k exitSpan)
  , assertEqual "exit error count" 0 (TraceRequest.ec exitSpan)
  , assertEqual "exit from" from $ TraceRequest.f exitSpan
  ]


httpAnnotationAssertions ::
  SpecTestCase
  -> String
  -> HttpAnnotations
  -> HttpAnnotations
  -> [Assertion]
httpAnnotationAssertions
  testCaseDefinition
  expectedEntryUrl
  entryHttpAnnotations
  exitHttpAnnotations =
  [ assertEqual "entry http method"
      (Just "GET" :: Maybe String)
      (method entryHttpAnnotations)
  , assertEqual "entry http host"
      (Just "127.0.0.1:1207" :: Maybe String)
      (host entryHttpAnnotations)
  , assertEqual "entry http url"
      (Just ("/" ++ expectedEntryUrl) :: Maybe String)
      (url entryHttpAnnotations)
  , assertEqual "entry http params"
      (entrySpanParams testCaseDefinition)
      (params entryHttpAnnotations)
  , assertEqual "entry http status"
      (Just 200 :: Maybe Int)
      (status entryHttpAnnotations)
  , assertEqual "exit http method"
      (Just "GET" :: Maybe String)
      (method exitHttpAnnotations)
  , assertEqual "exit http url"
      (Just "http://127.0.0.1:1208/echo" :: Maybe String)
      (url exitHttpAnnotations)
  , assertEqual "exit http params"
      (exitSpanParams testCaseDefinition)
      (params exitHttpAnnotations)
  , assertEqual "exit http status"
      (Just 200 :: Maybe Int)
      (status exitHttpAnnotations)
  ]


spanAssertionsFromTestCaseDefinition ::
  SpecTestCase
  -> TestContext
  -> Span
  -> Span
  -> TestContext
spanAssertionsFromTestCaseDefinition
  testCaseDefinition
  testContext
  entrySpan
  exitSpan =
  let
    testContextAfterEntrySpanChecks =
      verifyEntrySpan
        testCaseDefinition
        testContext
        entrySpan
  in
  verifyExitSpan
    testCaseDefinition
    testContextAfterEntrySpanChecks
    exitSpan


verifyEntrySpan ::
  SpecTestCase
  -> TestContext
  -> Span
  -> TestContext
verifyEntrySpan testCaseDefinition testContext entrySpan =
  verifySpan testCaseDefinition testContext "entry" entrySpan $
    [ ( "t"   , MS  . entrySpanT    , S   . TraceRequest.t    )
    , ( "p"   , MS  . entrySpanP    , MS  . TraceRequest.p    )
    , ( "s"   , MS  . entrySpanS    , S   . TraceRequest.s    )
    , ( "ia"  , MIA . entrySpanIa   , MIA . TraceRequest.ia   )
    , ( "tp"  , MB  . entrySpanTp   , MB  . TraceRequest.tp   )
    , ( "lt"  , MS  . entrySpanLt   , MS  . TraceRequest.lt   )
    , ( "crid", MS  . entrySpanCrid , MS  . TraceRequest.crid )
    , ( "crtp", MS  . entrySpanCrtp , MS  . TraceRequest.crtp )
    , ( "sy"  , MB  . entrySpanSy   , MB  . TraceRequest.sy   )
    ]


verifyExitSpan ::
  SpecTestCase
  -> TestContext
  -> Span
  -> TestContext
verifyExitSpan testCaseDefinition testContext exitSpan =
  verifySpan testCaseDefinition testContext "exit" exitSpan $
  [ ( "t"   , MS  . exitSpanT    , S   . TraceRequest.t    )
  , ( "p"   , MS  . exitSpanP    , MS  . TraceRequest.p    )
  , ( "s"   , MS  . exitSpanS    , S   . TraceRequest.s    )
  , ( "ia"  , MIA . exitSpanIa   , MIA . TraceRequest.ia   )
  , ( "tp"  , MB  . exitSpanTp   , MB  . TraceRequest.tp   )
  , ( "lt"  , MS  . exitSpanLt   , MS  . TraceRequest.lt   )
  , ( "crid", MS  . exitSpanCrid , MS  . TraceRequest.crid )
  , ( "crtp", MS  . exitSpanCrtp , MS  . TraceRequest.crtp )
  , ( "sy"  , MB  . exitSpanSy   , MB  . TraceRequest.sy   )
  ]


verifySpan ::
  SpecTestCase
  -> TestContext
  -> String
  -> Span
  -> [(String, SpecTestCase -> ExpectedActual, Span -> ExpectedActual)]
  -> TestContext
verifySpan testCaseDefinition testContext spanKindLabel span_ accessorPairs =
  foldr
    (\(attributeLabel, testCaseAccesor, spanAccesor) currentTestContext ->
      let
        label = spanKindLabel ++ " span." ++ attributeLabel
        expectedValue = testCaseAccesor testCaseDefinition
        actualValue = spanAccesor span_
      in
      if containsPlaceholder' expectedValue then
        parseForPlaceholders
          currentTestContext
          label
          (unpack expectedValue)
          (unpack actualValue)
      else
        addAssertion
          (assertEqual
            label
            expectedValue
            actualValue
          )
          currentTestContext
    )
    testContext
    accessorPairs


verifySuppression ::
  TestContext
  -> IO Test
verifySuppression (assertions, _) = do
  -- wait a few seconds, then check that no spans have been recorded
  threadDelay $ 5 * 1000 * 1000
  spansResults <-
    TestHelper.waitForRegisteredSpansMatching []
  case spansResults of
    Left failure ->
      failIO $ "Could not load recorded spans from agent stub: " ++ failure
    Right spans -> do
      if not (null spans)
        then
          failIO "spans have been recorded although they should have not"
        else
          assertAllIO $ assertions


containsPlaceholder :: String -> Bool
containsPlaceholder = elem '$'


containsPlaceholder' :: ExpectedActual -> Bool
containsPlaceholder' (S s)         = elem '$' s
containsPlaceholder' (MS (Just s)) = elem '$' s
containsPlaceholder' (MS Nothing)  = False
containsPlaceholder' (MIA _)       = False
containsPlaceholder' (MB _)        = False


parseForPlaceholders ::
  TestContext
  -> String
  -> String
  -> String
  -> TestContext
parseForPlaceholders testContext label template value =
  let
    templateRegex =
      (RegexBase.makeRegex ("\\$[a-z0-9_]+" :: String)) :: Regex

    -- I'm sorry, this is ugly and complicated. To ease the pain, let's follow
    -- along with an example. Assuming we have:
    -- template = 00-0000000000000000$new_64_bit_trace_id-$new_span_id_2-01
    -- value = 00-0000000000000000b9e374754ca092b9-de54a2e7e1ceffc4-01
    -- Then we will get:

    -- placeholdersInString = [ '$new_64_bit_trace_id', '$new_span_id_2' ]
    placeholdersInStringMatches = RegexBase.matchAllText templateRegex template
    placeholdersInString =
       map
         (\match-> T.pack $ fst $ match ! 0)
         placeholdersInStringMatches

    -- Now escape "$" characters so they are used as literals in the regex:
    -- escapedPlaceholdersInString = [ '\\$new_64_bit_trace_id', '\\$new_span_id_2' ]
    escapedPlaceholdersInString =
      map (\tpl -> T.replace "$" "\\$" tpl) placeholdersInString

    -- Now we want to find all _fixed_ parts of the template string, that is,
    -- the parts between the placeholders:
    -- placeholderRegex = "^(.*)\\$new_64_bit_trace_id(.*)\\$new_span_id_2(.*)$"
    placeholderRegex =
      constructRegex "^(.*)" escapedPlaceholdersInString "(.*)" "(.*)$"

    fixedLiteralsMatchStructure =
      RegexBase.matchOnceText placeholderRegex template
    in
    case fixedLiteralsMatchStructure of

      Nothing ->
        addAssertion
          (assertFailure
              ("Failure: No fixed literals match for template " ++ template ++
               " (" ++ label ++ ")."
              )
          )
          testContext

      Just (_, fixedLiteralsMatches, _) ->
        if null fixedLiteralsMatches
          then
            addAssertion
              (assertFailure
                  ("Failure: No placeholder match result " ++ template ++
                   " (" ++ label ++ ")."
                  )
              )
              testContext
          else
            let
               -- fixedLiterals = [ '00-0000000000000000', '-', '-01' ]
               fixedLiterals = extractMatches fixedLiteralsMatches

               -- valuesRegex = '^00-0000000000000000(.*)-(.*)-01$'
               valuesRegex =
                 constructRegex "^" fixedLiterals "(.*)" "$"

               -- Finally, extract the values that the current value has for the
               -- placeholders.
               valuesMatchStructure =
                 RegexBase.matchOnceText valuesRegex value
               in
               case valuesMatchStructure of

                 Nothing ->
                   addAssertion
                     (assertFailure $
                       "Failure: Could not match the value " ++ value ++
                       " against the template " ++ template ++
                       " (" ++ label ++ ")."
                     )
                     testContext

                 Just (_, valuesMatches, _) ->
                   if null valuesMatches
                     then
                       addAssertion
                         (assertFailure $
                            "Failure: Could not match the value " ++ value ++
                            " against the template " ++ template ++
                            " (" ++ label ++ ")."
                         )
                         testContext
                     else
                       let
                         -- actualValues =
                         --   ['3124d02b3e5b1531', 'e16a9d4443b7e2d1']
                         actualValues = extractMatches valuesMatches
                       in
                       updatePlaceholdersAndCreateAssertions
                         testContext
                         label
                         placeholdersInString
                         actualValues


updatePlaceholdersAndCreateAssertions ::
  TestContext
  -> String
  -> [Text]
  -> [Text]
  -> TestContext
updatePlaceholdersAndCreateAssertions
  testContext
  label
  placeholdersInString
  actualValues =
  foldr
    (\(idx, placeholder) currentTestContext ->
       let
         (_, currentValuesForPlaceholders) = currentTestContext
         existingValueM = lookup placeholder currentValuesForPlaceholders
         newValue = actualValues !! idx
       in
       case existingValueM of
         Just existingValue ->
           addAssertion
             ( assertEqual
                 ( T.unpack $ T.concat
                   [ "The placeholder "
                   , placeholder
                   , " had the value "
                   , existingValue
                   , " earlier but now it has the value "
                   , newValue
                   , ". The same placeholders needs to always have "
                   , "the same value throughout one single test case ("
                   , (T.pack label)
                   , ")."
                   ]
                 )
                 existingValue
                 newValue
             )
             currentTestContext

         Nothing ->
           addPlaceholderValue
             (placeholder, newValue)
             testContext
    )
    testContext
    (zip [0..] placeholdersInString)


parseForPlaceholderJsonValue ::
  TestContext
  -> String
  -> String
  -> Maybe AesonTypes.Value
  -> TestContext
parseForPlaceholderJsonValue testContext label template value =
  case value of
    Just aesonValue ->
      case aesonValue of
        AesonTypes.String stringValue ->
          parseForPlaceholders
            testContext
            label
            template
            (T.unpack stringValue)
        _ ->
          addAssertion
            (assertFailure $
                "Failure: Expecte a string value but instead got " ++
                show aesonValue ++ " when matching against the template " ++
                template ++ " (" ++ label ++ ")."
            )
            testContext
    Nothing ->
      addAssertion
        (assertFailure $
            "Failure: A value was expected to be present in the HTTP " ++
            "response but was not when matching against the template " ++
            template ++ " (" ++ label ++ ")."

        )
        testContext


constructRegex :: Text -> [Text] -> Text -> Text -> Regex
constructRegex start strings separator end =
  RegexBase.makeRegex
    (T.unpack $ T.concat
      [ start
      , T.intercalate separator strings
      , end
      ])


extractMatches :: RegexBase.MatchText String -> [Text]
extractMatches matchArray =
  map
    (\submatch -> T.pack $ fst submatch)
    (tail $ Array.elems matchArray)


assertEqualInMap ::
  String ->
  String ->
  Text ->
  HashMap Text Aeson.Value ->
  Assertion
assertEqualInMap label expected key jsonMap =
  assertEqual label
      (Just $ Aeson.String $ T.pack $ expected)
      (HashMap.lookup key jsonMap)


assertNotInMap ::
  String ->
  Text ->
  HashMap Text Aeson.Value ->
  Assertion
assertNotInMap label key jsonMap =
  assertEqual label Nothing (HashMap.lookup key jsonMap)

