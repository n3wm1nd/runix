{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LangFuseSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.State (State, get, put, runState)
import Polysemy.Fail (Fail, runFail)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Aeson (Value, FromJSON, decode, encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.Time (UTCTime)

import Runix.Tracing.LangFuse
import Runix.HTTP (HTTP(..), HTTPRequest(..), HTTPResponse(..), httpIO_)
import Runix.RestAPI (RestAPI(..), Endpoint(..), RestEndpoint(..), restapiHTTP)
import qualified Runix.RestAPI as RestAPI
import Runix.Logging (Logging(..), Level(..), info)
import Runix.Cancellation (Cancellation, cancelNoop)
import Runix.StreamChunk (StreamChunk, ignoreChunks)
import Data.String (fromString)
import UniversalLLM
import UniversalLLM.Providers.OpenAI (LlamaCpp(..))
import UniversalLLM.Models.Minimax.M (MinimaxM25(..))
import Runix.LLM (LLM, queryLLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import System.Environment (lookupEnv)
import Runix.Time (Time(..), timeIO, getCurrentTime)

-- ============================================================================
-- Test Data
-- ============================================================================

mockLangFuse :: LangFuse
mockLangFuse = LangFuse
  { langfusePublicKey = "pk-test-123"
  , langfuseSecretKey = "sk-test-456"
  , langfuseBaseUrl = "https://test.langfuse.com"
  }

anthropicRequest :: HTTPRequest
anthropicRequest = HTTPRequest
  { method = "POST"
  , uri = "https://api.anthropic.com/v1/messages"
  , headers = [("Content-Type", "application/json")]
  , body = Just $ encode $ object
      [ "model" .= ("claude-3-5-sonnet-20241022" :: T.Text)
      , "messages" .= [object ["role" .= ("user" :: T.Text), "content" .= ("Hello" :: T.Text)]]
      , "max_tokens" .= (100 :: Int)
      ]
  }

anthropicResponse :: HTTPResponse
anthropicResponse = HTTPResponse
  { code = 200
  , headers = [("Content-Type", "application/json")]
  , body = encode $ object
      [ "id" .= ("msg_123" :: T.Text)
      , "type" .= ("message" :: T.Text)
      , "role" .= ("assistant" :: T.Text)
      , "content" .= [object ["type" .= ("text" :: T.Text), "text" .= ("Hi there!" :: T.Text)]]
      , "usage" .= object
          [ "input_tokens" .= (10 :: Int)
          , "output_tokens" .= (5 :: Int)
          ]
      ]
  }

openaiRequest :: HTTPRequest
openaiRequest = HTTPRequest
  { method = "POST"
  , uri = "https://api.openai.com/v1/chat/completions"
  , headers = [("Content-Type", "application/json")]
  , body = Just $ encode $ object
      [ "model" .= ("gpt-4" :: T.Text)
      , "messages" .= [object ["role" .= ("user" :: T.Text), "content" .= ("Test" :: T.Text)]]
      ]
  }

nonLLMRequest :: HTTPRequest
nonLLMRequest = HTTPRequest
  { method = "GET"
  , uri = "https://example.com/api/data"
  , headers = []
  , body = Nothing
  }

-- ============================================================================
-- Mock Interpreters
-- ============================================================================

-- | Captured OTLP trace data
data CapturedTrace = CapturedTrace
  { capturedEndpoint :: String
  , capturedPayload :: Value
  } deriving (Show, Eq)

-- | Mock RestAPI interpreter that captures requests
mockRestAPI :: Member (State [CapturedTrace]) r
            => Sem (RestAPI LangFuse : r) a
            -> Sem r a
mockRestAPI = interpret $ \case
  RestRequest _method (Endpoint endpoint) maybeData -> do
    -- Capture the request
    case maybeData of
      Just payload -> do
        captured <- get @[CapturedTrace]
        put (CapturedTrace endpoint (Aeson.toJSON payload) : captured)
      Nothing -> return ()

    -- Return a mock success response
    -- We just parse a generic success object, which should work for Value
    case Aeson.fromJSON (object ["success" .= True]) of
      Aeson.Success x -> return x
      Aeson.Error err -> error $ "Mock parse error: " <> err

-- | Mock HTTP interpreter that returns fixed responses
mockHTTP :: Member (State [HTTPRequest]) r
         => HTTPResponse
         -> Sem (HTTP : r) a
         -> Sem r a
mockHTTP response = interpret $ \case
  HttpRequest req -> do
    -- Capture the request
    captured <- get @[HTTPRequest]
    put (req : captured)
    -- Return mock response
    return $ Right response

-- | Mock Time interpreter that returns fixed time
mockTime :: UTCTime -> Sem (Time : r) a -> Sem r a
mockTime fixedTime = interpret $ \case
  GetCurrentTime -> return fixedTime

-- | Mock Logging interpreter that captures logs
loggingToIORef :: Member (Embed IO) r => IORef [String] -> Sem (Logging : r) a -> Sem r a
loggingToIORef logsRef = interpret $ \case
  Log _level _stack msg -> embed $ modifyIORef logsRef (++ [T.unpack msg])

-- | Null logging interpreter
loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
  Log _ _ _ -> return ()

-- ============================================================================
-- Test Runner Helpers
-- ============================================================================

-- | Run a test action with mocked HTTP and RestAPI, capturing requests
runTest :: HTTPResponse
        -> Sem '[HTTP, RestAPI LangFuse, Time, Logging, State [CapturedTrace], State [HTTPRequest]] ()
        -> ([HTTPRequest], [CapturedTrace])
runTest response action =
  let (httpReqs, (otlpTraces, ())) = run
        $ runState ([] :: [HTTPRequest])
        $ runState ([] :: [CapturedTrace])
        $ loggingNull
        $ mockTime fixedTime
        $ mockRestAPI
        $ mockHTTP response
        $ action
  in (httpReqs, otlpTraces)
  where
    fixedTime = read "2024-01-01 00:00:00 UTC" :: UTCTime

-- | Run a real LLM test with LangFuse tracing
runRealLLMTest :: LangFuse
               -> LlamaCppAuth
               -> Sem '[LLM (Model MinimaxM25 LlamaCpp), StreamChunk BS.ByteString, Cancellation, RestAPI LangFuse, HTTP, Time, Logging, Fail, Embed IO] a
               -> IO (Either String a)
runRealLLMTest langfuse auth action = do
  logsRef <- newIORef []
  runM
    $ runFail
    $ loggingToIORef logsRef
    $ timeIO
    $ httpIO_
    $ restapiHTTP langfuse
    $ withLangFuse langfuse
    $ cancelNoop
    $ ignoreChunks @BS.ByteString
    $ interpretLLMWith auth route (Model MinimaxM25 LlamaCpp) []
    $ action

-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec
spec = describe "LangFuse OpenTelemetry Integration" $ do

  describe "LLM Request Detection" $ do
    it "detects Anthropic requests" $ do
      let (httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest anthropicRequest
              return ()

      -- Should have captured the HTTP request
      length httpReqs `shouldBe` 1

      -- Should have sent an OTLP trace
      length otlpTraces `shouldBe` 1

      let trace = head otlpTraces
      capturedEndpoint trace `shouldBe` "v1/traces"

    it "detects OpenAI requests" $ do
      let (httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest openaiRequest
              return ()

      -- Should have captured the HTTP request
      length httpReqs `shouldBe` 1

      -- Should have sent an OTLP trace
      length otlpTraces `shouldBe` 1

    it "does not trace non-LLM requests" $ do
      let (httpReqs, otlpTraces) = runTest (HTTPResponse 200 [] "OK") $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest nonLLMRequest
              return ()

      -- Should have captured the HTTP request
      length httpReqs `shouldBe` 1

      -- Should NOT have sent an OTLP trace
      length otlpTraces `shouldBe` 0

  describe "OTLP Span Generation" $ do
    it "generates valid OTLP span with correct attributes" $ do
      let (_httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest anthropicRequest
              return ()

      -- Should have exactly one trace
      length otlpTraces `shouldBe` 1

      let trace = head otlpTraces
          payload = capturedPayload trace

      -- Verify OTLP structure exists
      case payload of
        Aeson.Object obj -> do
          -- Should have resourceSpans
          case KM.lookup "resourceSpans" obj of
            Just (Aeson.Array _) -> return ()
            _ -> expectationFailure "Missing or invalid resourceSpans"
        _ -> expectationFailure "Payload is not an object"

    it "includes gen_ai.system attribute" $ do
      let (_httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest anthropicRequest
              return ()

      let payload = capturedPayload $ head otlpTraces

      -- Should contain gen_ai.system attribute
      show payload `shouldContain` "gen_ai.system"
      show payload `shouldContain` "anthropic"

    it "includes model information from request" $ do
      let (_httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest anthropicRequest
              return ()

      let payload = capturedPayload $ head otlpTraces

      -- Should contain model information
      show payload `shouldContain` "gen_ai.request.model"
      show payload `shouldContain` "claude-3-5-sonnet"

    it "includes usage information from response" $ do
      let (_httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest anthropicRequest
              return ()

      let payload = capturedPayload $ head otlpTraces

      -- Should contain usage information
      show payload `shouldContain` "gen_ai.usage"
      show payload `shouldContain` "input_tokens"
      show payload `shouldContain` "output_tokens"

  describe "Error Handling" $ do
    it "handles HTTP errors gracefully" $ do
      let errorResponse = HTTPResponse
            { code = 500
            , headers = []
            , body = "Internal Server Error"
            }

      let (_httpReqs, otlpTraces) = runTest errorResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest anthropicRequest
              return ()

      -- Should still send trace even on error
      length otlpTraces `shouldBe` 1

      let payload = capturedPayload $ head otlpTraces

      -- Should include error status
      show payload `shouldContain` "HTTP 500"

  describe "Logging" $ do
    it "logs trace export events" $ do
      logsRef <- newIORef ([] :: [String])

      let fixedTime = read "2024-01-01 00:00:00 UTC" :: UTCTime

      _ <- runM
         $ runState ([] :: [CapturedTrace])
         $ mockRestAPI
         $ runState ([] :: [HTTPRequest])
         $ mockHTTP anthropicResponse
         $ mockTime fixedTime
         $ loggingToIORef logsRef
         $ withLangFuse mockLangFuse $ do
             _ <- send $ HttpRequest anthropicRequest
             return ()

      logs <- readIORef logsRef

      -- Should log both exporting and exported messages
      length logs `shouldBe` 2
      head logs `shouldContain` "LangFuse: Exporting span"
      last logs `shouldContain` "LangFuse: Exported span"

  describe "Provider Detection" $ do
    it "correctly identifies OpenRouter as OpenAI-compatible" $ do
      let openrouterRequest = HTTPRequest
            { method = "POST"
            , uri = "https://openrouter.ai/api/v1/chat/completions"
            , headers = []
            , body = Just $ encode $ object ["model" .= ("test" :: T.Text)]
            }

      let (_httpReqs, otlpTraces) = runTest anthropicResponse $
            withLangFuse mockLangFuse $ do
              _ <- send $ HttpRequest openrouterRequest
              return ()

      let payload = capturedPayload $ head otlpTraces

      -- Should map OpenRouter to openai system
      show payload `shouldContain` "gen_ai.system"
      show payload `shouldContain` "openai"

  describe "Real API Integration (Temporary)" $ do
    it "sends trace to actual LangFuse instance (manual)" $ do
      mLangFuse <- runM langFuseFromEnv
      case mLangFuse of
        Nothing -> pendingWith "LANGFUSE_PUBLIC_KEY, LANGFUSE_SECRET_KEY not set"
        Just langfuse -> do
          let anthropicRequest = HTTPRequest
                { method = "POST"
                , uri = "https://api.anthropic.com/v1/messages"
                , headers = [("Content-Type", "application/json")]
                , body = Just $ encode $ object
                    [ "model" .= ("claude-3-5-sonnet-20241022" :: T.Text)
                    , "messages" .= [object ["role" .= ("user" :: T.Text), "content" .= ("Hello from test!" :: T.Text)]]
                    , "max_tokens" .= (100 :: Int)
                    ]
                }
              mockResponse = HTTPResponse
                { code = 200
                , headers = [("Content-Type", "application/json")]
                , body = encode $ object
                    [ "id" .= ("msg_test_123" :: T.Text)
                    , "type" .= ("message" :: T.Text)
                    , "role" .= ("assistant" :: T.Text)
                    , "content" .= [object ["type" .= ("text" :: T.Text), "text" .= ("Hi there!" :: T.Text)]]
                    , "usage" .= object
                        [ "input_tokens" .= (15 :: Int)
                        , "output_tokens" .= (8 :: Int)
                        ]
                    ]
                }

          logsRef <- newIORef []
          result <- runM
            $ runFail
            $ loggingToIORef logsRef
            $ timeIO
            $ httpIO_
            $ restapiHTTP langfuse
            $ withLangFuse langfuse
            $ do
                currentTime <- getCurrentTime
                traceId <- generateTraceId
                spanId <- generateSpanId

                let span = buildSpan traceId spanId anthropicRequest currentTime currentTime (Right mockResponse)
                    exportReq = mkExportRequest span

                info $ fromString $ "Sending test trace to LangFuse at " <> langfuseBaseUrl langfuse
                (_ :: Value) <- RestAPI.post @LangFuse (Endpoint "v1/traces") exportReq
                info "Successfully sent trace to LangFuse!"
                return ()

          case result of
            Left err -> expectationFailure $ "Failed to send trace: " <> err
            Right () -> return ()

    it "sends trace from real LLM call to llama.cpp" $ do
      mLangFuse <- runM langFuseFromEnv
      mEndpoint <- lookupEnv "LLAMACPP_ENDPOINT"

      case (mLangFuse, mEndpoint) of
        (Nothing, _) -> pendingWith "LANGFUSE_PUBLIC_KEY, LANGFUSE_SECRET_KEY not set"
        (_, Nothing) -> pendingWith "LLAMACPP_ENDPOINT not set"
        (Just langfuse, Just endpoint) -> do
          let auth = LlamaCppAuth endpoint

          result <- runRealLLMTest langfuse auth $ do
            let messages = [UserText "Say hello in one sentence!"] :: [Message (Model MinimaxM25 LlamaCpp)]
                configs = [] :: [ModelConfig (Model MinimaxM25 LlamaCpp)]

            info "Making real LLM call to llama.cpp..."
            response <- queryLLM @(Model MinimaxM25 LlamaCpp) configs messages
            info $ fromString $ "LLM responded with " <> show (length response) <> " messages"

            return response

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right response -> length response `shouldSatisfy` (> 0)

    it "groups multiple LLM calls in same session" $ do
      mLangFuse <- runM langFuseFromEnv
      mEndpoint <- lookupEnv "LLAMACPP_ENDPOINT"

      case (mLangFuse, mEndpoint) of
        (Nothing, _) -> pendingWith "LANGFUSE_PUBLIC_KEY, LANGFUSE_SECRET_KEY not set"
        (_, Nothing) -> pendingWith "LLAMACPP_ENDPOINT not set"
        (Just langfuse, Just endpoint) -> do
          let auth = LlamaCppAuth endpoint

          result <- runRealLLMTest langfuse auth $ do
            -- Make multiple LLM calls in the same session
            let messages1 = [UserText "Say hi!"] :: [Message (Model MinimaxM25 LlamaCpp)]
                messages2 = [UserText "Say bye!"] :: [Message (Model MinimaxM25 LlamaCpp)]
                configs = [] :: [ModelConfig (Model MinimaxM25 LlamaCpp)]

            info "Making first LLM call..."
            response1 <- queryLLM @(Model MinimaxM25 LlamaCpp) configs messages1
            info $ fromString $ "First call responded with " <> show (length response1) <> " messages"

            info "Making second LLM call..."
            response2 <- queryLLM @(Model MinimaxM25 LlamaCpp) configs messages2
            info $ fromString $ "Second call responded with " <> show (length response2) <> " messages"

            return (response1, response2)

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right (response1, response2) -> do
              (length response1 `shouldSatisfy` (> 0))
              (length response2 `shouldSatisfy` (> 0))
