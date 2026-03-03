{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module OpenAIStreamingSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error (runError)
import Polysemy.Fail (Fail, runFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Paths_runix (getDataFileName)
import Data.Default (Default)

import UniversalLLM
import UniversalLLM.Providers.OpenAI (OpenAI(..))
import qualified UniversalLLM.Providers.OpenAI as Provider
import UniversalLLM.Protocols.OpenAI (OpenAIRequest, OpenAIResponse)
import Autodocodec (HasCodec)

import Runix.LLM.Interpreter (OpenAIAuth(..), ProviderProtocol, EnableStreaming, ProtocolRequest)
import Runix.LLM.Interpreter (interpretLLMStreamingWith, queryStreamingLLM)
import Runix.LLM (LLM, queryLLM)
import Runix.LLMStream (LLMStreaming, LLMStreamResult, StreamEvent(..))
import Runix.Streaming (fetchNext)
import Runix.HTTP (HTTP, HTTPRequest, HTTPStreaming, HTTPStreamResult(..), HTTPResponse(..))
import qualified Runix.HTTP as HTTPEff
import Runix.Logging (Logging, loggingNull)
import Runix.Cancellation (cancelNoop)
import Runix.StreamChunk (ignoreChunks)
import Runix.Streaming (interpretStreamingStateful)
import UniversalLLM.Providers.XMLToolCalls (xmlResponseParser)

-- ============================================================================
-- Test Models
-- ============================================================================

-- GLM4.5 model supporting tools and reasoning (via OpenAI protocol)
data GLM45 = GLM45 deriving stock (Show, Eq)

instance Provider (Model GLM45 OpenAI) where
  type ProviderRequest (Model GLM45 OpenAI) = OpenAIRequest
  type ProviderResponse (Model GLM45 OpenAI) = OpenAIResponse

instance ModelName (Model GLM45 OpenAI) where
  modelName (Model _ _) = "glm-4-plus"

instance HasTools (Model GLM45 OpenAI) where
  withTools = Provider.openAITools

instance HasReasoning (Model GLM45 OpenAI) where
  withReasoning = Provider.openAIReasoning

instance BaseComposableProvider (Model GLM45 OpenAI) where
  baseProvider = Provider.baseComposableProvider

-- Composable provider for GLM45: with tools, reasoning, and XML response parsing
glm45ComposableProvider :: ComposableProvider (Model GLM45 OpenAI) ((), ((), ((), ())))
glm45ComposableProvider = xmlResponseParser `chainProviders` withReasoning `chainProviders` withTools `chainProviders` baseProvider

-- GLM45 with tools but no reasoning (simplified version for text-only test)
data GLM45TextOnly = GLM45TextOnly deriving stock (Show, Eq)

instance Provider (Model GLM45TextOnly OpenAI) where
  type ProviderRequest (Model GLM45TextOnly OpenAI) = OpenAIRequest
  type ProviderResponse (Model GLM45TextOnly OpenAI) = OpenAIResponse

instance ModelName (Model GLM45TextOnly OpenAI) where
  modelName (Model _ _) = "glm-4-plus"

instance BaseComposableProvider (Model GLM45TextOnly OpenAI) where
  baseProvider = Provider.baseComposableProvider

-- Composable provider for GLM45TextOnly: base provider only
glm45TextOnlyComposableProvider :: ComposableProvider (Model GLM45TextOnly OpenAI) ()
glm45TextOnlyComposableProvider = baseProvider

-- ============================================================================
-- Mocked HTTP Effect Provider with cached SSE responses
-- ============================================================================

-- Mock HTTP effect that uses cached SSE responses from test fixtures
mockHTTP :: forall r a. BSL.ByteString -> Members '[Logging, Embed IO, Fail] r => Sem (HTTP ': HTTPStreaming ': r) a -> Sem r a
mockHTTP sseBody =
    -- First interpret HTTPStreaming (which is Streaming BS.ByteString HTTPStreamResult HTTPRequest)
    interpretStreamingStateful onStart onFetch onClose
    -- Then interpret HTTP (non-streaming)
    . interpret @HTTP (\case
        HTTPEff.HttpRequest _ -> do
          -- Non-streaming: return success with empty response (not tested in these tests)
          return $ Right $ HTTPResponse
            200
            [("content-type", "application/json")]
            "{\"id\":\"mock\",\"object\":\"chat.completion\",\"created\":1234567890,\"model\":\"glm-4-plus\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":null},\"finish_reason\":null}],\"usage\":{\"prompt_tokens\":0,\"completion_tokens\":0,\"total_tokens\":0}}")
  where
    -- Convert lazy ByteString to chunks of strict ByteStrings
    chunks :: [BS.ByteString]
    chunks = BSL.toChunks sseBody

    -- Stream state: just the list of remaining chunks
    onStart :: HTTPRequest -> Sem r (Either String [BS.ByteString])
    onStart _req = return $ Right chunks

    -- Fetch next chunk
    onFetch :: [BS.ByteString] -> Sem r (Maybe BS.ByteString, [BS.ByteString])
    onFetch [] = return (Nothing, [])
    onFetch (c:cs) = return (Just c, cs)

    -- Close: return success result
    onClose :: [BS.ByteString] -> Sem r HTTPStreamResult
    onClose _ = return $ HTTPStreamResult 200 [] BSL.empty

-- ============================================================================
-- Test Runner
-- ============================================================================

-- Reusable test runner that composes all effect interpreters for testing
testRunner :: forall model s a.
              ( ModelName model
              , Default s
              , HasCodec (ProviderRequest model)
              , Monoid (ProviderRequest model)
              , EnableStreaming (ProviderResponse model)
              , ProtocolRequest (ProviderResponse model) ~ ProviderRequest model
              , HasStreaming model
              , ProviderResponse model ~ OpenAIResponse
              )
           => ComposableProvider model s
           -> model
           -> BSL.ByteString
           -> (forall r . Members '[LLMStreaming model, Fail] r => Sem r a)
           -> IO (Either String (Either String a))
testRunner composableProvider model sseBody action =
  runM
    . runError @String
    . runFail
    . loggingNull
    . mockHTTP sseBody
    . cancelNoop
    . ignoreChunks @BS.ByteString
    . interpretLLMStreamingWith (OpenAIAuth "mock-api-key") composableProvider model []
    $ action

-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec
spec = do
  -- Load SSE test fixtures
  textResponsePath <- runIO $ getDataFileName "test-fixtures/openai-text-response.sse"
  textResponseBody <- runIO $ BSL.readFile textResponsePath

  toolCallResponsePath <- runIO $ getDataFileName "test-fixtures/openai-tool-call-response.sse"
  toolCallResponseBody <- runIO $ BSL.readFile toolCallResponsePath

  reasoningOnlyPath <- runIO $ getDataFileName "test-fixtures/openai-reasoning-response.sse"
  reasoningOnlyBody <- runIO $ BSL.readFile reasoningOnlyPath

  reasoningWithToolsPath <- runIO $ getDataFileName "test-fixtures/openai-reasoning-with-tools-response.sse"
  reasoningWithToolsBody <- runIO $ BSL.readFile reasoningWithToolsPath

  -- Run tests with the loaded SSE responses
  describe "Runix OpenAI Streaming (Mocked HTTP)" $ do

    it "can parse text from SSE streaming response" $ do
      result <- testRunner glm45TextOnlyComposableProvider (Model GLM45TextOnly OpenAI) textResponseBody $ do
        let msgs = [UserText "Say hello"] :: [Message (Model GLM45TextOnly OpenAI)]
            configs = [] :: [ModelConfig (Model GLM45TextOnly OpenAI)]
        queryStreamingLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Should parse the text content from SSE response
          let textMsgs = [txt | AssistantText txt <- responseMessages]
          length textMsgs `shouldSatisfy` (> 0)

    it "can parse tool calls from SSE streaming response" $ do
      result <- testRunner glm45ComposableProvider (Model GLM45 OpenAI) toolCallResponseBody $ do
        let msgs = [UserText "What's the weather in Paris?"] :: [Message (Model GLM45 OpenAI)]
            configs = [] :: [ModelConfig (Model GLM45 OpenAI)]
        queryStreamingLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Should parse XML tool calls from SSE response
          let toolCalls = [tc | AssistantTool tc <- responseMessages]
          let reasoningBlocks = [txt | AssistantReasoning txt <- responseMessages]
          -- Should have tool calls extracted from XML
          length toolCalls `shouldSatisfy` (> 0)

    it "can parse reasoning blocks from SSE streaming response" $ do
      result <- testRunner glm45ComposableProvider (Model GLM45 OpenAI) reasoningOnlyBody $ do
        let msgs = [UserText "Solve this puzzle: What has cities but no houses, forests but no trees, and water but no fish?"] :: [Message (Model GLM45 OpenAI)]
            configs = [Reasoning True] :: [ModelConfig (Model GLM45 OpenAI)]
        queryStreamingLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Should parse some content from SSE response without errors
          length responseMessages `shouldSatisfy` (> 0)

    it "can parse reasoning and tool calls from SSE streaming response" $ do
      result <- testRunner glm45ComposableProvider (Model GLM45 OpenAI) reasoningWithToolsBody $ do
        let msgs = [UserText "What's the weather in Paris?"] :: [Message (Model GLM45 OpenAI)]
            configs = [Reasoning True] :: [ModelConfig (Model GLM45 OpenAI)]
        queryStreamingLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Should parse both reasoning and XML tool calls from SSE response
          let toolCalls = [tc | AssistantTool tc <- responseMessages]
          let reasoningBlocks = [txt | AssistantReasoning txt <- responseMessages]
          -- Both should be present in the combined response
          length toolCalls `shouldSatisfy` (> 0)
          length reasoningBlocks `shouldSatisfy` (> 0)

    it "reasoning blocks should come before text in message order" $ do
      result <- testRunner glm45ComposableProvider (Model GLM45 OpenAI) reasoningOnlyBody $ do
        let msgs = [UserText "Solve this puzzle: What has cities but no houses, forests but no trees, and water but no fish?"] :: [Message (Model GLM45 OpenAI)]
            configs = [Reasoning True] :: [ModelConfig (Model GLM45 OpenAI)]
        queryStreamingLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Messages are returned oldest-first
          -- Check what we actually get
          case responseMessages of
            [AssistantReasoning _, AssistantText _] ->
              -- Reasoning before text (oldest-first)
              return ()
            [AssistantText _, AssistantReasoning _] ->
              -- Text before reasoning (oldest-first)
              fail "Messages are in wrong order: Text before Reasoning (should be Reasoning before Text for oldest-first)"
            _ -> fail $ "Unexpected message types or order: " ++ show (map (\case
                                  AssistantText _ -> "Text"
                                  AssistantReasoning _ -> "Reasoning"
                                  _ -> "Other") responseMessages)
