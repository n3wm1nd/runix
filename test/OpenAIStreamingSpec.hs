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

module OpenAIStreamingSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error (runError)
import Polysemy.Fail (runFail)
import qualified Data.ByteString.Lazy as BSL
import Paths_runix (getDataFileName)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Default (Default)

import UniversalLLM
import UniversalLLM.Providers.OpenAI (OpenAI(..))
import qualified UniversalLLM.Providers.OpenAI as Provider
import qualified UniversalLLM.Providers.XMLToolCalls as XMLTools
import UniversalLLM.Core.Types (chainProviders)

import Runix.LLM.Interpreter (interpretOpenAI)
import Runix.LLM.Effects (LLM, queryLLM)
import Runix.HTTP.Effects (HTTP, HTTPResponse(..))
import qualified Runix.HTTP.Effects as HTTPEff
import Runix.Logging.Effects (Logging, loggingNull)
import Runix.Secret.Effects (runSecret, Secret)
import Runix.Cancellation.Effects (cancelNoop)
import Runix.Streaming.Effects (ignoreChunks)

-- ============================================================================
-- Test Models
-- ============================================================================

-- GLM4.5 model supporting tools and reasoning (via OpenAI protocol)
data GLM45 = GLM45 deriving stock (Show, Eq)

instance ModelName OpenAI GLM45 where
  modelName _ = "glm-4-plus"

instance HasTools GLM45 OpenAI where
  withTools = chainProviders Provider.openAITools

instance HasReasoning GLM45 OpenAI where
  withReasoning = chainProviders Provider.openAIReasoning

-- Composable provider for GLM45: with tools, reasoning, and XML response parsing
glm45ComposableProvider :: ComposableProvider OpenAI GLM45 ((), ((), ((), ())))
glm45ComposableProvider = XMLTools.withXMLResponseParsing . withReasoning . withTools $ Provider.baseComposableProvider @OpenAI @GLM45

-- GLM45 with tools but no reasoning (simplified version for text-only test)
data GLM45TextOnly = GLM45TextOnly deriving stock (Show, Eq)

instance ModelName OpenAI GLM45TextOnly where
  modelName _ = "glm-4-plus"

-- Composable provider for GLM45TextOnly: base provider only
glm45TextOnlyComposableProvider :: ComposableProvider OpenAI GLM45TextOnly ()
glm45TextOnlyComposableProvider = Provider.baseComposableProvider

-- ============================================================================
-- Mocked HTTP Effect Provider with cached SSE responses
-- ============================================================================

-- Mock HTTP effect that uses cached SSE responses from test fixtures
mockHTTP :: BSL.ByteString -> Members '[Logging, Embed IO] r => Sem (HTTP ': r) a -> Sem r a
mockHTTP sseBody = interpret $ \case
  HTTPEff.HttpRequest _ -> do
    -- Non-streaming: return success with empty response (not tested in these tests)
    return $ HTTPResponse
      200
      [("content-type", "application/json")]
      "{\"id\":\"mock\",\"object\":\"chat.completion\",\"created\":1234567890,\"model\":\"glm-4-plus\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":null},\"finish_reason\":null}],\"usage\":{\"prompt_tokens\":0,\"completion_tokens\":0,\"total_tokens\":0}}"

  HTTPEff.HttpRequestStreaming _req -> do
    -- Streaming: return the cached SSE response
    return $ HTTPResponse
      200
      [("content-type", "text/event-stream")]
      sseBody

-- ============================================================================
-- Test Runner
-- ============================================================================

-- Reusable test runner that composes all effect interpreters for testing
testRunner :: forall model s a. (ModelName OpenAI model, Default s)
           => ComposableProvider OpenAI model s
           -> model
           -> BSL.ByteString
           -> (forall r . Members '[LLM OpenAI model] r => Sem r a)
           -> IO (Either String (Either String a))
testRunner composableProvider model sseBody action =
  runM
    . runError @String
    . runFail
    . loggingNull
    . mockHTTP sseBody
    . runSecret (pure ("mock-api-key" :: String))
    . cancelNoop
    . ignoreChunks
    . interpretOpenAI composableProvider OpenAI model
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
      result <- testRunner glm45TextOnlyComposableProvider GLM45TextOnly textResponseBody $ do
        let msgs = [UserText "Say hello"] :: [Message GLM45TextOnly OpenAI]
            configs = [Streaming True] :: [ModelConfig OpenAI GLM45TextOnly]
        queryLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Should parse the text content from SSE response
          let textMsgs = [txt | AssistantText txt <- responseMessages]
          length textMsgs `shouldSatisfy` (> 0)

    it "can parse tool calls from SSE streaming response" $ do
      result <- testRunner glm45ComposableProvider GLM45 toolCallResponseBody $ do
        let msgs = [UserText "What's the weather in Paris?"] :: [Message GLM45 OpenAI]
            configs = [Streaming True] :: [ModelConfig OpenAI GLM45]
        queryLLM configs msgs

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
      result <- testRunner glm45ComposableProvider GLM45 reasoningOnlyBody $ do
        let msgs = [UserText "Solve this puzzle: What has cities but no houses, forests but no trees, and water but no fish?"] :: [Message GLM45 OpenAI]
            configs = [Streaming True, Reasoning True] :: [ModelConfig OpenAI GLM45]
        queryLLM configs msgs

      case result of
        Left err -> fail $ "Error effect: " ++ err
        Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
        Right (Right responseMessages) -> do
          -- Should parse some content from SSE response without errors
          length responseMessages `shouldSatisfy` (> 0)

    it "can parse reasoning and tool calls from SSE streaming response" $ do
      result <- testRunner glm45ComposableProvider GLM45 reasoningWithToolsBody $ do
        let msgs = [UserText "What's the weather in Paris?"] :: [Message GLM45 OpenAI]
            configs = [Streaming True, Reasoning True] :: [ModelConfig OpenAI GLM45]
        queryLLM configs msgs

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
      result <- testRunner glm45ComposableProvider GLM45 reasoningOnlyBody $ do
        let msgs = [UserText "Solve this puzzle: What has cities but no houses, forests but no trees, and water but no fish?"] :: [Message GLM45 OpenAI]
            configs = [Streaming True, Reasoning True] :: [ModelConfig OpenAI GLM45]
        queryLLM configs msgs

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
