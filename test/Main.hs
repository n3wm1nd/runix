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

module Main (main) where

import Test.Hspec
import Polysemy
import Polysemy.Error (runError)
import Polysemy.Fail (runFail)
import qualified Data.ByteString.Lazy as BSL
import Paths_runix (getDataFileName)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import qualified UniversalLLM.Providers.Anthropic as Provider
import UniversalLLM.Protocols.Anthropic (AnthropicRequest, AnthropicResponse)
import Autodocodec (HasCodec)
import Data.Default (Default)

import Runix.LLM.Interpreter (interpretAnthropicOAuth)
import Runix.LLM (LLM, queryLLM)
import Runix.HTTP (HTTP, HTTPStreaming, HTTPResponse(..))
import qualified Runix.HTTP as HTTPEff
import Runix.Logging (Logging, loggingNull)
import Runix.Secret (runSecret)
import Runix.Cancellation (cancelNoop)
import Runix.Streaming (ignoreChunks)
import qualified OpenAIStreamingSpec
import qualified SSEParserSpec
import qualified FileSystemSecuritySpec
import qualified FileSystemSecurityProperties
import qualified FileSystemProjectSpec
import qualified DummyFileSystemSpec
import qualified GrepSecuritySpec
import qualified PathResolutionSpec
import qualified ChrootTranslationSpec

-- ============================================================================
-- Test Models
-- ============================================================================

data ClaudeSonnet45 = ClaudeSonnet45 deriving stock (Show, Eq)

instance ModelName (Model ClaudeSonnet45 Anthropic) where
  modelName _ = "claude-sonnet-4-5-20250929"

instance HasTools (Model ClaudeSonnet45 Anthropic) where
  withTools = Provider.anthropicTools

-- Composable provider for ClaudeSonnet45 with tools
claudeSonnet45ComposableProvider :: ComposableProvider (Model ClaudeSonnet45 Anthropic) (ToolState (Model ClaudeSonnet45 Anthropic), ())
claudeSonnet45ComposableProvider = withTools `chainProviders` Provider.baseComposableProvider

-- ClaudeSonnet45 with reasoning/thinking support for extended thinking tests
data ClaudeSonnet45WithReasoning = ClaudeSonnet45WithReasoning deriving stock (Show, Eq)

instance ModelName (Model ClaudeSonnet45WithReasoning Anthropic) where
  modelName (Model _ _) = "claude-sonnet-4-5-20250929"

instance HasTools (Model ClaudeSonnet45WithReasoning Anthropic) where
  withTools = Provider.anthropicTools

instance HasReasoning (Model ClaudeSonnet45WithReasoning Anthropic) where
  type ReasoningState (Model ClaudeSonnet45WithReasoning Anthropic) = Provider.AnthropicReasoningState
  withReasoning = Provider.anthropicReasoning

-- Composable provider for ClaudeSonnet45WithReasoning with tools and reasoning
claudeSonnet45WithReasoningComposableProvider :: ComposableProvider (Model ClaudeSonnet45WithReasoning Anthropic) (Provider.AnthropicReasoningState, (ToolState (Model ClaudeSonnet45WithReasoning Anthropic), ()))
claudeSonnet45WithReasoningComposableProvider = withReasoning `chainProviders` withTools `chainProviders` Provider.baseComposableProvider

-- ============================================================================
-- Mocked HTTP Effect Provider with cached SSE responses
-- ============================================================================

-- Mock HTTP effect that uses cached SSE responses from test fixtures
-- SSE responses come from real Anthropic API calls recorded in universal-llm tests
mockHTTP :: BSL.ByteString -> Members '[Logging, Embed IO] r => Sem (HTTP ': HTTPStreaming ': r) a -> Sem r a
mockHTTP sseBody = interpret (\case
  HTTPEff.HttpRequestStreaming _req -> do
    -- Streaming: return the cached SSE response
    return $ HTTPResponse
      200
      [("content-type", "text/event-stream")]
      sseBody) . interpret (\case
    HTTPEff.HttpRequest _ -> do
      -- Non-streaming: return success with empty response (not tested in these tests)
      return $ HTTPResponse
        200
        [("content-type", "application/json")]
        "{\"id\":\"mock\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-sonnet-4-5-20250929\",\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":0,\"output_tokens\":0}}")

-- ============================================================================
-- Test Runner
-- ============================================================================

-- Reusable test runner that composes all effect interpreters for testing
-- with mocked HTTP effect provider (generic over model type).
testRunner :: forall model s a. (ModelName model, Default s, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model), Monoid (ProviderRequest model), ProviderRequest model ~ AnthropicRequest, ProviderResponse model ~ AnthropicResponse)
           => ComposableProvider model s
           -> model
           -> BSL.ByteString
           -> (forall r . Members '[LLM model] r => Sem r a)
           -> IO (Either String (Either String a))
testRunner composableProvider model sseBody action =
  runM
    . runError @String
    . runFail
    . loggingNull
    . mockHTTP sseBody
    . runSecret (pure ("mock-token" :: String))
    . cancelNoop
    . ignoreChunks
    . interpretAnthropicOAuth composableProvider model
    $ action

-- ============================================================================
-- Tests
-- ============================================================================

main :: IO ()
main = do
  -- Load SSE test fixtures
  textResponsePath <- getDataFileName "test-fixtures/text-response.sse"
  textResponseBody <- BSL.readFile textResponsePath

  toolCallResponsePath <- getDataFileName "test-fixtures/tool-call-response.sse"
  toolCallResponseBody <- BSL.readFile toolCallResponsePath

  thinkingOnlyPath <- getDataFileName "test-fixtures/thinking-only-response.sse"
  thinkingOnlyBody <- BSL.readFile thinkingOnlyPath

  thinkingWithToolsPath <- getDataFileName "test-fixtures/thinking-with-tools-response.sse"
  thinkingWithToolsBody <- BSL.readFile thinkingWithToolsPath

  -- Run tests with the loaded SSE responses
  hspec $ do
    describe "Dummy FileSystem" DummyFileSystemSpec.spec

    describe "FileSystem Security" FileSystemSecuritySpec.spec

    describe "FileSystem Security Properties" FileSystemSecurityProperties.spec

    describe "Path Resolution (Walking)" PathResolutionSpec.spec

    describe "Chroot Translation Logic" ChrootTranslationSpec.spec

    describe "FileSystem Project Effects" FileSystemProjectSpec.spec

    -- DISABLED: Grep tests scan entire filesystem
    -- describe "Grep Security" GrepSecuritySpec.spec

    describe "SSE Parser Unit Tests" SSEParserSpec.spec

    describe "Runix OpenAI Streaming (Mocked HTTP)" OpenAIStreamingSpec.spec

    describe "Runix Anthropic OAuth Streaming (Mocked HTTP)" $ do

      it "can parse text from SSE streaming response" $ do
        result <- testRunner claudeSonnet45ComposableProvider (Model ClaudeSonnet45 Anthropic) textResponseBody $ do
          let msgs = [UserText "Hello"] :: [Message (Model ClaudeSonnet45 Anthropic)]
              configs = [Streaming True] :: [ModelConfig (Model ClaudeSonnet45 Anthropic)]
          queryLLM configs msgs

        case result of
          Left err -> fail $ "Error effect: " ++ err
          Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
          Right (Right responseMessages) -> do
            -- Should parse the text content from SSE response
            let textMsgs = [txt | AssistantText txt <- responseMessages]
            length textMsgs `shouldSatisfy` (> 0)
            -- The accumulated text should be what was in the fixture
            mconcat textMsgs `shouldBe` "Hello! 👋\n\nI'm Claude Code, Anthropic's official CLI for Claude. I'm here to help you with coding tasks, answer questions, and assist with various text-based work directly from your command line.\n\nHow can I help you today?"

      it "can parse tool calls from SSE streaming response" $ do
        result <- testRunner claudeSonnet45ComposableProvider (Model ClaudeSonnet45 Anthropic) toolCallResponseBody $ do
          let msgs = [UserText "What's the weather in Paris?"] :: [Message (Model ClaudeSonnet45 Anthropic)]
              configs = [Streaming True] :: [ModelConfig (Model ClaudeSonnet45 Anthropic)]
          queryLLM configs msgs

        case result of
          Left err -> fail $ "Error effect: " ++ err
          Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
          Right (Right responseMessages) -> do
            -- Should parse tool use content from SSE response
            let toolCalls = [tc | AssistantTool tc <- responseMessages]
            length toolCalls `shouldSatisfy` (> 0)
            -- Should contain the get_weather tool call
            length toolCalls `shouldBe` 1

            -- Validate the tool call has correct name and arguments
            case toolCalls of
              [ToolCall _ toolName toolArgs] -> do
                toolName `shouldBe` "get_weather"

                -- Validate tool arguments are a JSON object (not a string!)
                case toolArgs of
                  Object obj -> do
                    -- Tool input should have "location" field set to "Paris"
                    case KM.lookup "location" obj of
                      Just (String location) -> location `shouldBe` "Paris"
                      _ -> fail "Tool input should have a string 'location' field"
                  String s -> fail $ "Tool input should be JSON object, not string: " ++ show s
                  _ -> fail $ "Tool input should be JSON object, got: " ++ show toolArgs
              _ -> fail "Expected exactly one tool call"

      it "can parse thinking blocks from SSE streaming response" $ do
        result <- testRunner claudeSonnet45WithReasoningComposableProvider (Model ClaudeSonnet45WithReasoning Anthropic) thinkingOnlyBody $ do
          let msgs = [UserText "Solve this puzzle: What has cities but no houses, forests but no trees, and water but no fish?"] :: [Message (Model ClaudeSonnet45WithReasoning Anthropic)]
              configs = [Streaming True, Reasoning True] :: [ModelConfig (Model ClaudeSonnet45WithReasoning Anthropic)]
          queryLLM configs msgs

        case result of
          Left err -> fail $ "Error effect: " ++ err
          Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
          Right (Right responseMessages) -> do
            -- Should parse thinking content from SSE response
            let thinkingBlocks = [txt | AssistantReasoning txt <- responseMessages]
            length thinkingBlocks `shouldSatisfy` (> 0)

      it "can parse thinking and tool calls from SSE streaming response" $ do
        result <- testRunner claudeSonnet45WithReasoningComposableProvider (Model ClaudeSonnet45WithReasoning Anthropic) thinkingWithToolsBody $ do
          let msgs = [UserText "What's the weather in Paris?"] :: [Message (Model ClaudeSonnet45WithReasoning Anthropic)]
              configs = [Streaming True, Reasoning True] :: [ModelConfig (Model ClaudeSonnet45WithReasoning Anthropic)]
          queryLLM configs msgs

        case result of
          Left err -> fail $ "Error effect: " ++ err
          Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
          Right (Right responseMessages) -> do
            -- Should parse both thinking and tool content from SSE response
            let thinkingBlocks = [txt | AssistantReasoning txt <- responseMessages]
            let toolCalls = [tc | AssistantTool tc <- responseMessages]

            length thinkingBlocks `shouldSatisfy` (> 0)
            length toolCalls `shouldSatisfy` (> 0)

      it "thinking blocks should come before text in message order" $ do
        result <- testRunner claudeSonnet45WithReasoningComposableProvider (Model ClaudeSonnet45WithReasoning Anthropic) thinkingOnlyBody $ do
          let msgs = [UserText "Solve this puzzle: What has cities but no houses, forests but no trees, and water but no fish?"] :: [Message (Model ClaudeSonnet45WithReasoning Anthropic)]
              configs = [Streaming True, Reasoning True] :: [ModelConfig (Model ClaudeSonnet45WithReasoning Anthropic)]
          queryLLM configs msgs

        case result of
          Left err -> fail $ "Error effect: " ++ err
          Right (Left failErr) -> fail $ "Fail effect: " ++ failErr
          Right (Right responseMessages) -> do
            -- Messages are returned oldest-first
            -- Anthropic should return reasoning before text (same as OpenAI)
            case responseMessages of
              [AssistantReasoning _, AssistantText _] ->
                -- Reasoning before text (oldest-first) - correct
                return ()
              [AssistantText _, AssistantReasoning _] ->
                -- Text before reasoning (oldest-first) - wrong
                fail "Messages are in wrong order: Text before Reasoning (should be Reasoning before Text for oldest-first)"
              _ -> fail $ "Unexpected message types or order: " ++ show (map (\case
                                    AssistantText _ -> "Text"
                                    AssistantReasoning _ -> "Reasoning"
                                    _ -> "Other") responseMessages)
