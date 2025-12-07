{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Runix.LLM.Interpreter
  ( -- * Provider interpreters
    interpretAnthropicAPIKey
  , interpretAnthropicOAuth
  , interpretOpenAI
  , interpretOpenRouter
  , interpretLlamaCpp
    -- * Cancellation wrapper
  , withLLMCancellation
    -- * Generic model wrappers
  , GenericModel(..)
    -- * Re-exports from universal-llm
  , module UniversalLLM
  , Anthropic(..)
  , OpenAI(..)
  , OpenRouter(..)
  , LlamaCpp(..)
  ) where

import Polysemy
import Polysemy.Fail
import Polysemy.State (State, evalState, get, put)
import Data.Aeson (toJSON)  -- Import for error response parsing
import Data.ByteString.Lazy (fromStrict)
import Autodocodec (HasCodec, toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Data.Default (Default, def)

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), withMagicSystemPrompt)
import UniversalLLM.Providers.Anthropic as Anthropic (baseComposableProvider)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI

import Runix.LLM.Effects (LLM(..), queryLLM)
import Runix.HTTP.Effects (HTTP, HTTPResponse(..))
import qualified Runix.HTTP.Effects as HTTPEff
import Runix.RestAPI.Effects (RestEndpoint(..), Endpoint(..), post, postStreaming, restapiHTTP)
import Runix.Secret.Effects (Secret, getSecret)
import Runix.Streaming.SSE (reassembleSSE)
import Runix.Cancellation.Effects (onCancellation, Cancellation)
import UniversalLLM.Protocols.Anthropic (AnthropicRequest, AnthropicResponse(..), AnthropicSuccessResponse(..), AnthropicUsage(..), mergeAnthropicDelta, AnthropicContentBlock(..))
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), OpenAIErrorResponse(..), OpenAIErrorDetail(..), mergeOpenAIDelta, defaultOpenAIMessage, defaultOpenAISuccessResponse, defaultOpenAIChoice)

-- ============================================================================
-- Configuration Helper
-- ============================================================================

-- | Get streaming setting from configs using first-match-wins pattern
-- This respects explicit overrides: if Streaming False is specified, it takes precedence
-- over defaultConfigs which may have Streaming True
isStreamingEnabled :: [ModelConfig provider model] -> Bool
isStreamingEnabled configs =
  case [s | Streaming s <- configs] of
    (s:_) -> s  -- First match wins
    [] -> False  -- Default to non-streaming if not specified

-- ============================================================================
-- Generic Model Wrapper
-- ============================================================================

-- Generic model that accepts any model name string
-- Useful for providers where you want to specify the model at runtime
-- Example: GenericModel "deepseek/deepseek-chat-v3-0324:free"
data GenericModel = GenericModel
    { modelId :: Text
    } deriving (Show, Eq)

-- Instances for OpenRouter (uses OpenAI protocol)
instance ModelName OpenRouter GenericModel where
  modelName m = modelId m

-- ============================================================================
-- Anthropic Interpreters
-- ============================================================================

-- Anthropic API Key Authentication
data AnthropicAPIKeyAuth = AnthropicAPIKeyAuth { anthropicApiKey :: String }

instance RestEndpoint AnthropicAPIKeyAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders api =
        [ ("x-api-key", anthropicApiKey api)
        , ("anthropic-version", "2023-06-01")
        , ("Content-Type", "application/json")
        ]

-- Internal version with state management
interpretAnthropicAPIKeyWithState :: forall model provider s r a.
                            ( ModelName provider model
                            , HasCodec (ProviderRequest provider)
                            , HasCodec (ProviderResponse provider)
                            , Monoid (ProviderRequest provider)
                            , ProviderResponse provider ~ AnthropicResponse
                            , Default s
                            , Members '[HTTP, Fail, Secret String, State (provider, model)] r
                            )
                         => ComposableProvider provider model s
                         -> Sem (LLM provider model : r) a
                         -> Sem r a
interpretAnthropicAPIKeyWithState composableProvider action = do
    apiKey <- getSecret
    let api = AnthropicAPIKeyAuth apiKey
        -- We need to thread provider stack state through the computation
        -- Start with empty state for the composable provider
        withRestAPI = reinterpret (\case
            QueryLLM configs messages -> do
                -- Get current provider/model from state
                (provider, model) <- get
                -- Thread composable provider state through
                -- Since () has only one value, we can use it directly
                let (stackState, request) = toProviderRequest composableProvider provider model configs def messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = isStreamingEnabled configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "messages") requestValue
                        let emptyResp = AnthropicSuccessResponse "" "" "assistant" [] Nothing (AnthropicUsage 0 0)
                        return $ reassembleSSE mergeAnthropicDelta (AnthropicSuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "messages") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse Anthropic response: " ++ err
                            Right resp -> return resp

                -- Convert provider response to messages, threading state through
                case fromProviderResponse composableProvider provider model configs stackState providerResponse of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState', resultMessages) -> return resultMessages
            ) action
    restapiHTTP api withRestAPI

-- Public wrapper for backward compatibility
interpretAnthropicAPIKey :: forall model provider s r a.
                            ( ModelName provider model
                            , HasCodec (ProviderRequest provider)
                            , HasCodec (ProviderResponse provider)
                            , Monoid (ProviderRequest provider)
                            , ProviderResponse provider ~ AnthropicResponse
                            , Default s
                            , Members '[HTTP, Fail, Secret String] r
                            )
                         => ComposableProvider provider model s
                         -> provider -- ^ Provider value
                         -> model   -- ^ Model value
                         -> Sem (LLM provider model : r) a
                         -> Sem r a
interpretAnthropicAPIKey composableProvider provider model action =
    evalState (provider, model) . interpretAnthropicAPIKeyWithState composableProvider . raiseUnder $ action

-- Anthropic OAuth Authentication (for Claude Code)
data AnthropicOAuthAuth = AnthropicOAuthAuth { anthropicOAuthToken :: String }

instance RestEndpoint AnthropicOAuthAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders auth =
        [ ("Authorization", "Bearer " <> anthropicOAuthToken auth)
        , ("anthropic-version", "2023-06-01")
        , ("anthropic-beta", "oauth-2025-04-20")
        , ("Content-Type", "application/json")
        , ("User-Agent", "hs-universal-llm (prerelease-dev)")
        ]

-- Internal version with state management
interpretAnthropicOAuthWithState :: forall provider model s r a.
                           ( ModelName provider model
                           , HasCodec (ProviderRequest provider)
                           , HasCodec (ProviderResponse provider)
                           , ProviderRequest provider ~ AnthropicRequest
                           , ProviderResponse provider ~ AnthropicResponse
                           , Default s
                           , Members '[HTTP, Fail, Secret String, State (provider, model)] r
                           )
                        => ComposableProvider provider model s
                        -> Sem (LLM provider model : r) a
                        -> Sem r a
interpretAnthropicOAuthWithState composableProvider action = do
    oauthToken <- getSecret
    let auth = AnthropicOAuthAuth oauthToken
        withRestAPI = reinterpret (\case
            QueryLLM configs messages -> do
                -- Get current provider/model from state
                (provider, model) <- get

                -- Use universal-llm to build the request with magic system prompt
                let (stackState, baseRequest) = toProviderRequest composableProvider provider model configs def messages
                -- Add the magic system prompt for OAuth
                let request = withMagicSystemPrompt baseRequest
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = isStreamingEnabled configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "messages") requestValue
                        -- Check HTTP status code for streaming responses
                        let respCode = Runix.HTTP.Effects.code httpResp
                            respBody = Runix.HTTP.Effects.body httpResp
                        if respCode >= 200 && respCode < 300
                          then do
                            let emptyResp = AnthropicSuccessResponse "" "" "assistant" [] Nothing (AnthropicUsage 0 0)
                            return $ reassembleSSE mergeAnthropicDelta (AnthropicSuccess emptyResp) respBody
                          else do
                            -- For error responses, just return an error without parsing SSE
                            fail $ "HTTP error " ++ show respCode ++ ": " ++ show respBody
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "messages") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse Anthropic response: " ++ err
                            Right resp -> return resp

                -- Convert provider response to messages, threading state through
                case fromProviderResponse composableProvider provider model configs stackState providerResponse of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState', resultMessages) -> return resultMessages
            ) action
    restapiHTTP auth withRestAPI

-- Public wrapper for backward compatibility
interpretAnthropicOAuth :: forall provider model s r a.
                           ( ModelName provider model
                           , HasCodec (ProviderRequest provider)
                           , HasCodec (ProviderResponse provider)
                           , ProviderRequest provider ~ AnthropicRequest
                           , ProviderResponse provider ~ AnthropicResponse
                           , Default s
                           , Members '[HTTP, Fail, Secret String] r
                           )
                        => ComposableProvider provider model s
                        -> provider
                        -> model
                        -> Sem (LLM provider model : r) a
                        -> Sem r a
interpretAnthropicOAuth composableProvider provider model action =
    evalState (provider, model) . interpretAnthropicOAuthWithState composableProvider . raiseUnder $ action

-- ============================================================================
-- OpenAI Interpreter
-- ============================================================================

data OpenAIAuth = OpenAIAuth { openaiApiKey :: String }

instance RestEndpoint OpenAIAuth where
    apiroot _ = "https://api.openai.com/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openaiApiKey api)
        , ("Content-Type", "application/json")
        ]

-- Internal version with state management
interpretOpenAIWithState :: forall model provider s r a.
                   ( ModelName provider model
                   , HasCodec (ProviderRequest provider)
                   , HasCodec (ProviderResponse provider)
                   , Monoid (ProviderRequest provider)
                   , ProviderResponse provider ~ OpenAIResponse
                   , Default s
                   , Members '[HTTP, Fail, Secret String, State (provider, model)] r
                   )
                => ComposableProvider provider model s
                -> Sem (LLM provider model : r) a
                -> Sem r a
interpretOpenAIWithState composableProvider action = do
    apiKey <- getSecret
    let auth = OpenAIAuth apiKey
        withRestAPI = reinterpret (\case
            QueryLLM configs messages -> do
                -- Get current provider/model from state
                (provider, model) <- get

                -- Use universal-llm to build the request
                let (stackState, request) = toProviderRequest composableProvider provider model configs def messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = isStreamingEnabled configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "chat/completions") requestValue
                        let emptyMsg = defaultOpenAIMessage { role = "assistant" }
                        let emptyResp = defaultOpenAISuccessResponse { choices = [defaultOpenAIChoice { message = emptyMsg }] }
                        return $ reassembleSSE mergeOpenAIDelta (OpenAISuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "chat/completions") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse OpenAI response: " ++ err
                            Right resp -> return resp

                -- Convert provider response to messages, threading state through
                -- (Error handling is now done in fromProviderResponse)
                case fromProviderResponse composableProvider provider model configs stackState providerResponse of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState', resultMessages) -> return resultMessages
            ) action
    restapiHTTP auth withRestAPI

-- Public wrapper for backward compatibility
interpretOpenAI :: forall model provider s r a.
                   ( ModelName provider model
                   , HasCodec (ProviderRequest provider)
                   , HasCodec (ProviderResponse provider)
                   , Monoid (ProviderRequest provider)
                   , ProviderResponse provider ~ OpenAIResponse
                   , Default s
                   , Members '[HTTP, Fail, Secret String] r
                   )
                => ComposableProvider provider model s
                -> provider -- ^ Provider value
                -> model   -- ^ Model value
                -> Sem (LLM provider model : r) a
                -> Sem r a
interpretOpenAI composableProvider provider model action =
    evalState (provider, model) . interpretOpenAIWithState composableProvider . raiseUnder $ action

-- ============================================================================
-- OpenRouter Interpreter
-- ============================================================================

-- OpenRouter uses the same API format as OpenAI, but different endpoint
data OpenRouterAuth = OpenRouterAuth { openrouterApiKey :: String }

instance RestEndpoint OpenRouterAuth where
    apiroot _ = "https://openrouter.ai/api/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openrouterApiKey api)
        , ("Content-Type", "application/json")
        ]

-- Internal version with state management
interpretOpenRouterWithState :: forall model provider s r a.
                       ( ModelName provider model
                       , HasCodec (ProviderRequest provider)
                       , HasCodec (ProviderResponse provider)
                       , Monoid (ProviderRequest provider)
                       , ProviderResponse provider ~ OpenAIResponse
                       , Default s
                       , Members '[HTTP, Fail, Secret String, State (provider, model)] r
                       )
                    => ComposableProvider provider model s
                    -> Sem (LLM provider model : r) a
                    -> Sem r a
interpretOpenRouterWithState composableProvider action = do
    apiKey <- getSecret
    let auth = OpenRouterAuth apiKey
        withRestAPI = reinterpret (\case
            QueryLLM configs messages -> do
                -- Get current provider/model from state
                (provider, model) <- get

                -- Use universal-llm to build the request
                let (stackState, request) = toProviderRequest composableProvider provider model configs def messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = isStreamingEnabled configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "chat/completions") requestValue
                        let emptyMsg = defaultOpenAIMessage { role = "assistant" }
                        let emptyResp = defaultOpenAISuccessResponse { choices = [defaultOpenAIChoice { message = emptyMsg }] }
                        return $ reassembleSSE mergeOpenAIDelta (OpenAISuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "chat/completions") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse OpenRouter response: " ++ err
                            Right resp -> return resp

                -- Check for error response before parsing
                -- Convert provider response to messages, threading state through
                -- (Error handling is now done in fromProviderResponse)
                case fromProviderResponse composableProvider provider model configs stackState providerResponse of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState', resultMessages) -> return resultMessages
            ) action
    restapiHTTP auth withRestAPI

-- Public wrapper for backward compatibility
interpretOpenRouter :: forall model provider s r a.
                       ( ModelName provider model
                       , HasCodec (ProviderRequest provider)
                       , HasCodec (ProviderResponse provider)
                       , Monoid (ProviderRequest provider)
                       , ProviderResponse provider ~ OpenAIResponse
                       , Default s
                       , Members '[HTTP, Fail, Secret String] r
                       )
                    => ComposableProvider provider model s
                    -> provider -- ^ Provider value
                    -> model   -- ^ Model value
                    -> Sem (LLM provider model : r) a
                    -> Sem r a
interpretOpenRouter composableProvider provider model action =
    evalState (provider, model) . interpretOpenRouterWithState composableProvider . raiseUnder $ action

-- ============================================================================
-- Llama.cpp Interpreter
-- ============================================================================

-- Llama.cpp uses OpenAI-compatible API but with custom endpoint
data LlamaCppAuth = LlamaCppAuth
    { llamacppEndpoint :: String
    }

instance RestEndpoint LlamaCppAuth where
    apiroot = llamacppEndpoint
    authheaders _ = [("Content-Type", "application/json")]

-- Internal version with state management
interpretLlamaCppWithState :: forall model provider s r a.
                     ( ModelName provider model
                     , HasCodec (ProviderRequest provider)
                     , HasCodec (ProviderResponse provider)
                     , ProviderResponse provider ~ OpenAIResponse
                     , Default s
                     , Members '[HTTP, Fail, State (provider, model)] r, Monoid (ProviderRequest provider)
                     )
                  => ComposableProvider provider model s
                  -> String  -- ^ Endpoint URL (e.g., "http://localhost:8080/v1")
                  -> Sem (LLM provider model : r) a
                  -> Sem r a
interpretLlamaCppWithState composableProvider endpoint action =
    let auth = LlamaCppAuth endpoint
        withRestAPI = reinterpret (\case
            QueryLLM configs messages -> do
                -- Get current provider/model from state
                (provider, model) <- get

                -- Use universal-llm with LlamaCpp (which uses OpenAI protocol)
                let (stackState, request) = toProviderRequest composableProvider provider model configs def messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = isStreamingEnabled configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "chat/completions") requestValue
                        let emptyMsg = defaultOpenAIMessage { role = "assistant" }
                        let emptyResp = defaultOpenAISuccessResponse { choices = [defaultOpenAIChoice { message = emptyMsg }] }
                        return $ reassembleSSE mergeOpenAIDelta (OpenAISuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "chat/completions") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse llama.cpp response: " ++ err
                            Right resp -> return resp

                -- Check for error response before parsing
                -- Convert provider response to messages, threading state through
                -- (Error handling is now done in fromProviderResponse)
                case fromProviderResponse composableProvider provider model configs stackState providerResponse of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState', resultMessages) -> return resultMessages
            ) action
    in restapiHTTP auth withRestAPI

-- Public wrapper for backward compatibility
-- Llama.cpp uses OpenAI protocol internally
interpretLlamaCpp :: forall model provider s r a.
                     ( ModelName provider model
                     , HasCodec (ProviderRequest provider)
                     , HasCodec (ProviderResponse provider)
                     , ProviderResponse provider ~ OpenAIResponse
                     , Default s
                     , Members '[HTTP, Fail] r, Monoid (ProviderRequest provider)
                     )
                  => ComposableProvider provider model s
                  -> String  -- ^ Endpoint URL (e.g., "http://localhost:8080/v1")
                  -> provider
                  -> model   -- ^ Model value
                  -> Sem (LLM provider model : r) a
                  -> Sem r a
interpretLlamaCpp composableProvider endpoint provider model action =
    evalState (provider, model) . interpretLlamaCppWithState composableProvider endpoint . raiseUnder $ action

-- ============================================================================
-- Cancellable Wrapper
-- ============================================================================

-- | Wrapper that adds cancellation support to any LLM interpreter
-- Returns empty list if cancellation is active
withLLMCancellation :: forall provider model r a.
                       Member Cancellation r
                    => Sem (LLM provider model : r) a
                    -> Sem (LLM provider model : r) a
withLLMCancellation action = reinterpret (\case
    QueryLLM configs messages -> onCancellation [] (queryLLM configs messages)
    ) action
