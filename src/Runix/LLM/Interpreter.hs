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
import Data.Aeson ()  -- Import only instances
import Autodocodec (HasCodec, toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), withMagicSystemPrompt)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI

import Runix.LLM.Effects (LLM(..), getModel, queryLLM)
import Runix.HTTP.Effects (HTTP, HTTPResponse(..))
import Runix.RestAPI.Effects (RestEndpoint(..), Endpoint(..), post, postStreaming, restapiHTTP)
import Runix.Secret.Effects (Secret, getSecret)
import Runix.Streaming.SSE (reassembleSSE)
import Runix.Cancellation.Effects (onCancellation, Cancellation)
import UniversalLLM.Protocols.Anthropic (AnthropicRequest, AnthropicResponse(..), AnthropicSuccessResponse(..), AnthropicUsage(..), mergeAnthropicDelta, AnthropicContentBlock(..))
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), OpenAIErrorResponse(..), OpenAIErrorDetail(..), mergeOpenAIDelta)

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

instance ProviderImplementation OpenRouter GenericModel where
  getComposableProvider = OpenAI.baseComposableProvider

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

interpretAnthropicAPIKey :: forall model provider r a.
                            ( ProviderImplementation provider model
                            , ModelName provider model
                            , HasCodec (ProviderRequest provider)
                            , HasCodec (ProviderResponse provider)
                            , Monoid (ProviderRequest provider)
                            , ProviderResponse provider ~ AnthropicResponse
                            , Members '[HTTP, Fail, Secret String] r
                            )
                         => provider -- ^ Provider value
                         -> model   -- ^ Default model
                         -> Sem (LLM provider model : r) a
                         -> Sem r a
interpretAnthropicAPIKey provider defaultModel action = do
    apiKey <- getSecret
    let api = AnthropicAPIKeyAuth apiKey
        withRestAPI = reinterpret (\case
            GetModel -> return defaultModel

            QueryLLM configs messages -> do
                -- Use universal-llm to build the request
                let request = toProviderRequest provider defaultModel configs messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = any (\case { Streaming True -> True; _ -> False }) configs

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

                -- Convert provider response to messages
                let resultMessages = fromProviderResponse provider defaultModel configs messages providerResponse
                return resultMessages
            ) action
    restapiHTTP api withRestAPI

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

interpretAnthropicOAuth :: forall provider model r a.
                           ( ProviderImplementation provider model
                           , ModelName provider model
                           , HasCodec (ProviderRequest provider)
                           , HasCodec (ProviderResponse provider)
                           , ProviderRequest provider ~ AnthropicRequest
                           , ProviderResponse provider ~ AnthropicResponse
                           , Members '[HTTP, Fail, Secret String] r
                           )
                        =>
                        provider
                        -> model   -- ^ Default model
                        -> Sem (LLM provider model : r) a
                        -> Sem r a
interpretAnthropicOAuth provider defaultModel action = do
    oauthToken <- getSecret
    let auth = AnthropicOAuthAuth oauthToken
        withRestAPI = reinterpret (\case
            GetModel -> return defaultModel

            QueryLLM configs messages -> do
                -- Use universal-llm to build the request with magic system prompt
                let baseRequest = toProviderRequest provider defaultModel configs messages
                -- Add the magic system prompt for OAuth
                let request = withMagicSystemPrompt baseRequest
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = any (\case { Streaming True -> True; _ -> False }) configs

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

                -- Convert provider response to messages
                let resultMessages = fromProviderResponse provider defaultModel configs messages providerResponse
                return resultMessages
            ) action
    restapiHTTP auth withRestAPI

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

interpretOpenAI :: forall model provider r a.
                   ( ProviderImplementation provider model
                   , ModelName provider model
                   , HasCodec (ProviderRequest provider)
                   , HasCodec (ProviderResponse provider)
                   , Monoid (ProviderRequest provider)
                   , ProviderResponse provider ~ OpenAIResponse
                   , Members '[HTTP, Fail, Secret String] r
                   )
                => provider -- ^ Provider value
                -> model   -- ^ Default model
                -> Sem (LLM provider model : r) a
                -> Sem r a
interpretOpenAI provider defaultModel action = do
    apiKey <- getSecret
    let auth = OpenAIAuth apiKey
        withRestAPI = reinterpret (\case
            GetModel -> return defaultModel

            QueryLLM configs messages -> do
                -- Use universal-llm to build the request
                let request = toProviderRequest provider defaultModel configs messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = any (\case { Streaming True -> True; _ -> False }) configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "chat/completions") requestValue
                        let emptyMsg = OpenAIMessage "assistant" Nothing Nothing Nothing Nothing
                        let emptyResp = OpenAISuccessResponse [OpenAIChoice emptyMsg]
                        return $ reassembleSSE mergeOpenAIDelta (OpenAISuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "chat/completions") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse OpenAI response: " ++ err
                            Right resp -> return resp

                -- Check for error response before parsing
                case providerResponse of
                    OpenAIError (OpenAIErrorResponse errDetail) ->
                        fail $ "OpenAI API error: " ++ show (errorMessage errDetail)
                    OpenAISuccess _ -> do
                        -- Convert provider response to messages
                        let resultMessages = fromProviderResponse provider defaultModel configs messages providerResponse
                        return resultMessages
            ) action
    restapiHTTP auth withRestAPI

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

interpretOpenRouter :: forall model provider r a.
                       ( ProviderImplementation provider model
                       , ModelName provider model
                       , HasCodec (ProviderRequest provider)
                       , HasCodec (ProviderResponse provider)
                       , Monoid (ProviderRequest provider)
                       , ProviderResponse provider ~ OpenAIResponse
                       , Members '[HTTP, Fail, Secret String] r
                       )
                    => provider -- ^ Provider value
                    -> model   -- ^ Default model
                    -> Sem (LLM provider model : r) a
                    -> Sem r a
interpretOpenRouter provider defaultModel action = do
    apiKey <- getSecret
    let auth = OpenRouterAuth apiKey
        withRestAPI = reinterpret (\case
            GetModel -> return defaultModel

            QueryLLM configs messages -> do
                -- Use universal-llm to build the request
                let request = toProviderRequest provider defaultModel configs messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = any (\case { Streaming True -> True; _ -> False }) configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "chat/completions") requestValue
                        let emptyMsg = OpenAIMessage "assistant" Nothing Nothing Nothing Nothing
                        let emptyResp = OpenAISuccessResponse [OpenAIChoice emptyMsg]
                        return $ reassembleSSE mergeOpenAIDelta (OpenAISuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "chat/completions") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse OpenRouter response: " ++ err
                            Right resp -> return resp

                -- Check for error response before parsing
                case providerResponse of
                    OpenAIError (OpenAIErrorResponse errDetail) ->
                        fail $ "OpenRouter API error: " ++ show (errorMessage errDetail)
                    OpenAISuccess _ -> do
                        -- Convert provider response to messages
                        let resultMessages = fromProviderResponse provider defaultModel configs messages providerResponse
                        return resultMessages
            ) action
    restapiHTTP auth withRestAPI

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

-- Llama.cpp uses OpenAI protocol internally
interpretLlamaCpp :: forall model provider r a.
                     ( ProviderImplementation provider model
                     , ModelName provider model
                     , HasCodec (ProviderRequest provider)
                     , HasCodec (ProviderResponse provider)
                     , ProviderResponse provider ~ OpenAIResponse
                     , Members '[HTTP, Fail] r, Monoid (ProviderRequest provider)
                     )
                  => String  -- ^ Endpoint URL (e.g., "http://localhost:8080/v1")
                  -> provider
                  -> model   -- ^ Default model
                  -> Sem (LLM provider model : r) a
                  -> Sem r a
interpretLlamaCpp endpoint p defaultModel action =
    let auth = LlamaCppAuth endpoint
        withRestAPI = reinterpret (\case
            GetModel -> return defaultModel

            QueryLLM configs messages -> do
                -- Use universal-llm with LlamaCpp (which uses OpenAI protocol)
                let request = toProviderRequest p defaultModel configs messages
                let requestValue = toJSONViaCodec request

                -- Check if streaming is enabled
                let useStreaming = any (\case { Streaming True -> True; _ -> False }) configs

                -- Make the API call and get typed response
                providerResponse <- if useStreaming
                    then do
                        -- Streaming: reassemble SSE into typed response
                        httpResp <- postStreaming (Endpoint "chat/completions") requestValue
                        let emptyMsg = OpenAIMessage "assistant" Nothing Nothing Nothing Nothing
                        let emptyResp = OpenAISuccessResponse [OpenAIChoice emptyMsg]
                        return $ reassembleSSE mergeOpenAIDelta (OpenAISuccess emptyResp) (body httpResp)
                    else do
                        -- Non-streaming: parse JSON to typed response
                        responseValue <- post (Endpoint "chat/completions") requestValue
                        case parseEither parseJSONViaCodec responseValue of
                            Left err -> fail $ "Failed to parse llama.cpp response: " ++ err
                            Right resp -> return resp

                -- Check for error response before parsing
                case providerResponse of
                    OpenAIError (OpenAIErrorResponse errDetail) ->
                        fail $ "llama.cpp API error: " ++ show (errorMessage errDetail)
                    OpenAISuccess _ -> do
                        -- Convert provider response to messages
                        let resultMessages = fromProviderResponse p defaultModel configs messages providerResponse
                        return resultMessages
            ) action
    in restapiHTTP auth withRestAPI

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
    GetModel -> getModel
    QueryLLM configs messages -> onCancellation [] (queryLLM configs messages)
    ) action
