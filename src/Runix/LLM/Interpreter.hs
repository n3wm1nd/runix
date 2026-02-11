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
  , interpretZAI
    -- * Unified interpreter
  , interpretLLM
    -- * Protocol typeclass
  , ProviderProtocol(..)
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
import Autodocodec (HasCodec, toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson (Value)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default (Default, def)

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), oauthHeaders)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))

import Runix.LLM (LLM(..), queryLLM)
import Runix.HTTP (HTTP, HTTPStreaming, HTTPResponse(..))
import Runix.RestAPI (RestEndpoint(..), Endpoint(..), post, postStreaming, restapiHTTP, restapiHTTPStreaming, RestAPI, RestAPIStreaming)
import Runix.Secret (Secret, getSecret)
import Runix.Streaming (StreamChunk)
import Runix.Streaming.SSE (reassembleSSE)
import qualified Data.ByteString as BS
import Runix.Cancellation (Cancellation, onCancellation)
import UniversalLLM.Protocols.Anthropic (AnthropicResponse(..), AnthropicSuccessResponse(..), AnthropicUsage(..), mergeAnthropicDelta)
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), mergeOpenAIDelta, defaultOpenAIMessage, defaultOpenAISuccessResponse, defaultOpenAIChoice)

-- ============================================================================
-- Configuration Helper
-- ============================================================================

-- | Get streaming setting from configs using first-match-wins pattern
-- This respects explicit overrides: if Streaming False is specified, it takes precedence
-- over defaultConfigs which may have Streaming True
isStreamingEnabled :: [ModelConfig model] -> Bool
isStreamingEnabled configs =
  case [s | Streaming s <- configs] of
    (s:_) -> s  -- First match wins
    [] -> False  -- Default to non-streaming if not specified

-- ============================================================================
-- Protocol Typeclass
-- ============================================================================

-- | Typeclass for provider protocols (Anthropic vs OpenAI-compatible)
-- Abstracts over the protocol-specific details like endpoint path and streaming handling
class ProviderProtocol response where
  -- | The endpoint path for this protocol
  protocolEndpoint :: Endpoint
  -- | Create an empty response for streaming accumulation
  emptyStreamingResponse :: response
  -- | Merge a streaming delta (as JSON Value) into the accumulated response
  mergeStreamingDelta :: response -> Value -> response

instance ProviderProtocol AnthropicResponse where
  protocolEndpoint = Endpoint "messages"
  emptyStreamingResponse = AnthropicSuccess $
    AnthropicSuccessResponse "" "" "assistant" [] Nothing (AnthropicUsage 0 0)
  mergeStreamingDelta = mergeAnthropicDelta

instance ProviderProtocol OpenAIResponse where
  protocolEndpoint = Endpoint "chat/completions"
  emptyStreamingResponse = OpenAISuccess $
    defaultOpenAISuccessResponse
      { choices = [defaultOpenAIChoice { message = defaultOpenAIMessage { role = "assistant" } }] }
  mergeStreamingDelta = mergeOpenAIDelta

-- ============================================================================
-- Unified Request Helpers
-- ============================================================================

-- | Send a non-streaming request to the provider
sendRequest :: forall p response r.
               ( ProviderProtocol response
               , HasCodec response
               , Members '[RestAPI p, Fail] r
               )
            => Value -> Sem r response
sendRequest requestValue = do
    responseValue <- post (protocolEndpoint @response) requestValue
    case parseEither parseJSONViaCodec responseValue of
        Left err -> fail $ "Failed to parse response: " ++ err
        Right resp -> return resp

-- | Send a streaming request to the provider
sendRequestStreaming :: forall p response r.
                        ( ProviderProtocol response
                        , Members '[RestAPIStreaming p] r
                        )
                     => Value -> Sem r response
sendRequestStreaming requestValue = do
    httpResp <- postStreaming (protocolEndpoint @response) requestValue
    return $ reassembleSSE (mergeStreamingDelta @response)
                           (emptyStreamingResponse @response)
                           (body httpResp)

-- ============================================================================
-- Unified LLM Interpreter
-- ============================================================================

-- | Internal interpreter with explicit state in effect row
interpretLLMWithState :: forall p model s r a.
                         ( ModelName model
                         , HasCodec (ProviderRequest model)
                         , HasCodec (ProviderResponse model)
                         , Monoid (ProviderRequest model)
                         , ProviderProtocol (ProviderResponse model)
                         , Members '[RestAPI p, RestAPIStreaming p, Fail, State (model, s)] r
                         )
                      => ComposableProvider model s
                      -> Sem (LLM model : r) a
                      -> Sem r a
interpretLLMWithState composableProvider = interpret $ \case
    QueryLLM configs messages -> do
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
        let requestValue = toJSONViaCodec request
        let useStreaming = isStreamingEnabled configs

        providerResponse <- if useStreaming
            then sendRequestStreaming @p requestValue
            else sendRequest @p requestValue

        case fromProviderResponse composableProvider m configs stackState' providerResponse of
            Left err -> fail $ "LLM error: " ++ show err
            Right (stackState'', resultMessages) -> do
                put (m, stackState'')
                return resultMessages

-- | Unified LLM interpreter that works with any provider protocol
-- Interprets LLM effect using RestAPI and RestAPIStreaming effects
-- The caller is responsible for setting up the RestAPI stack with appropriate auth
interpretLLM :: forall p model s r a.
                ( ModelName model
                , HasCodec (ProviderRequest model)
                , HasCodec (ProviderResponse model)
                , Monoid (ProviderRequest model)
                , ProviderProtocol (ProviderResponse model)
                , Default s
                , Members '[RestAPI p, RestAPIStreaming p, Fail] r
                )
             => ComposableProvider model s
             -> model
             -> Sem (LLM model : r) a
             -> Sem r a
interpretLLM composableProvider model action =
    evalState (model, def @s) $
    interpretLLMWithState @p composableProvider $
    raiseUnder action

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
instance ModelName (Model GenericModel OpenRouter) where
  modelName (Model m _) = modelId m

-- ============================================================================
-- Anthropic Auth Configurations
-- ============================================================================

-- Anthropic API Key Authentication
data AnthropicAPIKeyAuth = AnthropicAPIKeyAuth { anthropicApiKey :: String }

instance RestEndpoint AnthropicAPIKeyAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders api =
        [ ("x-api-key", anthropicApiKey api)
        , ("anthropic-version", "2023-06-01")
        ]

-- Anthropic OAuth Authentication (for Claude Code)
data AnthropicOAuthAuth = AnthropicOAuthAuth { anthropicOAuthToken :: String }

instance RestEndpoint AnthropicOAuthAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders auth = map (\(a, b) -> (T.unpack a, T.unpack b)) $ oauthHeaders (T.pack $ anthropicOAuthToken auth)

-- ============================================================================
-- Anthropic Interpreters
-- ============================================================================

interpretAnthropicAPIKey :: forall model s r a.
                            ( ModelName model
                            , HasCodec (ProviderRequest model)
                            , HasCodec (ProviderResponse model)
                            , Monoid (ProviderRequest model)
                            , ProviderResponse model ~ AnthropicResponse
                            , Default s
                            , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail, Secret String] r
                            )
                         => ComposableProvider model s
                         -> model
                         -> Sem (LLM model : r) a
                         -> Sem r a
interpretAnthropicAPIKey composableProvider model action = do
    apiKey <- getSecret
    let api = AnthropicAPIKeyAuth apiKey
    restapiHTTP api $
      restapiHTTPStreaming api $
      interpretLLM @AnthropicAPIKeyAuth composableProvider model $
      raiseUnder @(RestAPIStreaming AnthropicAPIKeyAuth) $
      raiseUnder @(RestAPI AnthropicAPIKeyAuth)
      action

interpretAnthropicOAuth :: forall model s r a.
                           ( ModelName model
                           , HasCodec (ProviderRequest model)
                           , HasCodec (ProviderResponse model)
                           , Monoid (ProviderRequest model)
                           , ProviderResponse model ~ AnthropicResponse
                           , Default s
                           , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail, Secret String] r
                           )
                        => ComposableProvider model s
                        -> model
                        -> Sem (LLM model : r) a
                        -> Sem r a
interpretAnthropicOAuth composableProvider model action = do
    oauthToken <- getSecret
    let api = AnthropicOAuthAuth oauthToken
    restapiHTTP api $
      restapiHTTPStreaming api $
      interpretLLM @AnthropicOAuthAuth composableProvider model $
      raiseUnder @(RestAPIStreaming AnthropicOAuthAuth) $
      raiseUnder @(RestAPI AnthropicOAuthAuth)
      action

-- ============================================================================
-- OpenAI Auth Configuration
-- ============================================================================

data OpenAIAuth = OpenAIAuth { openaiApiKey :: String }

instance RestEndpoint OpenAIAuth where
    apiroot _ = "https://api.openai.com/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openaiApiKey api)
        , ("Content-Type", "application/json")
        ]

-- ============================================================================
-- OpenAI Interpreter
-- ============================================================================

interpretOpenAI :: forall model s r a.
                   ( ModelName model
                   , HasCodec (ProviderRequest model)
                   , HasCodec (ProviderResponse model)
                   , Monoid (ProviderRequest model)
                   , ProviderResponse model ~ OpenAIResponse
                   , Default s
                   , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail, Secret String] r
                   )
                => ComposableProvider model s
                -> model
                -> Sem (LLM model : r) a
                -> Sem r a
interpretOpenAI composableProvider model action = do
    apiKey <- getSecret
    let api = OpenAIAuth apiKey
    restapiHTTP api $
      restapiHTTPStreaming api $
      interpretLLM @OpenAIAuth composableProvider model $
      raiseUnder @(RestAPIStreaming OpenAIAuth) $
      raiseUnder @(RestAPI OpenAIAuth)
      action

-- ============================================================================
-- OpenRouter Auth Configuration
-- ============================================================================

data OpenRouterAuth = OpenRouterAuth { openrouterApiKey :: String }

instance RestEndpoint OpenRouterAuth where
    apiroot _ = "https://openrouter.ai/api/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openrouterApiKey api)
        , ("Content-Type", "application/json")
        ]

-- ============================================================================
-- OpenRouter Interpreter
-- ============================================================================

interpretOpenRouter :: forall model s r a.
                       ( ModelName model
                       , HasCodec (ProviderRequest model)
                       , HasCodec (ProviderResponse model)
                       , Monoid (ProviderRequest model)
                       , ProviderResponse model ~ OpenAIResponse
                       , Default s
                       , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail, Secret String] r
                       )
                    => ComposableProvider model s
                    -> model
                    -> Sem (LLM model : r) a
                    -> Sem r a
interpretOpenRouter composableProvider model action = do
    apiKey <- getSecret
    let api = OpenRouterAuth apiKey
    restapiHTTP api $
      restapiHTTPStreaming api $
      interpretLLM @OpenRouterAuth composableProvider model $
      raiseUnder @(RestAPIStreaming OpenRouterAuth) $
      raiseUnder @(RestAPI OpenRouterAuth)
      action

-- ============================================================================
-- ZAI Auth Configuration
-- ============================================================================

data ZAIAuth = ZAIAuth { zaiApiKey :: String }

instance RestEndpoint ZAIAuth where
    apiroot _ = "https://api.z.ai/api/coding/paas/v4"
    authheaders api =
        [ ("Authorization", "Bearer " <> zaiApiKey api)
        , ("Content-Type", "application/json")
        ]

-- ============================================================================
-- ZAI Interpreter
-- ============================================================================

interpretZAI :: forall model s r a.
                ( ModelName model
                , HasCodec (ProviderRequest model)
                , HasCodec (ProviderResponse model)
                , Monoid (ProviderRequest model)
                , ProviderResponse model ~ OpenAIResponse
                , Default s
                , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail, Secret String] r
                )
             => ComposableProvider model s
             -> model
             -> Sem (LLM model : r) a
             -> Sem r a
interpretZAI composableProvider model action = do
    apiKey <- getSecret
    let api = ZAIAuth apiKey
    restapiHTTP api $
      restapiHTTPStreaming api $
      interpretLLM @ZAIAuth composableProvider model $
      raiseUnder @(RestAPIStreaming ZAIAuth) $
      raiseUnder @(RestAPI ZAIAuth)
      action

-- ============================================================================
-- Llama.cpp Auth Configuration
-- ============================================================================

data LlamaCppAuth = LlamaCppAuth { llamacppEndpoint :: String }

instance RestEndpoint LlamaCppAuth where
    apiroot = llamacppEndpoint
    authheaders _ = [("Content-Type", "application/json")]

-- ============================================================================
-- Llama.cpp Interpreter
-- ============================================================================

interpretLlamaCpp :: forall model s r a.
                     ( ModelName model
                     , HasCodec (ProviderRequest model)
                     , HasCodec (ProviderResponse model)
                     , Monoid (ProviderRequest model)
                     , ProviderResponse model ~ OpenAIResponse
                     , Default s
                     , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail] r
                     )
                  => ComposableProvider model s
                  -> String  -- ^ Endpoint URL (e.g., "http://localhost:8080/v1")
                  -> model
                  -> Sem (LLM model : r) a
                  -> Sem r a
interpretLlamaCpp composableProvider endpoint model action =
    let api = LlamaCppAuth endpoint
    in restapiHTTP api $
       restapiHTTPStreaming api $
       interpretLLM @LlamaCppAuth composableProvider model $
       raiseUnder @(RestAPIStreaming LlamaCppAuth) $
       raiseUnder @(RestAPI LlamaCppAuth)
       action

-- ============================================================================
-- Cancellable Wrapper
-- ============================================================================

-- | Wrapper that adds cancellation support to any LLM interpreter
-- Returns empty list if cancellation is active
withLLMCancellation :: forall model r a.
                       Member Cancellation r
                    => Sem (LLM model : r) a
                    -> Sem (LLM model : r) a
withLLMCancellation action = reinterpret (\case
    QueryLLM configs messages -> onCancellation [] (queryLLM configs messages)
    ) action
