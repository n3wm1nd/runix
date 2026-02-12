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
  ( -- * LLM interpreter (requires RestAPI already in effect row)
    interpretLLM
    -- * Convenience (bundles restapiHTTP + interpretLLM, for tests etc.)
  , interpretLLMWith
    -- * Protocol typeclass
  , ProviderProtocol(..)
    -- * Cancellation wrapper
  , withLLMCancellation
    -- * Generic model wrappers
  , GenericModel(..)
    -- * Auth configurations (for use with restapiHTTP)
  , AnthropicAPIKeyAuth(..)
  , AnthropicOAuthAuth(..)
  , OpenAIAuth(..)
  , OpenRouterAuth(..)
  , ZAIAuth(..)
  , LlamaCppAuth(..)
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
import Runix.HTTP (HTTP)
import Runix.RestAPI (RestEndpoint(..), Endpoint(..), post, restapiHTTP, RestAPI)
import Runix.Cancellation (Cancellation, onCancellation)
import UniversalLLM.Protocols.Anthropic (AnthropicResponse(..), AnthropicSuccessResponse(..), AnthropicUsage(..), mergeAnthropicDelta)
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), mergeOpenAIDelta, defaultOpenAIMessage, defaultOpenAISuccessResponse, defaultOpenAIChoice)

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
-- Request Helper
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

-- ============================================================================
-- LLM Interpreter (non-streaming)
-- ============================================================================

-- | Internal interpreter with explicit state in effect row
interpretLLMWithState :: forall p model s r a.
                         ( ModelName model
                         , HasCodec (ProviderRequest model)
                         , HasCodec (ProviderResponse model)
                         , Monoid (ProviderRequest model)
                         , ProviderProtocol (ProviderResponse model)
                         , Members '[RestAPI p, Fail, State (model, s)] r
                         )
                      => ComposableProvider model s
                      -> Sem (LLM model : r) a
                      -> Sem r a
interpretLLMWithState composableProvider = interpret $ \case
    QueryLLM configs messages -> do
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
        let requestValue = toJSONViaCodec request

        providerResponse <- sendRequest @p requestValue

        case fromProviderResponse composableProvider m configs stackState' providerResponse of
            Left err -> fail $ "LLM error: " ++ show err
            Right (stackState'', resultMessages) -> do
                put (m, stackState'')
                return resultMessages

-- | Non-streaming LLM interpreter.
-- For streaming support, use 'interpretLLMStreaming' from "Runix.LLM.Streaming".
interpretLLM :: forall p model s r a.
                ( ModelName model
                , HasCodec (ProviderRequest model)
                , HasCodec (ProviderResponse model)
                , Monoid (ProviderRequest model)
                , ProviderProtocol (ProviderResponse model)
                , Default s
                , Members '[RestAPI p, Fail] r
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
-- Auth Configurations
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

data OpenAIAuth = OpenAIAuth { openaiApiKey :: String }

instance RestEndpoint OpenAIAuth where
    apiroot _ = "https://api.openai.com/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openaiApiKey api)
        , ("Content-Type", "application/json")
        ]

data OpenRouterAuth = OpenRouterAuth { openrouterApiKey :: String }

instance RestEndpoint OpenRouterAuth where
    apiroot _ = "https://openrouter.ai/api/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openrouterApiKey api)
        , ("Content-Type", "application/json")
        ]

data ZAIAuth = ZAIAuth { zaiApiKey :: String }

instance RestEndpoint ZAIAuth where
    apiroot _ = "https://api.z.ai/api/coding/paas/v4"
    authheaders api =
        [ ("Authorization", "Bearer " <> zaiApiKey api)
        , ("Content-Type", "application/json")
        ]

data LlamaCppAuth = LlamaCppAuth { llamacppEndpoint :: String }

instance RestEndpoint LlamaCppAuth where
    apiroot = llamacppEndpoint
    authheaders _ = [("Content-Type", "application/json")]

-- ============================================================================
-- Generic Interpreter
-- ============================================================================

-- | Generic non-streaming LLM interpreter that works with any RestEndpoint.
-- The caller provides the endpoint configuration (with auth already resolved).
-- For streaming, use 'interpretLLMStreamingWith' from "Runix.LLM.Streaming".
interpretLLMWith :: forall p model s r a.
                    ( ModelName model
                    , HasCodec (ProviderRequest model)
                    , HasCodec (ProviderResponse model)
                    , Monoid (ProviderRequest model)
                    , ProviderProtocol (ProviderResponse model)
                    , RestEndpoint p
                    , Default s
                    , Members '[HTTP, Fail] r
                    )
                 => p
                 -> ComposableProvider model s
                 -> model
                 -> Sem (LLM model : r) a
                 -> Sem r a
interpretLLMWith api composableProvider model action =
    restapiHTTP api $
    interpretLLM @p composableProvider model $
    raiseUnder @(RestAPI p)
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
