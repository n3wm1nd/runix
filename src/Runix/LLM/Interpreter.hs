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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Runix.LLM.Interpreter
  ( -- * LLM interpreter
    interpretLLM
    -- * Convenience (bundles restapiHTTP and llmRetry)
  , interpretLLMWith
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
  , AlibabaCloudAuth(..)
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
import Data.Aeson (Value, encode)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default (Default, def)
import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), oauthHeaders)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))

import Runix.LLM (LLM(..))
import Runix.HTTP (HTTP)
import qualified Runix.RestAPI as RestAPI
import Runix.RestAPI (RestEndpoint(..), Endpoint(..), restapiHTTP, RestAPI, llmRetry)
import Runix.Time (Time, Sleep)
import Runix.Cancellation (Cancellation, onCancellation)


-- ============================================================================
-- Request Helper
-- ============================================================================

-- | Send a non-streaming request to the provider
sendRequest :: forall p response r.
               ( HasCodec response
               , Member (RestAPI p) r
               )
            => Endpoint -> Value -> Sem r (Either RestAPI.RestError response)
sendRequest endpoint requestValue = do
    result <- RestAPI.restRequest "POST" endpoint (Just requestValue)
    return $ result >>= \body -> case parseEither parseJSONViaCodec body of
        Left err -> Left $ RestAPI.ParseError err (encode body)
        Right v  -> Right v

-- ============================================================================
-- LLM Interpreter
-- ============================================================================

-- | Internal interpreter with explicit state in effect row
interpretLLMWithState :: forall p model s r a.
                         ( ModelName model
                         , Provider model
                         , Members '[RestAPI p, State (model, s)] r
                         )
                      => ComposableProvider model s
                      -> [ModelConfig model]
                      -> Sem (LLM model : r) a
                      -> Sem r a
interpretLLMWithState composableProvider defaultConfigs = interpret $ \case
    QueryLLM queryConfigs messages -> do
        let mergedConfigs = queryConfigs ++ defaultConfigs
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m mergedConfigs stackState messages
        let requestValue = toJSONViaCodec request
            endpoint = Endpoint (T.unpack (endpointPath @(ProviderRequest model)))

        result <- sendRequest @p endpoint requestValue
        case result of
            Left err -> return $ Left $ RestAPI.restErrorMessage err
            Right providerResponse ->
                case fromProviderResponse composableProvider m mergedConfigs stackState' providerResponse of
                    Left err -> return $ Left $ "LLM error: " ++ show err
                    Right (stackState'', resultMessages) -> do
                        put (m, stackState'')
                        return $ Right resultMessages

-- | LLM interpreter. Requires 'RestAPI p' in the effect row.
--
-- To add streaming support, insert 'llmStreamingRestAPI' between this and
-- 'restapiHTTP' in the interpreter stack. To add retry, wrap with 'llmRetry'.
-- For the common case, use 'interpretLLMWith' which bundles both.
interpretLLM :: forall p model s r a.
                ( ModelName model
                , Provider model
                , Default s
                , Member (RestAPI p) r
                )
             => ComposableProvider model s
             -> model
             -> [ModelConfig model]
             -> Sem (LLM model : r) a
             -> Sem r a
interpretLLM composableProvider model defaultConfigs action =
    evalState (model, def @s) $
    interpretLLMWithState @p composableProvider defaultConfigs $
    raiseUnder action

-- ============================================================================
-- Convenience Interpreter
-- ============================================================================

-- | Convenience interpreter: bundles 'restapiHTTP' and 'llmRetry'.
--
-- For streaming support, use 'llmStreamingRestAPI' from "Runix.LLM.Streaming"
-- between this and the HTTP interpreter:
--
-- > httpIO . httpStreamingIO . llmStreamingRestAPI @model api . interpretLLMWith api provider model configs
interpretLLMWith :: forall p model s r a.
                    ( ModelName model
                    , Provider model
                    , RestEndpoint p
                    , Default s
                    , Members '[HTTP, Fail, Time, Sleep] r
                    )
                 => p
                 -> ComposableProvider model s
                 -> model
                 -> [ModelConfig model]
                 -> Sem (LLM model : r) a
                 -> Sem r a
interpretLLMWith api composableProvider model defaultConfigs action =
    restapiHTTP api $
    llmRetry $
    interpretLLM @p composableProvider model defaultConfigs $
    raiseUnder @(RestAPI p)
    action

-- ============================================================================
-- Generic Model Wrapper
-- ============================================================================

-- | Generic model that accepts any model name string at runtime.
-- Example: GenericModel "deepseek/deepseek-chat-v3-0324:free"
data GenericModel = GenericModel
    { modelId :: Text
    } deriving (Show, Eq)

instance ModelName (Model GenericModel OpenRouter) where
  modelName (Model m _) = modelId m

-- ============================================================================
-- Auth Configurations
-- ============================================================================

data AnthropicAPIKeyAuth = AnthropicAPIKeyAuth { anthropicApiKey :: String }

instance RestEndpoint AnthropicAPIKeyAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders api =
        [ ("x-api-key", anthropicApiKey api)
        , ("anthropic-version", "2023-06-01")
        ]

data AnthropicOAuthAuth = AnthropicOAuthAuth { anthropicOAuthToken :: String }

instance RestEndpoint AnthropicOAuthAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders auth = map (\(a, b) -> (T.unpack a, T.unpack b)) $ oauthHeaders (T.pack $ anthropicOAuthToken auth)

data OpenAIAuth = OpenAIAuth { openaiApiKey :: String }

instance RestEndpoint OpenAIAuth where
    apiroot _ = "https://api.openai.com/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openaiApiKey api)
        ]

data OpenRouterAuth = OpenRouterAuth { openrouterApiKey :: String }

instance RestEndpoint OpenRouterAuth where
    apiroot _ = "https://openrouter.ai/api/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> openrouterApiKey api)
        ]

data ZAIAuth = ZAIAuth { zaiApiKey :: String }

instance RestEndpoint ZAIAuth where
    apiroot _ = "https://api.z.ai/api/coding/paas/v4"
    authheaders api =
        [ ("Authorization", "Bearer " <> zaiApiKey api)
        ]

data AlibabaCloudAuth = AlibabaCloudAuth { alibabaCloudApiKey :: String }

instance RestEndpoint AlibabaCloudAuth where
    apiroot _ = "https://coding-intl.dashscope.aliyuncs.com/v1"
    authheaders api =
        [ ("Authorization", "Bearer " <> alibabaCloudApiKey api)
        ]

data LlamaCppAuth = LlamaCppAuth { llamacppEndpoint :: String }

instance RestEndpoint LlamaCppAuth where
    apiroot = llamacppEndpoint
    authheaders _ = []

-- ============================================================================
-- Cancellable Wrapper
-- ============================================================================

-- | Wrapper that adds cancellation support to any LLM interpreter.
-- Returns empty list if cancellation is active.
withLLMCancellation :: forall model r a.
                       Member Cancellation r
                    => Sem (LLM model : r) a
                    -> Sem (LLM model : r) a
withLLMCancellation action = intercept (\case
    QueryLLM configs messages ->
        onCancellation (Right []) (send (QueryLLM configs messages))
    ) action
