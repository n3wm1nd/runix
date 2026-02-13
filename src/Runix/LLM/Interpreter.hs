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

module Runix.LLM.Interpreter
  ( -- * LLM interpreters
    -- ** Non-streaming (requires RestAPI already in effect row)
    interpretLLM
    -- ** Streaming (requires RestAPI + HTTPStreaming already in effect row)
  , interpretLLMStreaming
    -- ** Convenience (bundles restapiHTTP, for tests etc.)
  , interpretLLMWith
  , interpretLLMStreamingWith
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
import qualified Data.ByteString as BS
import Data.Default (Default, def)

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), oauthHeaders)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))

import Runix.LLM (LLM(..), LLMInfo(..))
import Runix.HTTP (HTTP, HTTPStreaming, HTTPResponse(..), httpRequestStreaming)
import Runix.RestAPI (RestEndpoint(..), Endpoint(..), post, restapiHTTP, RestAPI, makeHTTPRequest)
import Runix.Streaming (StreamChunk(..))
import Runix.Streaming.SSE (reassembleSSE, extractContentFromChunk)
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
-- Configuration Helper
-- ============================================================================

isStreamingEnabled :: [ModelConfig model] -> Bool
isStreamingEnabled configs =
  case [s | Streaming s <- configs] of
    (s:_) -> s
    [] -> False

-- ============================================================================
-- Request Helper
-- ============================================================================

-- | Send a non-streaming request to the provider
sendRequest :: forall p response r.
               ( ProviderProtocol response
               , HasCodec response
               , Member (RestAPI p) r
               )
            => Value -> Sem r (Either String response)
sendRequest requestValue = do
    responseValue <- post (protocolEndpoint @response) requestValue
    return $ parseEither parseJSONViaCodec responseValue

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
                         , Members '[RestAPI p, State (model, s)] r
                         )
                      => ComposableProvider model s
                      -> Sem (LLM model : r) a
                      -> Sem r a
interpretLLMWithState composableProvider = interpretH $ \case
    QueryLLM configs messages _callback -> do
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
        let requestValue = toJSONViaCodec request

        result <- sendRequest @p requestValue
        case result of
            Left err -> pureT $ Left $ "Failed to parse response: " ++ err
            Right providerResponse ->
                case fromProviderResponse composableProvider m configs stackState' providerResponse of
                    Left err -> pureT $ Left $ "LLM error: " ++ show err
                    Right (stackState'', resultMessages) -> do
                        put (m, stackState'')
                        pureT $ Right resultMessages

-- | Non-streaming LLM interpreter.
interpretLLM :: forall p model s r a.
                ( ModelName model
                , HasCodec (ProviderRequest model)
                , HasCodec (ProviderResponse model)
                , Monoid (ProviderRequest model)
                , ProviderProtocol (ProviderResponse model)
                , Default s
                , Member (RestAPI p) r
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
-- LLM Interpreter (streaming)
-- ============================================================================

-- | Internal streaming interpreter with explicit state.
-- Higher-order because 'LLM' carries an info callback.
--
-- In the streaming path, locally provides and interprets 'StreamChunk BS.ByteString'
-- within the tactical row. Each parsed chunk is delivered to the caller's context
-- via 'runTSimple' in real-time as it arrives from the HTTP layer.
--
-- The callback in 'QueryLLM' is executed via 'runTSimple' for each streaming
-- chunk. An 'interceptH' layer above can replace the callback to route chunks.
interpretLLMStreamingWithState :: forall p model s r a.
                                   ( ModelName model
                                   , HasCodec (ProviderRequest model)
                                   , HasCodec (ProviderResponse model)
                                   , Monoid (ProviderRequest model)
                                   , ProviderProtocol (ProviderResponse model)
                                   , RestEndpoint p
                                   , Members '[RestAPI p, HTTPStreaming, Cancellation, Fail, State (model, s)] r
                                   )
                                => p
                                -> ComposableProvider model s
                                -> Sem (LLM model : r) a
                                -> Sem r a
interpretLLMStreamingWithState api composableProvider = interpretH $ \case
    QueryLLM configs messages callback -> do
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
        let requestValue = toJSONViaCodec request

        if isStreamingEnabled configs
            then do
                let httpReq = makeHTTPRequest api "POST" (protocolEndpoint @(ProviderResponse model)) (Just requestValue)
                httpResp <- interpret (\case
                    EmitChunk (chunk :: BS.ByteString) -> do
                        let contents = extractContentFromChunk chunk
                        mapM_ (\c -> do _ <- runTSimple (callback (LLMInfo c)); return ()) contents
                    ) $ httpRequestStreaming httpReq

                let providerResponse = reassembleSSE (mergeStreamingDelta @(ProviderResponse model))
                                                      (emptyStreamingResponse @(ProviderResponse model))
                                                      (body httpResp)
                case fromProviderResponse composableProvider m configs stackState' providerResponse of
                    Left err -> pureT $ Left $ "LLM error: " ++ show err
                    Right (stackState'', resultMessages) -> do
                        put (m, stackState'')
                        pureT $ Right resultMessages
            else do
                -- Non-streaming path: no callback events
                result <- sendRequest @p requestValue
                case result of
                    Left err -> pureT $ Left $ "Failed to parse response: " ++ err
                    Right providerResponse ->
                        case fromProviderResponse composableProvider m configs stackState' providerResponse of
                            Left err -> pureT $ Left $ "LLM error: " ++ show err
                            Right (stackState'', resultMessages) -> do
                                put (m, stackState'')
                                pureT $ Right resultMessages

-- | Streaming LLM interpreter.
--
-- When @Streaming True@ is in configs, uses 'HTTPStreaming' and delivers
-- parsed SSE chunks via the callback in 'QueryLLM'. When streaming is disabled,
-- falls back to non-streaming 'RestAPI'.
--
-- The callback in 'QueryLLM' is executed via 'runTSimple', so an 'interceptH'
-- layer above can replace the callback to route chunks (e.g. to a TUI widget).
interpretLLMStreaming :: forall p model s r a.
                          ( ModelName model
                          , HasCodec (ProviderRequest model)
                          , HasCodec (ProviderResponse model)
                          , Monoid (ProviderRequest model)
                          , ProviderProtocol (ProviderResponse model)
                          , RestEndpoint p
                          , Default s
                          , Members '[RestAPI p, HTTPStreaming, Cancellation, Fail] r
                          )
                       => p
                       -> ComposableProvider model s
                       -> model
                       -> Sem (LLM model : r) a
                       -> Sem r a
interpretLLMStreaming api composableProvider model action =
    evalState (model, def @s) $
    interpretLLMStreamingWithState @p api composableProvider $
    raiseUnder @(State (model, s)) action

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
-- Convenience Interpreters (bundle restapiHTTP)
-- ============================================================================

-- | Non-streaming convenience interpreter.
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

-- | Streaming convenience interpreter.
interpretLLMStreamingWith :: forall p model s r a.
                              ( ModelName model
                              , HasCodec (ProviderRequest model)
                              , HasCodec (ProviderResponse model)
                              , Monoid (ProviderRequest model)
                              , ProviderProtocol (ProviderResponse model)
                              , RestEndpoint p
                              , Default s
                              , Members '[HTTP, HTTPStreaming, Cancellation, Fail] r
                              )
                           => p
                           -> ComposableProvider model s
                           -> model
                           -> Sem (LLM model : r) a
                           -> Sem r a
interpretLLMStreamingWith api composableProvider model action =
    restapiHTTP api $
    interpretLLMStreaming @p api composableProvider model $
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
withLLMCancellation action = reinterpretH (\case
    QueryLLM configs messages _callback ->
        -- TODO: forward callback through cancellation wrapper
        pureT =<< raise (onCancellation (Right []) (send (QueryLLM configs messages (\_ -> pure ()))))
    ) action
