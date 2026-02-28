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
  ( -- * LLM interpreters
    -- ** Non-streaming (requires RestAPI already in effect row)
    interpretLLM
    -- ** LLMStream interpreter (new streaming architecture)
  , interpretLLMStream
  , startLLMStream
  , queryStreamingLLM
    -- ** Interpret LLM in terms of LLMStreaming
  , interpretLLMViaStreaming
    -- ** Convenience (bundles restapiHTTP, for tests etc.)
  , interpretLLMWith
  , interpretLLMStreamingWith
    -- * Protocol typeclass
  , ProviderProtocol(..)
  , EnableStreaming(..)
  , ProtocolRequest
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
import Data.Aeson (Value, encode, eitherDecodeStrict)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default (Default, def)

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), oauthHeaders)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))

import Runix.LLM (LLM(..))
import Runix.LLMStream (LLMStreaming, LLMStreamResult, StreamEvent(..))
import Runix.HTTP (HTTP, HTTPRequest(..), HTTPStreaming)
import qualified Runix.Streaming as Streaming
import Runix.Streaming (interpretStreamingStateful)
import Runix.RestAPI (RestEndpoint(..), Endpoint(..), post, restapiHTTP, RestAPI)
import Runix.Cancellation (Cancellation, onCancellation)
import Runix.Streaming.SSE (StreamingContent(..), extractContentFromChunk, parseSSE)
import qualified Data.ByteString.Lazy as BSL
import UniversalLLM.Protocols.Anthropic (AnthropicResponse(..), AnthropicSuccessResponse(..), AnthropicUsage(..), mergeAnthropicDelta, AnthropicRequest, enableAnthropicStreaming)
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), mergeOpenAIDelta, defaultOpenAIMessage, defaultOpenAISuccessResponse, defaultOpenAIChoice, OpenAIRequest, enableOpenAIStreaming)

-- ============================================================================
-- Protocol Typeclass
-- ============================================================================

-- | Typeclass for provider protocols (Anthropic vs OpenAI-compatible)
-- Abstracts over the protocol-specific details like endpoint path and streaming handling
class ProviderProtocol response where
  type ProtocolRequest response
  -- | The endpoint path for this protocol
  protocolEndpoint :: Endpoint
  -- | Create an empty response for streaming accumulation
  emptyStreamingResponse :: response
  -- | Merge a streaming delta (as JSON Value) into the accumulated response
  mergeStreamingDelta :: response -> Value -> response

instance ProviderProtocol AnthropicResponse where
  type ProtocolRequest AnthropicResponse = AnthropicRequest
  protocolEndpoint = Endpoint "messages"
  emptyStreamingResponse = AnthropicSuccess $
    AnthropicSuccessResponse "" "" "assistant" [] Nothing (AnthropicUsage 0 0)
  mergeStreamingDelta = mergeAnthropicDelta

instance ProviderProtocol OpenAIResponse where
  type ProtocolRequest OpenAIResponse = OpenAIRequest
  protocolEndpoint = Endpoint "chat/completions"
  emptyStreamingResponse = OpenAISuccess $
    defaultOpenAISuccessResponse
      { choices = [defaultOpenAIChoice { message = defaultOpenAIMessage { role = "assistant" } }] }
  mergeStreamingDelta = mergeOpenAIDelta

-- ============================================================================
-- Streaming Enabler Typeclass
-- ============================================================================

-- | Typeclass to enable streaming based on the response type
class ProviderProtocol response => EnableStreaming response where
  enableStreamingForProtocol :: ProtocolRequest response -> ProtocolRequest response

instance EnableStreaming AnthropicResponse where
  enableStreamingForProtocol = enableAnthropicStreaming

instance EnableStreaming OpenAIResponse where
  enableStreamingForProtocol = enableOpenAIStreaming

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
-- | Internal interpreter with explicit state - first-order, no streaming
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
interpretLLMWithState composableProvider = interpret $ \case
    QueryLLM configs messages -> do
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
        let requestValue = toJSONViaCodec request

        result <- sendRequest @p requestValue
        case result of
            Left err -> return $ Left $ "Failed to parse response: " ++ err
            Right providerResponse ->
                case fromProviderResponse composableProvider m configs stackState' providerResponse of
                    Left err -> return $ Left $ "LLM error: " ++ show err
                    Right (stackState'', resultMessages) -> do
                        put (m, stackState'')
                        return $ Right resultMessages

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
-- LLMStream Interpreter (new streaming architecture)
-- ============================================================================

-- | Internal state for an active LLM stream
data LLMStreamState model s = LLMStreamState
    { streamHTTPStreamId :: Streaming.StreamId  -- HTTP stream ID for this LLM stream
    , streamEventBuffer :: [StreamEvent]  -- Buffered events from current chunk
    , streamAccumulatedResponse :: ProviderResponse model  -- Accumulated response
    , streamStackState :: s
    , streamConfigs :: [ModelConfig model]
    , streamModel :: model
    }

-- | LLMStream interpreter - provides LLM streaming via HTTPStreaming
--
-- Uses interpretStreamingStateful to manage stream state.
-- Fetches HTTP chunks, parses SSE, extracts events, accumulates responses.
interpretLLMStream :: forall p model s r a.
                      ( ModelName model
                      , HasCodec (ProviderRequest model)
                      , Monoid (ProviderRequest model)
                      , EnableStreaming (ProviderResponse model)
                      , ProtocolRequest (ProviderResponse model) ~ ProviderRequest model
                      , HasStreaming model
                      , RestEndpoint p
                      , Default s
                      , Members '[HTTPStreaming, Fail] r
                      )
                   => p
                   -> ComposableProvider model s
                   -> model
                   -> Sem (LLMStreaming model : r) a
                   -> Sem r a
interpretLLMStream api composableProvider model action =
    evalState (model, def @s) $
    interpretStreamingStateful onStart onFetch onClose $
    raiseUnder @(State (model, s)) action
  where
    contentToEvent :: StreamingContent -> StreamEvent
    contentToEvent (StreamingText txt) = StreamText txt
    contentToEvent (StreamingReasoning txt) = StreamThinking txt

    -- Initialize: Build HTTP request and start HTTP stream
    onStart (configs, messages) = do
        (m, stackState) <- get @(model, s)

        -- Build the provider request
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
            -- Enable streaming on the request
            streamingRequest = enableStreamingForProtocol @(ProviderResponse model) request
            requestValue = encode $ toJSONViaCodec streamingRequest
            httpReq = HTTPRequest
                { method = "POST"
                , uri = apiroot api <> "/" <> case protocolEndpoint @(ProviderResponse model) of Endpoint p -> p
                , headers = authheaders api ++ [("Content-Type", "application/json")]
                , body = Just requestValue
                }

        put (m, stackState')

        -- Start HTTP stream
        result <- raise $ send (Streaming.StartStream httpReq)
        case result of
            Left err -> return $ Left err
            Right httpStreamId -> do
                let initialStreamState = LLMStreamState
                        { streamEventBuffer = []
                        , streamAccumulatedResponse = emptyStreamingResponse @(ProviderResponse model)
                        , streamStackState = stackState'
                        , streamHTTPStreamId = httpStreamId
                        , streamConfigs = configs
                        , streamModel = m
                        }
                return $ Right initialStreamState

    -- Fetch: Get next event, parsing HTTP chunks as needed
    onFetch streamState = do
        -- Check buffer first
        case streamEventBuffer streamState of
            (event:rest) -> do
                return (Just event, streamState { streamEventBuffer = rest })
            [] -> do
                -- Fetch next HTTP chunk using the tracked HTTP StreamId
                mChunk <- send (Streaming.FetchItem (streamHTTPStreamId streamState))
                case mChunk of
                    Nothing -> return (Nothing, streamState)  -- HTTP stream ended
                    Just chunk -> do
                        -- Extract streaming content from SSE chunk
                        let contents = extractContentFromChunk chunk
                            events = map contentToEvent contents

                        -- Parse SSE chunk to get JSON deltas and merge into accumulated response
                        let chunkLazy = BSL.fromStrict chunk
                            deltas = parseSSE chunkLazy
                            accumulated' = foldl (mergeStreamingDelta @(ProviderResponse model))
                                                 (streamAccumulatedResponse streamState)
                                                 deltas

                        case events of
                            [] -> do
                                -- Update accumulated response but no events to emit, recursively fetch next
                                onFetch (streamState { streamAccumulatedResponse = accumulated' })
                            (event:rest) -> do
                                -- Update state with new accumulation and buffered events
                                let streamState' = streamState
                                        { streamEventBuffer = rest
                                        , streamAccumulatedResponse = accumulated'
                                        }
                                return (Just event, streamState')

    -- Close: Convert accumulated response to messages
    onClose streamState = do
        (m, _) <- get @(model, s)
        -- Close the HTTP stream
        _ <- raise $ send (Streaming.CloseStream (streamHTTPStreamId streamState))
        -- Use composableProvider to convert accumulated response to messages
        case fromProviderResponse composableProvider m (streamConfigs streamState)
                (streamStackState streamState)
                (streamAccumulatedResponse streamState) of
            Left err -> return $ Left $ "Failed to parse accumulated response: " ++ show err
            Right (_stackState', messages) -> return $ Right messages

-- | Start an LLM streaming request
--
-- Similar to httpRequestStreaming, this function:
-- - Takes model configs and messages
-- - Takes an action that uses LLMStreamResult effect
-- - Starts the stream and provides LLMStreamResult interpreter for the action
-- - Returns both the action result and the accumulated messages
-- - Automatically cleans up when done
startLLMStream :: forall model r a.
                  Members '[LLMStreaming model, Fail] r
               => [ModelConfig model]
               -> [Message model]
               -> Sem (LLMStreamResult model : r) a
               -> Sem r (a, Either String [Message model])
startLLMStream configs messages = Streaming.startStream (configs, messages)

-- | Convenience function that streams an LLM request and returns the accumulated messages
--
-- This consumes all streaming events and returns just the final accumulated messages.
-- Use this when you want to use streaming transport (SSE) but don't need real-time events.
queryStreamingLLM :: forall model r.
                     Members '[LLMStreaming model, Fail] r
                  => [ModelConfig model]
                  -> [Message model]
                  -> Sem r [Message model]
queryStreamingLLM configs messages = do
    ((), messagesResult) <- startLLMStream configs messages $ do
        let consumeEvents = do
              mEvent <- Streaming.fetchNext @StreamEvent
              case mEvent of
                Nothing -> return ()
                Just _ -> consumeEvents
        consumeEvents
    case messagesResult of
        Left err -> fail err
        Right msgs -> return msgs

-- ============================================================================
-- Interpret LLM in terms of LLMStreaming
-- ============================================================================

-- | Interpret LLM effect using LLMStreaming infrastructure
--
-- This allows non-streaming LLM code to run on top of streaming infrastructure.
-- It consumes all streaming events internally and returns the final accumulated messages.
interpretLLMViaStreaming :: forall model r a.
                            Members '[LLMStreaming model, Fail] r
                         => Sem (LLM model : r) a
                         -> Sem r a
interpretLLMViaStreaming = interpret $ \case
    QueryLLM configs messages -> do
        Right <$> queryStreamingLLM configs messages

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
                             , Monoid (ProviderRequest model)
                             , EnableStreaming (ProviderResponse model)
                             , ProtocolRequest (ProviderResponse model) ~ ProviderRequest model
                             , HasStreaming model
                             , RestEndpoint p
                             , Default s
                             , Members '[HTTPStreaming, Fail] r
                             )
                          => p
                          -> ComposableProvider model s
                          -> model
                          -> Sem (LLMStreaming model : r) a
                          -> Sem r a
interpretLLMStreamingWith api composableProvider model action =
    interpretLLMStream api composableProvider model action

-- ============================================================================
-- Cancellable Wrapper
-- ============================================================================

-- | Wrapper that adds cancellation support to any LLM interpreter
-- Returns empty list if cancellation is active
withLLMCancellation :: forall model r a.
                       Member Cancellation r
                    => Sem (LLM model : r) a
                    -> Sem (LLM model : r) a
withLLMCancellation action = intercept (\case
    QueryLLM configs messages ->
        onCancellation (Right []) (send (QueryLLM configs messages))
    ) action
