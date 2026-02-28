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
    -- ** LLMStream interpreter (new streaming architecture)
  , interpretLLMStream
  , startLLMStream
    -- ** Convenience (bundles restapiHTTP, for tests etc.)
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
import Polysemy.State (State, evalState, get, put, modify)
import Autodocodec (HasCodec, toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson (Value, encode, eitherDecodeStrict)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Default (Default, def)
import qualified Data.Map.Strict as Data.Map

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..), oauthHeaders)
import UniversalLLM.Providers.OpenAI (OpenAI(..), OpenRouter(..), LlamaCpp(..))

import Runix.LLM (LLM(..))
import Runix.LLMStream (LLMStream(..), LLMStreamResult(..), StreamEvent(..), StreamId(..), fetchStreamEvent, cancelLLMStream)
import Runix.HTTP (HTTP, HTTPRequest(..), HTTPStreaming(..), HTTPStreamedResult(..), ConnectionId(..), httpRequestStreaming, fetchChunk, cancelStream)
import Runix.RestAPI (RestEndpoint(..), Endpoint(..), post, restapiHTTP, RestAPI)
import Runix.Cancellation (Cancellation, onCancellation)
import Runix.Streaming.SSE (StreamingContent(..), extractContentFromChunk)
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
    { streamEventBuffer :: [StreamEvent]
    , streamAccumulatedResponse :: ProviderResponse model  -- Accumulated response being built
    , streamStackState :: s
    , streamConnectionId :: ConnectionId  -- HTTP connection for this stream
    , streamConfigs :: [ModelConfig model]  -- Original configs for this stream
    }

-- | State for managing active streams
data LLMStreamsState model s = LLMStreamsState
    { nextStreamId :: Int
    , activeStreams :: Data.Map.Map StreamId (LLMStreamState model s)
    }

-- | LLMStream interpreter - provides the LLMStream effect via HTTPStreaming
--
-- This is the interpreter that handles LLMStream operations by managing
-- streaming state and delegating to HTTPStreaming.
interpretLLMStream :: forall p model s r a.
                      ( ModelName model
                      , HasCodec (ProviderRequest model)
                      , Monoid (ProviderRequest model)
                      , ProviderProtocol (ProviderResponse model)
                      , RestEndpoint p
                      , Default s
                      , Members '[HTTPStreaming, Fail] r
                      )
                   => p
                   -> ComposableProvider model s
                   -> model
                   -> Sem (LLMStream model : r) a
                   -> Sem r a
interpretLLMStream api composableProvider model action =
    evalState (model, def @s) $
    evalState (LLMStreamsState 0 mempty :: LLMStreamsState model s) $
    interpret (\case
        StartLLMStreamInternal configs messages -> runFail $ do
            (m, stackState) <- get @(model, s)

            -- Build the provider request
            let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
                requestValue = encode $ toJSONViaCodec request
                httpReq = HTTPRequest
                    { method = "POST"
                    , uri = apiroot api <> "/" <> case protocolEndpoint @(ProviderResponse model) of Endpoint p -> p
                    , headers = authheaders api ++ [("Content-Type", "application/json")]
                    , body = Just requestValue
                    }

            put (m, stackState')

            -- Forward to HTTPStreaming to start the request
            result <- raise $ raise $ send (HttpRequestStreaming httpReq)
            case result of
                Left err -> fail err
                Right connId -> do
                    -- Allocate new StreamId and associate with ConnectionId
                    streamsState <- get @(LLMStreamsState model s)
                    let streamId = StreamId (nextStreamId streamsState)
                        initialStreamState = LLMStreamState
                            { streamEventBuffer = []
                            , streamAccumulatedResponse = emptyStreamingResponse @(ProviderResponse model)
                            , streamStackState = stackState'
                            , streamConnectionId = connId
                            , streamConfigs = configs
                            }
                    put $ streamsState
                        { nextStreamId = nextStreamId streamsState + 1
                        , activeStreams = Data.Map.insert streamId initialStreamState (activeStreams streamsState)
                        }
                    return streamId

        FetchStreamEventInternal streamId -> fetchEventForStream streamId

        GetAccumulatedResult streamId -> do
            (m, _) <- get @(model, s)
            streamsState <- get @(LLMStreamsState model s)
            case Data.Map.lookup streamId (activeStreams streamsState) of
                Nothing -> return $ Left "Stream not found"
                Just streamState -> do
                    -- Use composableProvider to convert accumulated response to messages
                    case fromProviderResponse composableProvider m (streamConfigs streamState)
                            (streamStackState streamState)
                            (streamAccumulatedResponse streamState) of
                        Left err -> return $ Left $ "Failed to parse accumulated response: " ++ show err
                        Right (_stackState', messages) -> return $ Right messages

        CancelLLMStreamInternal streamId -> do
            streamsState <- get @(LLMStreamsState model s)
            case Data.Map.lookup streamId (activeStreams streamsState) of
                Nothing -> return ()
                Just streamState -> do
                    -- Close the HTTP connection
                    raise $ raise $ send (CloseConnectionInternal (streamConnectionId streamState))
                    -- Remove from active streams
                    modify $ \ss -> ss { activeStreams = Data.Map.delete streamId (activeStreams ss) }

    ) (raiseUnder @(State (LLMStreamsState model s)) $ raiseUnder @(State (model, s)) action)
  where
    contentToEvent :: StreamingContent -> StreamEvent
    contentToEvent (StreamingText txt) = StreamText txt
    contentToEvent (StreamingReasoning txt) = StreamThinking txt

    fetchEventForStream :: Members '[State (LLMStreamsState model s), State (model, s), HTTPStreaming, Fail] r' => StreamId -> Sem r' (Maybe StreamEvent)
    fetchEventForStream streamId = do
        streamsState <- get @(LLMStreamsState model s)
        case Data.Map.lookup streamId (activeStreams streamsState) of
            Nothing -> return Nothing  -- Stream closed
            Just streamState -> do
                -- Check buffer first
                case streamEventBuffer streamState of
                    (event:rest) -> do
                        modify $ \ss -> ss { activeStreams =
                            Data.Map.insert streamId (streamState { streamEventBuffer = rest }) (activeStreams ss) }
                        return $ Just event
                    [] -> do
                        -- Fetch next HTTP chunk using the tracked ConnectionId
                        mChunk <- send (FetchChunkInternal (streamConnectionId streamState))
                        case mChunk of
                            Nothing -> return Nothing  -- HTTP stream ended
                            Just chunk -> do
                                -- Extract streaming content from SSE chunk
                                let contents = extractContentFromChunk chunk
                                    events = map contentToEvent contents

                                -- Also parse chunk as JSON Value and merge into accumulated response
                                -- (SSE format has data: lines with JSON deltas)
                                let accumulated' = case eitherDecodeStrict chunk of
                                        Left _err -> streamAccumulatedResponse streamState  -- Keep old if parse fails
                                        Right (delta :: Value) ->
                                            mergeStreamingDelta @(ProviderResponse model)
                                                (streamAccumulatedResponse streamState) delta

                                case events of
                                    [] -> do
                                        -- Update accumulated response but no events to emit
                                        modify $ \ss -> ss { activeStreams =
                                            Data.Map.insert streamId
                                                (streamState { streamAccumulatedResponse = accumulated' })
                                                (activeStreams ss) }
                                        fetchEventForStream streamId  -- Recursively fetch next
                                    (event:rest) -> do
                                        -- Update state with new accumulation and buffered events
                                        modify $ \ss -> ss { activeStreams =
                                            Data.Map.insert streamId
                                                (streamState { streamEventBuffer = rest
                                                            , streamAccumulatedResponse = accumulated' })
                                                (activeStreams ss) }
                                        return $ Just event

-- | Start an LLM streaming request
--
-- Similar to httpRequestStreaming, this function:
-- - Takes model configs and messages
-- - Takes an action that uses LLMStreamResult effect
-- - Starts the stream and provides LLMStreamResult interpreter for the action
-- - Returns both the action result and the accumulated messages
-- - Automatically cleans up when done
startLLMStream :: forall model r a.
                  Members '[LLMStream model, Fail] r
               => [ModelConfig model]
               -> [Message model]
               -> Sem (LLMStreamResult model : r) a
               -> Sem r (a, Either String [Message model])
startLLMStream configs messages action = do
    -- Start the stream, get StreamId
    result <- send (StartLLMStreamInternal configs messages)
    case result of
        Left err -> fail err
        Right streamId -> do
            -- Interpret LLMStreamResult by forwarding to LLMStream with streamId
            -- Also manage event buffering via State
            actionResult <- evalState ([] :: [StreamEvent]) $
              interpret (\case
                  FetchStreamEvent -> fetchNext streamId
                  CancelLLMStream -> raise $ send (CancelLLMStreamInternal streamId)
              ) (raiseUnder @(State [StreamEvent]) action)

            -- Get the accumulated result
            accumulated <- send (GetAccumulatedResult streamId)

            -- Clean up
            send (CancelLLMStreamInternal streamId)

            return (actionResult, accumulated)
  where
    fetchNext :: Member (LLMStream model) r' => Member (State [StreamEvent]) r' => StreamId -> Sem r' (Maybe StreamEvent)
    fetchNext streamId = do
        -- Check buffer first
        buffer <- get @[StreamEvent]
        case buffer of
            (event:rest) -> do
                put rest
                return $ Just event
            [] -> do
                -- Buffer empty, need to fetch and parse more
                -- Forward to LLMStream interpreter which handles HTTP fetching
                send (FetchStreamEventInternal streamId)

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
