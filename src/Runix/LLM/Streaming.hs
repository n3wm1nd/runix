{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | LLM-specific streaming wiring for 'Runix.RestAPI.Streaming'.
--
-- Bridges the generic 'streamingRestAPI' interceptor with the 'EnableStreaming'
-- typeclass from "UniversalLLM", supplying protocol-aware detection, reassembly,
-- and event extraction functions derived from the model type.
module Runix.LLM.Streaming
  ( llmStreamingRestAPI
  , interpretLLMWithStreaming
  ) where

import Polysemy
import Polysemy.Fail

import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.LLM (LLM)
import Runix.LLMStream (StreamEvent(..))
import Runix.RestAPI (RestAPI, RestEndpoint, restapiHTTP, llmRetry)
import Runix.RestAPI.Streaming (streamingRestAPI)
import Runix.StreamChunk (StreamChunk)
import Runix.Time (Time, Sleep)
import Runix.LLM.Interpreter (interpretLLM)
import Data.Default (Default, def)

import UniversalLLM (EnableStreaming, StreamingContent(..), isStreamingRequestJSON, reassembleToJSON)
import UniversalLLM (StreamingProtocol, extractStreamingContent, parseDelta, ProviderResponse, ModelName, Provider, ComposableProvider, ModelConfig)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

-- | Convert a 'StreamingContent' to a 'StreamEvent'.
contentToEvent :: StreamingContent -> StreamEvent
contentToEvent (StreamingText t)      = StreamText t
contentToEvent (StreamingReasoning t) = StreamThinking t

-- | Extract 'StreamEvent's from a decoded SSE payload using the protocol's
-- 'StreamingProtocol' instance.
extractEvents :: forall model. EnableStreaming model => Value -> [StreamEvent]
extractEvents v =
  case Aeson.encode v of
    bytes ->
      case parseDelta @(ProviderResponse model) (BSL.toStrict bytes) of
        Nothing    -> []
        Just delta -> map contentToEvent (extractStreamingContent @(ProviderResponse model) delta)

-- | Intercept 'RestAPI p' and transparently route LLM streaming requests
-- through 'HTTPStreaming', reassembling the SSE response into a normal
-- response body and emitting 'StreamEvent's via 'StreamChunk' as tokens arrive.
--
-- Detection and reassembly are derived from the 'EnableStreaming model'
-- typeclass. 'StreamChunk StreamEvent' must be provided by the caller —
-- use 'ignoreChunks' for non-interactive use, or a TUI handler for progress.
--
-- Usage (innermost to outermost):
--
-- > httpIO . httpStreamingIO . ignoreChunks . llmStreamingRestAPI @model p . restapiHTTP p . llmRetry . interpretLLM @p
llmStreamingRestAPI :: forall model p r a.
  ( EnableStreaming model
  , RestEndpoint p
  , Members '[RestAPI p, HTTPStreaming, StreamChunk StreamEvent, Fail] r
  )
  => p
  -> Sem r a
  -> Sem r a
llmStreamingRestAPI api =
  streamingRestAPI api
    (isStreamingRequestJSON @model)
    (reassembleToJSON @model)
    (extractEvents @model)

-- | Convenience interpreter: bundles 'restapiHTTP', 'llmStreamingRestAPI', and 'llmRetry'.
--
-- Equivalent to 'interpretLLMWith' but with streaming support baked in.
-- 'StreamChunk StreamEvent' must be provided by the caller.
interpretLLMWithStreaming :: forall p model s r a.
  ( ModelName model, Provider model, EnableStreaming model
  , RestEndpoint p, Default s
  , Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep] r
  )
  => p
  -> ComposableProvider model s
  -> model
  -> [ModelConfig model]
  -> Sem (LLM model : r) a
  -> Sem r a
interpretLLMWithStreaming api provider model configs action =
    restapiHTTP api $
    llmStreamingRestAPI @model api $
    llmRetry $
    interpretLLM @p provider model configs $
    raiseUnder @(RestAPI p)
    action
