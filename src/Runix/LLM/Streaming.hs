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
--
-- Also defines 'StreamEvent', the protocol-agnostic event type emitted during streaming.
module Runix.LLM.Streaming
  ( llmStreamingRestAPI
  , StreamEvent(..)
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Data.Text (Text)

import Runix.HTTP (HTTPStreaming)
import Runix.RestAPI (RestAPI, RestEndpoint)
import Runix.RestAPI.Streaming (streamingRestAPI)
import Runix.StreamChunk (StreamChunk)

import UniversalLLM (EnableStreaming, StreamingContent(..), isStreamingRequestJSON, reassembleToJSON)
import UniversalLLM (extractStreamingContent, parseDelta, ProviderResponse)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

-- | Protocol-agnostic events emitted during LLM streaming.
data StreamEvent
  = StreamStarted                    -- ^ Stream has begun
  | StreamText Text                  -- ^ Assistant text chunk
  | StreamThinking Text              -- ^ Reasoning/thinking chunk
  | StreamToolCallStarted Text Text  -- ^ Tool call started (id, name)
  | StreamToolCallArgument Text Text -- ^ Tool call argument chunk (id, partial args)
  | StreamToolCallComplete Text      -- ^ Tool call finished (id)
  | StreamError Text                 -- ^ Error occurred
  | StreamDone                       -- ^ Stream completed normally
  deriving (Show, Eq)

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
    (Just StreamStarted)
    (Just StreamDone)

