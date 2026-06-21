{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Transparent streaming upgrade for RestAPI.
--
-- 'streamingRestAPI' is a generic interceptor that sits in front of any
-- 'RestAPI p' interpreter and transparently routes qualifying requests through
-- 'HTTPStreaming' instead of the normal 'HTTP' path.
--
-- The caller supplies three protocol-specific functions:
--
--  * a predicate (@Value -> Bool@) on the encoded request body;
--  * a reassembly function (@[Value] -> Value@) that folds SSE payloads into
--    a single response body; and
--  * an extraction function (@Value -> [event]@) that maps each SSE payload
--    to zero or more events to emit via 'StreamChunk'.
--
-- Non-streaming requests are forwarded to the underlying 'RestAPI p' unchanged.
-- 'StreamChunk event' must be provided by the caller — use 'ignoreChunks' for
-- non-interactive contexts, or a UI handler to observe progress.
--
-- LLM-specific wiring lives in "Runix.LLM.Streaming".
module Runix.RestAPI.Streaming
  ( streamingRestAPI
  ) where

import Polysemy
import Polysemy.Fail
import Data.Aeson (Value, ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Runix.HTTP (HTTPStreaming, HTTPStreamResult(..))
import Runix.RestAPI (RestAPI(..), RestEndpoint(..), RestError(..), Endpoint(..), makeHTTPRequest)
import Runix.StreamChunk (StreamChunk, emitChunk)
import qualified Runix.Streaming as Streaming
import Runix.Streaming.SSE (SSEEvent(..), SSEParseResult(..), parseSSEChunks)

-- | Intercept 'RestAPI p' and route streaming requests through 'HTTPStreaming',
-- emitting per-payload events via 'StreamChunk' as chunks arrive.
--
-- 'StreamChunk event' is required in the effect row — the caller provides
-- an interpreter for it ('ignoreChunks' for non-interactive use, a UI handler
-- for progress display). This keeps 'streamingRestAPI' a pure interceptor
-- rather than an eliminator.
--
-- Typical stack (innermost to outermost):
--
-- > httpIO . httpStreamingIO . ignoreChunks . llmStreamingRestAPI @model . restapiHTTP p . llmRetry . interpretLLM @p
streamingRestAPI :: forall p event r a.
  ( RestEndpoint p
  , Members '[RestAPI p, HTTPStreaming, StreamChunk event, Fail] r
  )
  => p
  -> (Value -> Bool)        -- ^ Is this a streaming request?
  -> ([Value] -> Value)     -- ^ Reassemble SSE payloads into a response body
  -> (Value -> [event])     -- ^ Extract events to emit from each SSE payload
  -> Maybe event            -- ^ Event to emit when the stream starts
  -> Maybe event            -- ^ Event to emit when the stream ends
  -> Sem r a
  -> Sem r a
streamingRestAPI api isStreaming reassemble extract onStart onEnd = intercept @(RestAPI p) $ \case
  RestRequest method endpoint (maybeBody :: Maybe reqBody) ->
    case fmap encode maybeBody >>= decode of
      Just (bodyVal :: Value) | isStreaming bodyVal ->
        runStreamingRequest api method endpoint maybeBody reassemble extract onStart onEnd
      _ ->
        send (RestRequest method endpoint maybeBody)

-- | Send a request via HTTPStreaming, emit events per payload, and return the
-- reassembled result as a normal REST response.
runStreamingRequest :: forall p reqBody respBody event r.
  ( RestEndpoint p
  , ToJSON reqBody
  , FromJSON respBody
  , Members '[HTTPStreaming, StreamChunk event, Fail] r
  )
  => p
  -> String
  -> Endpoint
  -> Maybe reqBody
  -> ([Value] -> Value)
  -> (Value -> [event])
  -> Maybe event
  -> Maybe event
  -> Sem r (Either RestError respBody)
runStreamingRequest api method endpoint maybeBody reassemble extract onStart onEnd = do
  let httpReq = makeHTTPRequest api method endpoint maybeBody
  streamResult <- send (Streaming.StartStream httpReq)
  case streamResult of
    Left err -> return $ Left $ ConnectionError err
    Right streamId -> do
      mapM_ emitChunk onStart
      sseValues <- consumeAndEmit streamId extract
      mapM_ emitChunk onEnd
      httpResult <- send (Streaming.CloseStream streamId)
      if httpResult.streamStatusCode < 200 || httpResult.streamStatusCode >= 300
        then return $ Left $ HttpError
               httpResult.streamStatusCode
               httpResult.streamHeaders
               httpResult.streamBody
        else
          let responseVal = reassemble sseValues
          in case decode (encode responseVal) of
               Just result -> return $ Right result
               Nothing     -> return $ Left $
                 ParseError "SSE reassembly produced undecodable JSON" (encode responseVal)

-- | Consume all chunks from an HTTPStreaming stream, parse SSE events
-- incrementally (carrying the remainder across chunk boundaries),
-- emit extracted events via 'StreamChunk', and return all decoded payloads
-- for reassembly.
consumeAndEmit :: forall event r.
  Members '[HTTPStreaming, StreamChunk event, Fail] r
  => Streaming.StreamId
  -> (Value -> [event])
  -> Sem r [Value]
consumeAndEmit streamId extract = go BS.empty []
  where
    go remainder valueAcc = do
      mChunk <- send (Streaming.FetchItem streamId)
      case mChunk of
        Nothing    -> return (reverse valueAcc)
        Just chunk -> do
          let SSEParseResult events nextRemainder = parseSSEChunks (remainder <> chunk)
              newValues = [val | event <- events
                               , Just val <- [decode (BSL.fromStrict (sseEventData event))]]
          mapM_ (\v -> mapM_ emitChunk (extract v)) newValues
          go nextRemainder (reverse newValues ++ valueAcc)
