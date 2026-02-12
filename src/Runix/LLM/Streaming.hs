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

-- | Streaming interpreter for "Runix.LLM"
--
-- This module provides a streaming interpreter for the standard 'LLM' effect.
-- The interpreter internally emits 'StreamChunk's as data arrives — agent code
-- only needs @Member (LLM model) r@ and is completely unaware of streaming.
--
-- To capture or redirect streaming chunks, reinterpret 'StreamChunk' at the
-- appropriate scope (e.g., per-widget in a TUI).
--
-- @
-- import Runix.LLM                        -- effect + queryLLM
-- import Runix.LLM.Streaming              -- streaming interpreter
-- @
module Runix.LLM.Streaming
  ( -- * Streaming interpreter (requires RestAPI already in effect row)
    interpretLLMStreaming
    -- * Convenience (bundles restapiHTTP + interpretLLMStreaming, for tests etc.)
  , interpretLLMStreamingWith

    -- * Re-exports from Runix.LLM (convenience for existing imports)
  , LLM(..)
  , queryLLM
  , Message(..)
  , ModelConfig(..)
  , HasReasoning
  , LLMTool(..)
  ) where

import Polysemy
import Polysemy.Fail
import Polysemy.State (State, evalState, get, put)
import Autodocodec (HasCodec, toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson (Value)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Default (Default, def)

import UniversalLLM (ModelName, ComposableProvider, toProviderRequest, fromProviderResponse, ProviderRequest, ProviderResponse)

import Runix.LLM (LLM(..), queryLLM, Message(..), ModelConfig(..), HasReasoning, LLMTool(..))
import Runix.HTTP (HTTP, HTTPStreaming, HTTPResponse(..), httpRequestStreaming)
import Runix.RestAPI (RestEndpoint(..), post, restapiHTTP, RestAPI, makeHTTPRequest)
import Runix.Streaming (StreamChunk)
import Runix.Streaming.SSE (reassembleSSE)
import Runix.Cancellation (Cancellation)
import Runix.LLM.Interpreter (ProviderProtocol(..))

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

-- | Send a non-streaming request via RestAPI
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
-- Streaming LLM Interpreter
-- ============================================================================

-- | Streaming interpreter for the standard 'LLM' effect.
--
-- Interprets 'QueryLLM' from "Runix.LLM". When @Streaming True@ is in configs,
-- uses 'HTTPStreaming' and emits raw chunks via 'StreamChunk'. When streaming
-- is disabled, falls back to non-streaming 'RestAPI'.
--
-- Agent code only needs @Member (LLM model) r@ — streaming is transparent.
interpretLLMStreaming :: forall p model s r a.
                          ( ModelName model
                          , HasCodec (ProviderRequest model)
                          , HasCodec (ProviderResponse model)
                          , Monoid (ProviderRequest model)
                          , ProviderProtocol (ProviderResponse model)
                          , RestEndpoint p
                          , Default s
                          , Members '[RestAPI p, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail] r
                          )
                       => p
                       -> ComposableProvider model s
                       -> model
                       -> Sem (LLM model : r) a
                       -> Sem r a
interpretLLMStreaming api composableProvider model action =
    evalState (model, def @s) $
    interpretLLMStreamingWithState @p api composableProvider $
    raiseUnder action

-- | Internal streaming interpreter with explicit state.
-- Uses first-order 'interpret' since 'LLM' is a first-order effect.
-- Streaming is handled by calling 'httpRequestStreaming' which internally
-- binds 'emitChunk' and 'isCanceled'.
interpretLLMStreamingWithState :: forall p model s r a.
                                   ( ModelName model
                                   , HasCodec (ProviderRequest model)
                                   , HasCodec (ProviderResponse model)
                                   , Monoid (ProviderRequest model)
                                   , ProviderProtocol (ProviderResponse model)
                                   , RestEndpoint p
                                   , Members '[RestAPI p, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail, State (model, s)] r
                                   )
                                => p
                                -> ComposableProvider model s
                                -> Sem (LLM model : r) a
                                -> Sem r a
interpretLLMStreamingWithState api composableProvider = interpret $ \case
    QueryLLM configs messages -> do
        (m, stackState) <- get @(model, s)
        let (stackState', request) = toProviderRequest composableProvider m configs stackState messages
        let requestValue = toJSONViaCodec request

        if isStreamingEnabled configs
            then do
                -- Streaming path: use httpRequestStreaming which emits StreamChunk internally
                let httpReq = makeHTTPRequest api "POST" (protocolEndpoint @(ProviderResponse model)) (Just requestValue)
                httpResp <- httpRequestStreaming httpReq
                let providerResponse = reassembleSSE (mergeStreamingDelta @(ProviderResponse model))
                                                      (emptyStreamingResponse @(ProviderResponse model))
                                                      (body httpResp)
                case fromProviderResponse composableProvider m configs stackState' providerResponse of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState'', resultMessages) -> do
                        put (m, stackState'')
                        return resultMessages
            else do
                -- Non-streaming path: use RestAPI
                resp <- sendRequest @p requestValue
                case fromProviderResponse composableProvider m configs stackState' resp of
                    Left err -> fail $ "LLM error: " ++ show err
                    Right (stackState'', resultMessages) -> do
                        put (m, stackState'')
                        return resultMessages

-- ============================================================================
-- Convenience Interpreter (bundles restapiHTTP)
-- ============================================================================

-- | Like 'interpretLLMWith' from "Runix.LLM.Interpreter", but with streaming.
-- Sets up RestAPI and delegates to 'interpretLLMStreaming'.
interpretLLMStreamingWith :: forall p model s r a.
                              ( ModelName model
                              , HasCodec (ProviderRequest model)
                              , HasCodec (ProviderResponse model)
                              , Monoid (ProviderRequest model)
                              , ProviderProtocol (ProviderResponse model)
                              , RestEndpoint p
                              , Default s
                              , Members '[HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation, Fail] r
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
