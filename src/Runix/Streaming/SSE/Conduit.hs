{-# LANGUAGE OverloadedStrings #-}

-- | Conduit-based SSE (Server-Sent Events) parsing utilities
--
-- Provides conduit transformers for parsing SSE streams and extracting content.
-- Use these to process streaming HTTP responses from LLM providers.
module Runix.Streaming.SSE.Conduit
  ( -- * Conduit transformers
    parseSSEConduit
  , extractContentConduit
  , reassembleConduit
    -- * Re-exports from SSE module
  , StreamingContent(..)
  , parseSSE
  , extractContentFromChunk
  ) where

import Conduit
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Runix.Streaming.SSE (StreamingContent(..), parseSSE, extractContentFromChunk)

-- | Parse SSE format from ByteString chunks
--
-- Converts raw byte chunks into JSON values by parsing SSE "data: {...}" lines.
-- Buffers partial lines across chunk boundaries.
--
-- Usage:
-- @
-- source .| parseSSEConduit .| mapM_C processValue
-- @
parseSSEConduit :: Monad m => ConduitT BS.ByteString Value m ()
parseSSEConduit = do
    -- Convert strict ByteStrings to lazy for parseSSE
    chunk <- await
    case chunk of
        Nothing -> return ()
        Just bs -> do
            let values = parseSSE (BSL.fromStrict bs)
            mapM_ yield values
            parseSSEConduit

-- | Extract streaming content from SSE chunks
--
-- Parses SSE format and extracts text/reasoning content from provider-specific formats.
-- Handles both Anthropic and OpenAI streaming formats.
--
-- Usage:
-- @
-- source .| extractContentConduit .| mapM_C displayContent
-- @
extractContentConduit :: Monad m => ConduitT BS.ByteString StreamingContent m ()
extractContentConduit = do
    chunk <- await
    case chunk of
        Nothing -> return ()
        Just bs -> do
            let contents = extractContentFromChunk bs
            mapM_ yield contents
            extractContentConduit

-- | Reassemble SSE stream into final response
--
-- Applies a delta merger function to accumulate streaming chunks into a final response.
-- The merger knows how to combine provider-specific deltas.
--
-- Usage:
-- @
-- chunks <- runConduit $ source .| parseSSEConduit .| sinkList
-- let finalResponse = reassembleConduit mergeFunction initialState chunks
-- @
reassembleConduit :: (response -> Value -> response)  -- ^ Delta merger
                  -> response                          -- ^ Initial state
                  -> [Value]                           -- ^ SSE chunks
                  -> response                          -- ^ Final response
reassembleConduit = foldl
