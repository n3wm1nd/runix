{-# LANGUAGE OverloadedStrings #-}

-- | Conduit-based SSE (Server-Sent Events) parsing utilities
--
-- Provides conduit transformers for parsing SSE streams.
-- Use these to process streaming HTTP responses from LLM providers.
module Runix.Streaming.SSE.Conduit
  ( -- * Conduit transformers
    parseSSEConduit
  ) where

import Conduit
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Runix.Streaming.SSE (SSEEvent(..), SSEParseResult(..), parseSSEChunks)

-- | Parse SSE format from ByteString chunks
--
-- Converts raw byte chunks into SSE events, properly handling chunk boundaries.
-- Maintains state across chunks to handle events split across HTTP chunk boundaries.
--
-- Usage:
-- @
-- source .| parseSSEConduit .| mapM_C processEvent
-- @
parseSSEConduit :: Monad m => ConduitT BS.ByteString SSEEvent m ()
parseSSEConduit = go BS.empty
  where
    go buffer = do
        mChunk <- await
        case mChunk of
            Nothing ->
                -- End of stream - if there's buffered data, it's incomplete
                return ()
            Just chunk -> do
                let input = buffer <> chunk
                    SSEParseResult events remainder = parseSSEChunks input
                mapM_ yield events
                go remainder
