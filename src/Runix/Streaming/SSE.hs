{-# LANGUAGE OverloadedStrings #-}

-- | Server-Sent Events (SSE) parsing for streaming responses.
--
-- Re-exports the core SSE types and functions from the @sse-parser@ library.
-- The conduit-based interface is available in "Runix.Streaming.SSE.Conduit".
module Runix.Streaming.SSE
  ( -- * Core SSE types
    SSEEvent(..)
  , SSEParseResult(..)
    -- * Parsing functions
  , parseSSEChunks
  , splitOnEventTerminator
  , parseEvent
  , parseSSEComplete
  ) where

import Network.SSE
