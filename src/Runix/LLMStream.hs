{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | LLM streaming effect for interactive/abortable generation.
--
-- Returns structured, protocol-agnostic streaming events. Events represent
-- what's happening in the stream (thinking, text generation, tool calls, etc.)
-- independent of the provider's wire format.
--
-- Use this when you need:
--
-- * Interactive generation (display tokens as they arrive)
-- * Abort capability (stop mid-generation, keep partial results)
-- * Real-time event handling (thinking, tool calls, etc.)
--
-- For standard request/response, use the 'LLM' effect instead.
module Runix.LLMStream
  ( -- * Effects
    LLMStreaming
  , LLMStreamResult
    -- * Streaming events
  , StreamEvent(..)
  ) where

import Data.Text (Text)
import Runix.Streaming (Streaming, StreamResult)

-- Re-export universal-llm types
import UniversalLLM (Message(..), ModelConfig(..))

-- | Events that occur during LLM streaming
--
-- Protocol-agnostic representation of what's happening in the stream.
-- These map to provider-specific formats but hide those details.
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


-- | LLM streaming using the generic Streaming abstraction
-- Streams: StreamEvent
-- Returns: Either String [Message model] (accumulated messages)
-- Config: ([ModelConfig model], [Message model])
type LLMStreaming model = Streaming StreamEvent (Either String [Message model]) ([ModelConfig model], [Message model])

-- | Public result type for LLM streaming
type LLMStreamResult model = StreamResult StreamEvent
