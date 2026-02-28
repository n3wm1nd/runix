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
    LLMStream(..)
  , LLMStreamResult(..)
  , fetchStreamEvent
  , cancelLLMStream
  , StreamId(..)
    -- * Streaming events
  , StreamEvent(..)
  ) where

import Polysemy
import Data.Kind (Type)
import Data.Text (Text)

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

-- | Stream identifier for LLM streaming
newtype StreamId = StreamId Int
    deriving (Eq, Ord, Show)

-- | Internal LLM streaming effect for managing streams
-- Users don't interact with this directly - use LLMStreamResult instead
data LLMStream model (m :: Type -> Type) a where
    StartLLMStreamInternal :: [ModelConfig model]
                           -> [Message model]
                           -> LLMStream model m (Either String StreamId)
    FetchStreamEventInternal :: StreamId
                             -> LLMStream model m (Maybe StreamEvent)
    GetAccumulatedResult :: StreamId
                         -> LLMStream model m (Either String [Message model])
    CancelLLMStreamInternal :: StreamId
                            -> LLMStream model m ()

-- | Public effect for consuming LLM streams
-- Provided by startLLMStream which manages the stream lifecycle
data LLMStreamResult model (m :: Type -> Type) a where
    FetchStreamEvent :: LLMStreamResult model m (Maybe StreamEvent)
    CancelLLMStream :: LLMStreamResult model m ()

-- | Fetch the next stream event
fetchStreamEvent :: Member (LLMStreamResult model) r => Sem r (Maybe StreamEvent)
fetchStreamEvent = send FetchStreamEvent

-- | Cancel the current stream
cancelLLMStream :: Member (LLMStreamResult model) r => Sem r ()
cancelLLMStream = send CancelLLMStream
