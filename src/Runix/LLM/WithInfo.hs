{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Info-aware LLM interface.
--
-- This module re-exports everything from "Runix.LLM" but replaces 'queryLLM'
-- with a version that emits 'LLMInfo' events (streaming chunks, usage, etc.).
--
-- To use: change @import Runix.LLM@ to @import Runix.LLM.WithInfo@ — the
-- only difference is that 'queryLLM' here requires @Member LLMInfo r@ and
-- emits info events instead of discarding them.
module Runix.LLM.WithInfo
  ( -- * LLM Effect
    LLM(..)
  , queryLLM

    -- * LLM Info Effect
  , LLMInfo(..)
  , emitLLMInfo
  , swallowLLMInfo

    -- * Convenience functions
  , askLLM
  , askLLMWithReasoning
  , continueConversation
  , extractThinking
  , askLLMWithThinking

    -- * Re-exports from UniversalLLM
  , Message(..)
  , ModelConfig(..)
  , HasReasoning

    -- * Tool system (safe for Safe modules to use)
  , LLMTool(..)
  ) where

import Polysemy
import Polysemy.Fail

import Runix.LLM
  ( LLM(..)
  , LLMInfo(..)
  , emitLLMInfo
  , swallowLLMInfo
  , askLLM
  , askLLMWithReasoning
  , continueConversation
  , extractThinking
  , askLLMWithThinking
  , Message(..)
  , ModelConfig(..)
  , HasReasoning
  , LLMTool(..)
  )

-- | Query the LLM with info observation.
--
-- Info events (streaming chunks, usage, etc.) are emitted via the 'LLMInfo'
-- effect. The caller (or an enclosing interpreter) decides what to do with
-- them — display in a TUI, log, aggregate costs, etc.
--
-- For a version that silently discards info, use 'queryLLM' from "Runix.LLM".
queryLLM :: Members '[LLM model, LLMInfo, Fail] r
         => [ModelConfig model] -> [Message model] -> Sem r [Message model]
queryLLM configs msgs = do
    result <- send (QueryLLM configs msgs send)
    case result of
        Left err -> fail err
        Right messages -> return messages
