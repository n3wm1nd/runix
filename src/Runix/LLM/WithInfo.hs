{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Info-aware LLM interface.
--
-- This module re-exports everything from "Runix.LLM" but replaces 'queryLLM'
-- with a version whose callback is a no-op placeholder — the real delivery
-- mechanism is an 'interceptH' layer above that substitutes its own callback.
--
-- To use: change @import Runix.LLM@ to @import Runix.LLM.WithInfo@ — the
-- only difference is that 'queryLLM' here uses a no-op callback, signalling
-- that an interceptor above should replace it with a real one.
module Runix.LLM.WithInfo
  ( -- * LLM Effect
    LLM(..)
  , queryLLM

    -- * LLM Info (plain data)
  , LLMInfo(..)

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
-- The callback here is a no-op placeholder. The actual streaming delivery
-- is handled by an 'interceptH' layer above the interpreter (e.g.
-- 'interpretAsWidget' in the TUI) that replaces the callback with one
-- routing chunks to the appropriate output.
--
-- For a version that silently discards info, use 'queryLLM' from "Runix.LLM".
queryLLM :: Members '[LLM model, Fail] r
         => [ModelConfig model] -> [Message model] -> Sem r [Message model]
queryLLM configs msgs = do
    result <- send (QueryLLM configs msgs (\_ -> pure ()))
    case result of
        Left err -> fail err
        Right messages -> return messages
