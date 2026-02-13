{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core LLM effect.
--
-- This module provides the standard LLM interface where info events
-- (streaming chunks, usage, etc.) are silently discarded.
--
-- For an info-aware interface where streaming chunks are observable,
-- import "Runix.LLM.WithInfo" instead — it re-exports everything from
-- this module but replaces 'queryLLM' with a version that emits 'LLMInfo'.
module Runix.LLM
  ( -- * LLM Effect
    LLM(..)
  , queryLLM

    -- * LLM Info Effect (used internally and by Runix.LLM.WithInfo)
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
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T

-- Re-export universal-llm types directly
import UniversalLLM
  ( Message(..)
  , ModelConfig(..)
  , HasReasoning
  )

-- Re-export tool system
import UniversalLLM.Tools (LLMTool(..))

-- Re-export orphan instances for Tool system
-- This ensures anyone using LLM effects automatically gets the instances they need
import Runix.LLM.ToolInstances ()

import Runix.Streaming.SSE (StreamingContent)

-- | The LLM info effect — a write-only sideband for metadata emitted during queries.
--
-- Streaming chunks, usage stats, cost information, warnings, etc. are delivered
-- through this effect. The constructors carry only data, so the interpreter can
-- produce values in any monadic context and the call-site callback simply
-- re-sends them via @send@ in the caller's context.
data LLMInfo (m :: Type -> Type) a where
    EmitLLMInfo :: StreamingContent -> LLMInfo m ()

makeSem ''LLMInfo

-- | Swallow all LLMInfo events (noop interpreter).
swallowLLMInfo :: Sem (LLMInfo : r) a -> Sem r a
swallowLLMInfo = interpret $ \case
    EmitLLMInfo _ -> pure ()

-- | The LLM effect — higher-order due to the info callback.
--
-- The callback receives 'LLMInfo' values from the interpreter (produced in
-- the tactical row) and re-sends them in the caller's monadic context.
-- This is structural plumbing — use 'queryLLM' (or 'queryLLM' from
-- "Runix.LLM.WithInfo" for info-aware queries) instead.
data LLM model (m :: Type -> Type) a where
    QueryLLM :: [ModelConfig model]                    -- ^ Temperature, MaxTokens, Tools, etc.
             -> [Message model]                         -- ^ History (append manually)
             -> (LLMInfo m () -> m ())                  -- ^ Callback: re-sends LLMInfo values in caller's context
             -> LLM model m (Either String [Message model])  -- ^ Response messages or error

-- | Query the LLM with no info observation (standard agent usage).
--
-- Info events (streaming chunks, usage, etc.) are silently discarded.
-- For an info-aware version, use 'queryLLM' from "Runix.LLM.WithInfo".
queryLLM :: Members '[LLM model, Fail] r => [ModelConfig model] -> [Message model] -> Sem r [Message model]
queryLLM configs msgs = do
    result <- send (QueryLLM configs msgs (\_ -> pure ()))
    case result of
        Left err -> fail err
        Right messages -> return messages

-- Convenience functions for simple queries
askLLM :: Members '[LLM model, Fail] r => Text -> Sem r Text
askLLM query = do
    response <- queryLLM [] [UserText query]  -- No history, simple query
    case [t | AssistantText t <- response] of
        [text] -> return text
        _ -> fail "Expected single text response"

-- Get response with reasoning/thinking
askLLMWithReasoning :: (Members '[LLM model, Fail] r, HasReasoning model)
                    => Text -> Sem r (Text, Maybe Text)
askLLMWithReasoning query = do
    response <- queryLLM [Reasoning True] [UserText query]
    let text = [t | AssistantText t <- response]
    let reasoning = [r | AssistantReasoning r <- response]
    return (mconcat text, listToMaybe reasoning)
  where
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

-- Continue conversation - manual history management
continueConversation :: Members '[LLM model, Fail] r
                     => [ModelConfig model]
                     -> [Message model]  -- YOU manage this
                     -> Message model     -- New message
                     -> Sem r [Message model]
continueConversation configs history newMsg = do
    queryLLM configs (history ++ [newMsg])

-- Pure function to extract thinking from response text (for backward compatibility)
-- This is now deprecated in favor of using AssistantReasoning messages directly
extractThinking :: Text -> (Text, Maybe Text)
extractThinking input =
    case T.splitOn (T.pack "<think>") input of
        [beforeThink] -> (beforeThink, Nothing)  -- No thinking tags
        [beforeThink, rest] ->
            case T.splitOn (T.pack "</think>") rest of
                [thinking, afterThink] ->
                    let cleanContent = T.strip (beforeThink <> afterThink)
                        cleanThinking = T.strip thinking
                    in (cleanContent, if T.null cleanThinking then Nothing else Just cleanThinking)
                _ -> (input, Nothing)  -- Malformed tags, return original
        _ -> (input, Nothing)  -- Multiple thinking tags, return original

-- Legacy compatibility: Get response with thinking content (using text extraction)
askLLMWithThinking :: Members '[LLM model, Fail] r => Text -> Sem r (Text, Maybe Text)
askLLMWithThinking query = do
    response <- queryLLM [] [UserText query]
    case [t | AssistantText t <- response] of
        [text] -> return $ extractThinking text
        _ -> fail "Expected single text response"
