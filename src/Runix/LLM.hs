{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Runix.LLM
  ( -- * LLM Effect
    LLM(..)
  , queryLLM

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
import UniversalLLM.Core.Tools (LLMTool(..))

-- Re-export orphan instances for Tool system
-- This ensures anyone using LLM effects automatically gets the instances they need
import Runix.LLM.ToolInstances ()

-- The LLM effect now works directly with universal-llm messages
-- History management is MANUAL - just append and pass around
data LLM model (m :: Type -> Type) a where
    QueryLLM :: [ModelConfig model]       -- Temperature, MaxTokens, Tools, etc.
             -> [Message model]            -- History (append manually)
             -> LLM model m [Message model]  -- Response messages

makeSem ''LLM

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
continueConversation :: Member (LLM model) r
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
