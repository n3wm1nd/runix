{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Context management for LLM conversations
--
-- This module provides utilities for:
-- - Token counting (estimating context size)
-- - Context compaction (using LLM to summarize history)
module Runix.LLM.Context
  ( -- * Types
    TokenCount(..)
    -- * Token counting
  , tokenCount
  , estimateTokens
    -- * Context compaction
  , compact
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail
import UniversalLLM (Message(..), HasTools, SupportsSystemPrompt, ProviderOf, ModelConfig(..))
import Runix.LLM (LLM(..), queryLLM)
import Data.Aeson (Value)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Token count - represents the number of tokens in a context
newtype TokenCount = TokenCount Int
  deriving (Show, Eq, Ord, Num)

--------------------------------------------------------------------------------
-- Token Counting
--------------------------------------------------------------------------------

-- | Count tokens in a list of messages
--
-- This uses an approximation: ~4 characters per token for English text.
-- This is a conservative estimate that works reasonably well for Claude models.
--
-- For more accurate counting, you would need to use the actual tokenizer,
-- but that requires model-specific dependencies.
tokenCount :: [Message model] -> TokenCount
tokenCount msgs = TokenCount $ sum $ map messageTokens msgs

-- | Estimate tokens in a single message
messageTokens :: Message model -> Int
messageTokens msg = case msg of
  UserText txt -> estimateTokens txt
  UserImage url desc -> estimateTokens url + estimateTokens desc
  UserRequestJSON txt schema -> estimateTokens txt + estimateValueTokens schema
  AssistantText txt -> estimateTokens txt
  AssistantReasoning txt -> estimateTokens txt
  AssistantTool toolCall -> estimateToolCallTokens toolCall
  AssistantJSON val -> estimateValueTokens val
  SystemText txt -> estimateTokens txt
  ToolResultMsg toolResult -> estimateToolResultTokens toolResult

-- | Estimate tokens in text (4 chars ≈ 1 token)
estimateTokens :: Text -> Int
estimateTokens txt = (T.length txt + 3) `div` 4

-- | Estimate tokens in a JSON Value
estimateValueTokens :: Value -> Int
estimateValueTokens val =
  -- Simple approximation: count characters in JSON representation
  -- In reality, structured data often tokenizes more efficiently
  let jsonStr = T.pack $ show val
  in estimateTokens jsonStr

-- | Estimate tokens in a tool call (includes structure overhead)
estimateToolCallTokens :: toolcall -> Int
estimateToolCallTokens _ = 50  -- Conservative estimate for tool call structure

-- | Estimate tokens in a tool result (includes structure overhead)
estimateToolResultTokens :: toolresult -> Int
estimateToolResultTokens _ = 100  -- Conservative estimate for tool result structure

--------------------------------------------------------------------------------
-- Context Compaction
--------------------------------------------------------------------------------

-- | Compact a message history by using the LLM to summarize older messages
--
-- This preserves recent messages but summarizes older context to reduce token count.
-- The compaction strategy:
-- 1. Keep the most recent N messages (e.g., last 10)
-- 2. Summarize everything before that into a system message
-- 3. Return: [SystemMessage (summary)] ++ recent messages
--
-- This requires LLM effect to generate the summary.
compact :: forall model r.
           ( Member (LLM model) r
           , Member Fail r
           , HasTools model
           , SupportsSystemPrompt (ProviderOf model)
           )
        => [ModelConfig model]  -- ^ Model configs for the compaction query
        -> Int                   -- ^ Number of recent messages to keep unmodified
        -> [Message model]       -- ^ Original message history
        -> Sem r [Message model] -- ^ Compacted history
compact configs keepRecent history
  | length history <= keepRecent = return history  -- No compaction needed
  | otherwise = do
      let (toSummarize, toKeep) = splitAt (length history - keepRecent) history

      -- Build a prompt asking the LLM to summarize the older messages
      let summaryPrompt = UserText $ T.unlines
            [ "Please provide a concise summary of the following conversation history."
            , "Focus on key decisions, important context, and unresolved issues."
            , "This summary will be used to maintain context in a continued conversation."
            , ""
            , formatMessagesForSummary toSummarize
            ]

      -- Query LLM for summary
      summaryMsgs <- queryLLM configs [summaryPrompt]

      -- Extract the summary text from the response
      let summaryText = extractSummaryText summaryMsgs

      -- Return: summary as system message + recent messages
      return $ [SystemText summaryText] ++ toKeep

-- | Format messages for the summary prompt
formatMessagesForSummary :: [Message model] -> Text
formatMessagesForSummary msgs = T.intercalate "\n\n" $ map formatMessage msgs
  where
    formatMessage :: Message model -> Text
    formatMessage (UserText txt) = "User: " <> txt
    formatMessage (AssistantText txt) = "Assistant: " <> txt
    formatMessage (AssistantReasoning txt) = "Assistant (thinking): " <> txt
    formatMessage (SystemText txt) = "System: " <> txt
    formatMessage (UserImage _ desc) = "User: [Image: " <> desc <> "]"
    formatMessage (UserRequestJSON txt _) = "User: " <> txt
    formatMessage (AssistantTool _) = "Assistant: [Tool call]"
    formatMessage (AssistantJSON _) = "Assistant: [JSON response]"
    formatMessage (ToolResultMsg _) = "Tool result: [...]"

-- | Extract summary text from LLM response
extractSummaryText :: [Message model] -> Text
extractSummaryText msgs = T.unlines $ map extract msgs
  where
    extract (AssistantText txt) = txt
    extract (AssistantReasoning txt) = txt
    extract _ = ""
