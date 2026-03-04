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
  , compact             -- ^ LLM-based summarization (requires LLM effect)
  , compactQuick        -- ^ Quick offline compression (strip tool results & system reminders)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail
import UniversalLLM (Message(..), HasTools, SupportsSystemPrompt, ProviderOf, ModelConfig(..), ToolCall(..), ToolResult(..))
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

-- | Count tokens in message history
--
-- This uses an approximation: ~4 characters per token for English text.
-- This is a conservative estimate that works reasonably well for Claude models.
--
-- Note: This only counts the messages themselves, not system prompts or
-- tool definitions passed via ModelConfig. To get the full context size:
--   tokenCount msgs + estimateTokens systemPromptText + estimateTokens toolDefsText
--
-- For more accurate counting, you would need to use the actual tokenizer,
-- but that requires model-specific dependencies.
tokenCount :: [Message model] -> TokenCount
tokenCount msgs = TokenCount $ sum $ map messageTokens msgs

-- | Estimate tokens in a single message (internal helper)
messageTokens :: Message model -> Int
messageTokens msg = case msg of
  UserText txt -> unTC $ estimateTokens txt
  UserImage url desc -> unTC (estimateTokens url) + unTC (estimateTokens desc)
  UserRequestJSON txt schema -> unTC (estimateTokens txt) + estimateValueTokens schema
  AssistantText txt -> unTC $ estimateTokens txt
  AssistantReasoning txt -> unTC $ estimateTokens txt
  AssistantTool toolCall -> estimateToolCallTokens toolCall
  AssistantJSON val -> estimateValueTokens val
  SystemText txt -> unTC $ estimateTokens txt
  ToolResultMsg toolResult -> estimateToolResultTokens toolResult
  where
    unTC (TokenCount n) = n

-- | Estimate tokens in raw text (4 chars ≈ 1 token)
--
-- Use this for counting tokens in system prompts, tool definitions,
-- or any other text content that's not in the message history.
estimateTokens :: Text -> TokenCount
estimateTokens txt = TokenCount $ (T.length txt + 3) `div` 4

-- | Estimate tokens in a JSON Value (internal helper)
estimateValueTokens :: Value -> Int
estimateValueTokens val =
  -- Simple approximation: count characters in JSON representation
  -- In reality, structured data often tokenizes more efficiently
  let jsonStr = T.pack $ show val
      TokenCount n = estimateTokens jsonStr
  in n

-- | Estimate tokens in a tool call (internal helper)
estimateToolCallTokens :: ToolCall -> Int
estimateToolCallTokens (ToolCall tcId tcName tcArgs) =
  -- Tool call ID + name + JSON arguments
  let TokenCount idTokens = estimateTokens tcId
      TokenCount nameTokens = estimateTokens tcName
      argsTokens = estimateValueTokens tcArgs
  in idTokens + nameTokens + argsTokens + 10  -- +10 for structure
estimateToolCallTokens (InvalidToolCall tcId tcName rawArgs errMsg) =
  -- Invalid tool call: ID + name + raw args + error message
  let TokenCount idTokens = estimateTokens tcId
      TokenCount nameTokens = estimateTokens tcName
      TokenCount argsTokens = estimateTokens rawArgs
      TokenCount errTokens = estimateTokens errMsg
  in idTokens + nameTokens + argsTokens + errTokens + 10

-- | Estimate tokens in a tool result (internal helper)
estimateToolResultTokens :: ToolResult -> Int
estimateToolResultTokens (ToolResult toolCall output) =
  let callTokens = estimateToolCallTokens toolCall
      outputTokens = case output of
        Left errText -> let TokenCount n = estimateTokens errText in n
        Right val -> estimateValueTokens val
  in callTokens + outputTokens + 10  -- +10 for result structure

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

--------------------------------------------------------------------------------
-- Quick Offline Compaction
--------------------------------------------------------------------------------

-- | Quick offline compression: strip tool results and system reminders
--
-- This is a lightweight, fast compression that:
-- 1. Replaces AssistantTool + ToolResultMsg pairs with a SystemText reminder
--    showing just the tool name and args (no large results like file contents)
-- 2. Strips out existing SystemText messages (ephemeral reminders that can be
--    inferred from context)
-- 3. Adds a note that tool results were stripped (files may need re-reading)
--
-- This can significantly reduce token count for conversations with many
-- tool calls, especially file reads.
--
-- Unlike 'compact', this is pure and doesn't require an LLM call.
compactQuick :: [Message model] -> [Message model]
compactQuick msgs =
  let -- First pass: strip system reminders and replace tool calls
      compressed = compressMessages msgs
      -- Add header if we actually compressed anything
      hasToolCalls = any isToolCall msgs
      header = if hasToolCalls
               then [SystemText "Note: Tool results have been stripped to reduce context size. Files may need to be re-read if their contents are needed."]
               else []
  in header ++ compressed
  where
    isToolCall (AssistantTool _) = True
    isToolCall _ = False

-- | Compress messages by stripping system reminders and replacing tool calls
compressMessages :: [Message model] -> [Message model]
compressMessages [] = []
compressMessages (msg:rest) = case msg of
  -- Strip existing system text, EXCEPT our tool call placeholders
  SystemText txt
    | isToolPlaceholder txt -> msg : compressMessages rest  -- Keep tool placeholders
    | otherwise -> compressMessages rest  -- Strip ephemeral reminders

  -- Replace tool call + result with a compact reminder
  AssistantTool toolCall -> case rest of
    -- Tool call followed by result: replace both with summary
    (ToolResultMsg toolResult : rest') ->
      let summary = summarizeToolCall toolCall toolResult
      in SystemText summary : compressMessages rest'
    -- Tool call without result: keep it but note it
    _ -> SystemText (summarizeToolCallOnly toolCall) : compressMessages rest

  -- Skip standalone tool results (shouldn't happen, but handle it)
  ToolResultMsg _ -> compressMessages rest

  -- Keep all other messages as-is
  _ -> msg : compressMessages rest

-- | Check if a system text is one of our tool call placeholders
isToolPlaceholder :: Text -> Bool
isToolPlaceholder txt =
  T.isPrefixOf "[Tool:" txt || T.isPrefixOf "[Tool call pending:" txt || T.isPrefixOf "[Invalid tool" txt

-- | Summarize a tool call and its result into a compact text reminder
summarizeToolCall :: ToolCall -> ToolResult -> Text
summarizeToolCall toolCall _toolResult =
  -- Just show the call, not the result (which might be huge)
  case toolCall of
    ToolCall tcId tcName tcArgs ->
      "[Tool: " <> tcName <> " (id: " <> tcId <> ") with args: " <> T.pack (show tcArgs) <> "]"
    InvalidToolCall tcId tcName rawArgs errMsg ->
      "[Invalid tool: " <> tcName <> " (id: " <> tcId <> ") - Error: " <> errMsg <> "]"

-- | Summarize a tool call without result
summarizeToolCallOnly :: ToolCall -> Text
summarizeToolCallOnly toolCall =
  case toolCall of
    ToolCall tcId tcName tcArgs ->
      "[Tool call pending: " <> tcName <> " (id: " <> tcId <> ") with args: " <> T.pack (show tcArgs) <> "]"
    InvalidToolCall tcId tcName _ errMsg ->
      "[Invalid tool call: " <> tcName <> " (id: " <> tcId <> ") - Error: " <> errMsg <> "]"
