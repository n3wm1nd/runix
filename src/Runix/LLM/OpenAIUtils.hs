{-# LANGUAGE OverloadedStrings #-}

module Runix.LLM.OpenAIUtils (
    normalizeOpenAIMessages,
    userInputToOpenAI,
    toolResultsToOpenAI,
    messageToOpenAI,
    openAIToToolCall,
    toolCallToOpenAI,
    toolParametersToText
) where

import qualified Runix.LLM.Effects as Effects
import qualified Runix.LLM.Protocol.OpenAICompatible as OpenAICompatible
import Runix.LLM.Effects (MessageHistory, Message(..), UserInput(..), AssistantOutput(..), ToolCall(..), ToolResult(..))
import Runix.LLM.Protocol.OpenAICompatible (OpenAIMessage(..), OpenAIToolCall(..), OpenAIFunctionCall(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Maybe (fromMaybe)

-- OpenAI-style normalization (strict alternation, merge adjacent messages)
normalizeOpenAIMessages :: MessageHistory -> MessageHistory
normalizeOpenAIMessages = go where
  go [] = []

  -- Merge consecutive UserMessage with UserQuery
  go (UserMessage (UserQuery q1) : UserMessage (UserQuery q2) : rest) =
    go (UserMessage (UserQuery (q1 <> "\n\n" <> q2)) : rest)

  -- Merge consecutive UserMessage with ToolCallResults
  go (UserMessage (ToolCallResults r1) : UserMessage (ToolCallResults r2) : rest) =
    go (UserMessage (ToolCallResults (r1 ++ r2)) : rest)

  -- Merge consecutive AssistantMessage responses (combine content and tool calls)
  go (AssistantMessage (AssistantResponse t1 tools1) : AssistantMessage (AssistantResponse t2 tools2) : rest) =
    let combinedText = if T.null t1 then t2 else if T.null t2 then t1 else t1 <> "\n\n" <> t2
        combinedTools = tools1 ++ tools2
    in go (AssistantMessage (AssistantResponse combinedText combinedTools) : rest)

  -- Cancel outstanding tool calls if user asks new question
  go (AssistantMessage (AssistantResponse text (_:_)) : UserMessage (UserQuery newQ) : rest) =
    go (UserMessage (UserQuery newQ) : rest)  -- Drop incomplete tool interaction

  -- Keep other messages as-is
  go (msg : rest) = msg : go rest

-- Convert UserInput to OpenAI message format
userInputToOpenAI :: UserInput -> OpenAIMessage
userInputToOpenAI (UserQuery text) = OpenAIMessage "user" (Just text) Nothing Nothing
userInputToOpenAI (ToolCallResults _) =
    -- ToolCallResults should not be converted to a single message
    -- They need to be handled separately as multiple tool messages
    error "ToolCallResults should be converted using toolResultsToOpenAI"

-- Convert tool results to multiple OpenAI tool messages
toolResultsToOpenAI :: [ToolResult] -> [OpenAIMessage]
toolResultsToOpenAI results =
    map (\(ToolResult content callId) -> OpenAIMessage "tool" (Just content) Nothing (Just callId)) results

-- Convert internal Message to OpenAI messages
messageToOpenAI :: Message -> [OpenAIMessage]
messageToOpenAI (UserMessage (UserQuery text)) = [OpenAIMessage "user" (Just text) Nothing Nothing]
messageToOpenAI (UserMessage (ToolCallResults results)) = toolResultsToOpenAI results
messageToOpenAI (AssistantMessage (AssistantResponse text toolCalls)) =
    if null toolCalls
        then [OpenAIMessage "assistant" (if T.null text then Nothing else Just text) Nothing Nothing]
        else [OpenAIMessage "assistant" (if T.null text then Nothing else Just text) (Just $ map toolCallToOpenAI toolCalls) Nothing]

-- Convert OpenAI tool call to our internal format
openAIToToolCall :: OpenAIToolCall -> ToolCall
openAIToToolCall (OpenAIToolCall tcId tcType func) =
    let OpenAIFunctionCall funcName funcArgs = func
    in ToolCall
        { toolName = funcName
        , toolId = tcId
        , parameters = case decode (BL.fromStrict $ TE.encodeUtf8 funcArgs) of
            Just params -> params
            Nothing -> object []  -- Fallback to empty object
        }

-- Convert ToolCall to OpenAI format
toolCallToOpenAI :: ToolCall -> OpenAIToolCall
toolCallToOpenAI (ToolCall name id params) =
    OpenAIToolCall id "function" (OpenAIFunctionCall name (toolParametersToText params))

-- Helper to convert JSON parameters to Text (simplified for now)
toolParametersToText :: Value -> Text
toolParametersToText = T.pack . show