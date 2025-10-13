{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.LLM.Effects where
import Polysemy
import Polysemy.Fail
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value)

-- What can be sent TO the LLM
data UserInput
  = UserQuery Text
  | ToolCallResults [ToolResult]
  deriving (Show, Eq)

-- What comes back FROM the LLM
data AssistantOutput
  = AssistantResponse Text [ToolCall]  -- Raw text + any tool calls
  deriving (Show, Eq)

-- Internal message representation for history
data Message
  = UserMessage UserInput
  | AssistantMessage AssistantOutput
  deriving (Show, Eq)

data ToolCall = ToolCall
  { toolName :: Text
  , toolId :: Text
  , parameters :: Value
  } deriving (Show, Eq)

data ToolResult = ToolResult
  { toolResultContent :: Text
  , toolCallId :: Text
  } deriving (Show, Eq)

type MessageHistory = [Message]

-- Typeclass for models that can provide system instructions
class HasSystemPrompt model where
  getSystemPrompt :: model -> Maybe Text

data LLM model (m :: Type -> Type) a where
    GetModel :: LLM model m model
    QueryLLM :: model -> MessageHistory -> UserInput -> LLM model m (MessageHistory, AssistantOutput)
makeSem ''LLM

-- Pure function to extract thinking from response text
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

-- Convenience functions for simple queries
askLLM :: Members [LLM model, Fail] r => Text -> Sem r Text
askLLM query = do
    model <- getModel
    (_, AssistantResponse rawText []) <- queryLLM model [] (UserQuery query)
    let (cleanText, _) = extractThinking rawText
    return cleanText

-- Get response with thinking content
askLLMWithThinking :: Members [LLM model, Fail] r => Text -> Sem r (Text, Maybe Text)
askLLMWithThinking query = do
    model <- getModel
    (_, AssistantResponse rawText []) <- queryLLM model [] (UserQuery query)
    return $ extractThinking rawText

-- Query with conversation context
continueConversation :: Member (LLM model) r => MessageHistory -> UserInput -> Sem r (MessageHistory, AssistantOutput)
continueConversation history userInput = do
    model <- getModel
    queryLLM model history userInput


