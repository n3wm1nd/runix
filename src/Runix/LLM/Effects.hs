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
import Data.Aeson (Value)

data Message
  = SystemPrompt Text
  | UserQuery Text
  | AssistantResponse (Maybe Text) [ToolCall] (Maybe Text) -- content, tool calls, thinking
  | ToolCallResult Text
  deriving (Show, Eq)

data ToolCall = ToolCall
  { toolName :: Text
  , toolId :: Text
  , parameters :: Value
  } deriving (Show, Eq)

type MessageHistory = [Message]
newtype LLMInstructions = LLMInstructions Text
data LLM model (m :: Type -> Type) a where
    QueryLLMWithModel :: (model -> model) -> LLMInstructions -> MessageHistory -> Text -> LLM model m (MessageHistory, Message)
makeSem ''LLM

-- Convenience functions that use the single effect
askLLM :: Members [LLM model, Fail] r => Text -> Sem r Text
askLLM = askLLMWith id

askLLMWith :: Members [LLM model, Fail] r => (model -> model) -> Text -> Sem r Text
askLLMWith modifier query = do
    (_, msg) <- queryLLMWithModel modifier (LLMInstructions mempty) [] query
    case msg of
        AssistantResponse (Just content) [] _ -> return content
        _ -> fail "Unexpected response format"

queryLLM :: Member (LLM model) r => LLMInstructions -> MessageHistory -> Text -> Sem r (MessageHistory, Message)
queryLLM = queryLLMWithModel id

queryLLMWith :: Member (LLM model) r => (model -> model) -> LLMInstructions -> MessageHistory -> Text -> Sem r (MessageHistory, Message)
queryLLMWith = queryLLMWithModel

