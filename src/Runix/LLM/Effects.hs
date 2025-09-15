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
import Data.Kind (Type)
import Data.Text (Text)
import Data.Aeson (Value)

data Message 
  = SystemPrompt Text
  | UserQuery Text  
  | AssistantResponse (Maybe Text) [ToolCall]
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
    AskLLM :: Text -> LLM model m Text
    QueryLLM :: LLMInstructions -> MessageHistory -> Text -> LLM model m (MessageHistory, Message)
makeSem ''LLM

