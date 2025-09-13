{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Runix.LLM.Protocol.OpenAICompatible where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Runix.LLM.Effects (Message(..))

data OpenAIMessage = OpenAIMessage 
  { role :: String
  , content :: Text
  } deriving (Generic, ToJSON, FromJSON)

data OpenAIQuery = OpenAIQuery 
  { model :: String
  , messages :: [OpenAIMessage]
  , stream :: Bool
  } deriving (Generic, ToJSON, FromJSON)

data OpenAIResponse = OpenAIResponse 
  { id :: String
  , model :: String
  , choices :: [OpenAIChoice]
  , usage :: OpenAIUsage
  } deriving (Generic, ToJSON, FromJSON)

data OpenAIUsage = OpenAIUsage 
  { prompt_tokens :: Int
  , completion_tokens :: Int
  , total_tokens :: Int
  } deriving (Generic, ToJSON, FromJSON)

data OpenAIChoice = OpenAIChoice 
  { finish_reason :: String
  , native_finish_reason :: String
  , message :: OpenAIMessage
  } deriving (Generic, ToJSON, FromJSON)

messageToOpenAI :: Message -> OpenAIMessage
messageToOpenAI (SystemPrompt content) = OpenAIMessage "system" content
messageToOpenAI (UserQuery content) = OpenAIMessage "user" content
messageToOpenAI (AssistantResponse (Just content) []) = OpenAIMessage "assistant" content
messageToOpenAI (AssistantResponse Nothing []) = OpenAIMessage "assistant" ""
messageToOpenAI (AssistantResponse _ _) = error "Tool calls not yet supported in OpenAI compatible protocol"
messageToOpenAI (ToolCallResult content) = OpenAIMessage "tool" content

openAIToMessage :: OpenAIMessage -> Message
openAIToMessage (OpenAIMessage "user" content) = UserQuery content
openAIToMessage (OpenAIMessage "assistant" content) = AssistantResponse (Just content) []
openAIToMessage (OpenAIMessage "system" content) = SystemPrompt content
openAIToMessage (OpenAIMessage "tool" content) = ToolCallResult content
openAIToMessage (OpenAIMessage role content) = error $ "Unknown role: " ++ role