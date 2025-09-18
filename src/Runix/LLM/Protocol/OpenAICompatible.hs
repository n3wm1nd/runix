{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Runix.LLM.Protocol.OpenAICompatible (
    OpenAIMessage(..),
    OpenAIQuery(..),
    ReasoningConfig(..),
    OpenAIResponse(..),
    OpenAIUsage(..),
    OpenAIChoice(..),
    messageToOpenAI,
    openAIToMessage,
    extractThinking
) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Runix.LLM.Effects (Message(..))

data OpenAIMessage = OpenAIMessage 
  { role :: String
  , content :: Text
  } deriving (Generic, ToJSON, FromJSON)

data OpenAIQuery = OpenAIQuery
  { model :: String
  , messages :: [OpenAIMessage]
  , stream :: Bool
  , max_tokens :: Maybe Int
  , reasoning :: Maybe ReasoningConfig
  } deriving (Generic, ToJSON, FromJSON)

data ReasoningConfig = ReasoningConfig
  { max_tokens :: Maybe Int
  , effort :: Maybe String  -- "low", "medium", "high"
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
  , index :: Int
  , message :: OpenAIMessage
  } deriving (Generic, ToJSON, FromJSON)

messageToOpenAI :: Message -> OpenAIMessage
messageToOpenAI (SystemPrompt content) = OpenAIMessage "system" content
messageToOpenAI (UserQuery content) = OpenAIMessage "user" content
messageToOpenAI (AssistantResponse (Just content) [] _) = OpenAIMessage "assistant" content
messageToOpenAI (AssistantResponse Nothing [] _) = OpenAIMessage "assistant" ""
messageToOpenAI (AssistantResponse _ _ _) = error "Tool calls not yet supported in OpenAI compatible protocol"
messageToOpenAI (ToolCallResult content) = OpenAIMessage "tool" content

-- Extract thinking content from <think>...</think> tags
extractThinking :: Text -> (Text, Maybe Text)
extractThinking input =
    case T.splitOn "<think>" input of
        [beforeThink] -> (beforeThink, Nothing)  -- No thinking tags
        [beforeThink, rest] ->
            case T.splitOn "</think>" rest of
                [thinking, afterThink] ->
                    let cleanContent = T.strip (beforeThink <> afterThink)
                        cleanThinking = T.strip thinking
                    in (cleanContent, if T.null cleanThinking then Nothing else Just cleanThinking)
                _ -> (input, Nothing)  -- Malformed tags, return original
        _ -> (input, Nothing)  -- Multiple thinking tags, return original

openAIToMessage :: OpenAIMessage -> Message
openAIToMessage (OpenAIMessage "user" content) = UserQuery content
openAIToMessage (OpenAIMessage "assistant" content) =
    let (cleanContent, thinking) = extractThinking content
        finalContent = if T.null cleanContent then Nothing else Just cleanContent
    in AssistantResponse finalContent [] thinking
openAIToMessage (OpenAIMessage "system" content) = SystemPrompt content
openAIToMessage (OpenAIMessage "tool" content) = ToolCallResult content
openAIToMessage (OpenAIMessage role _content) = error $ "Unknown role: " ++ role