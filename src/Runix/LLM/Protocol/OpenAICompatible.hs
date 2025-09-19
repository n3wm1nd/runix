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
    ToolDefinition(..),
    FunctionDefinition(..),
    OpenAIToolCall(..),
    OpenAIFunctionCall(..),
    extractThinking
) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy.Char8 as L8
import Runix.LLM.Effects (ToolCall(..))

data OpenAIMessage = OpenAIMessage
  { role :: Text
  , content :: Maybe Text
  , tool_calls :: Maybe [OpenAIToolCall]
  , tool_call_id :: Maybe Text
  } deriving (Generic, FromJSON)

instance ToJSON OpenAIMessage where
  toJSON (OpenAIMessage r c tc tcid) = object $ catMaybes
    [ Just ("role" .= r)
    , ("content" .=) <$> c
    , ("tool_calls" .=) <$> tc
    , ("tool_call_id" .=) <$> tcid
    ]

data OpenAIToolCall = OpenAIToolCall
  { toolCallId :: Text
  , toolCallType :: Text  -- "function"
  , function :: OpenAIFunctionCall
  } deriving (Generic)

instance ToJSON OpenAIToolCall where
  toJSON (OpenAIToolCall tcId tcType f) = object ["id" .= tcId, "type" .= tcType, "function" .= f]

instance FromJSON OpenAIToolCall where
  parseJSON = withObject "OpenAIToolCall" $ \o -> OpenAIToolCall
    <$> o .: "id"
    <*> o .: "type"
    <*> o .: "function"

data OpenAIFunctionCall = OpenAIFunctionCall
  { name :: Text
  , arguments :: Text  -- JSON string
  } deriving (Generic, ToJSON, FromJSON)

data OpenAIQuery = OpenAIQuery
  { model :: Text
  , messages :: [OpenAIMessage]
  , stream :: Bool
  , max_tokens :: Maybe Int
  , reasoning :: Maybe ReasoningConfig
  , tools :: Maybe [ToolDefinition]
  } deriving (Generic, ToJSON, FromJSON)

data ToolDefinition = ToolDefinition
  { toolType :: String  -- "function"
  , function :: FunctionDefinition
  } deriving (Generic)

instance ToJSON ToolDefinition where
  toJSON (ToolDefinition t f) = object ["type" .= t, "function" .= f]

instance FromJSON ToolDefinition where
  parseJSON = withObject "ToolDefinition" $ \o -> ToolDefinition
    <$> o .: "type"
    <*> o .: "function"

data FunctionDefinition = FunctionDefinition
  { name :: String
  , description :: String
  , parameters :: Value
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

