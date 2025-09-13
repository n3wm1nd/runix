{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Runix.LLM.Protocol.Anthropic where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data AnthropicMessage = AnthropicMessage 
  { role :: String
  , content :: Text
  } deriving (Generic, ToJSON, FromJSON)

data AnthropicQuery = AnthropicQuery 
  { model :: String
  , max_tokens :: Int
  , messages :: [AnthropicMessage]
  , system :: Maybe Text
  , stream :: Bool
  } deriving (Generic, ToJSON, FromJSON)

data AnthropicResponse = AnthropicResponse 
  { id :: String
  , anthropic_type :: String
  , role :: String
  , model :: String
  , content :: [AnthropicContent]
  , stop_reason :: Maybe String
  , stop_sequence :: Maybe String
  , usage :: AnthropicUsage
  } deriving (Generic, ToJSON, FromJSON)

data AnthropicContent = AnthropicContent 
  { content_type :: String
  , text :: Text
  } deriving (Generic, ToJSON, FromJSON)

data AnthropicUsage = AnthropicUsage 
  { input_tokens :: Int
  , output_tokens :: Int
  } deriving (Generic, ToJSON, FromJSON)