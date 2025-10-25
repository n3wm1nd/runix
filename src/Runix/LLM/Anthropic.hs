{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runix.LLM.Anthropic
  ( -- * Provider
    Anthropic
  , anthropicConfig
  , interpretAnthropic
    -- * Models
  , ClaudeSonnet4_5(..)
  , ClaudeOpus(..)
  , ClaudeHaiku(..)
    -- * Re-exports from universal-llm
  , module UniversalLLM.Providers.Anthropic
  ) where

import Polysemy
import Polysemy.Fail
import Data.Text (Text)
import qualified Data.Aeson as Aeson

import UniversalLLM.Core.Types
import UniversalLLM.Providers.Anthropic
import UniversalLLM.Protocols.Anthropic (AnthropicRequest, AnthropicResponse)

import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter (LLMConfig(..), interpretLLM)
import Runix.HTTP.Effects (HTTP)

-- Convenience constructor for Anthropic config
anthropicConfig :: String -> LLMConfig Anthropic
anthropicConfig apiKey = LLMConfig
    { llmProvider = Anthropic
    , llmEndpoint = "https://api.anthropic.com/v1/messages"
    , llmHeaders =
        [ ("x-api-key", apiKey)
        , ("anthropic-version", "2023-06-01")
        , ("Content-Type", "application/json")
        ]
    }

-- Convenience interpreter for Anthropic
interpretAnthropic :: forall model r a.
                      ( ProviderImplementation Anthropic model
                      , ModelName Anthropic model
                      , Members '[HTTP, Fail] r
                      )
                   => LLMConfig Anthropic
                   -> model
                   -> Sem (LLM Anthropic model : r) a
                   -> Sem r a
interpretAnthropic = interpretLLM

-- ============================================================================
-- Common Anthropic Models
-- ============================================================================

-- Claude Sonnet 4.5 - Latest flagship model
data ClaudeSonnet4_5 = ClaudeSonnet4_5 deriving (Show, Eq)

instance ModelName Anthropic ClaudeSonnet4_5 where
  modelName _ = "claude-3-5-sonnet-20241022"

instance HasTools ClaudeSonnet4_5 Anthropic where
  toolsComposableProvider = UniversalLLM.Providers.Anthropic.toolsComposableProvider

instance ProviderImplementation Anthropic ClaudeSonnet4_5 where
  getComposableProvider =
    UniversalLLM.Providers.Anthropic.baseComposableProvider
    <> UniversalLLM.Providers.Anthropic.toolsComposableProvider

-- Claude Opus - Most capable model
data ClaudeOpus = ClaudeOpus deriving (Show, Eq)

instance ModelName Anthropic ClaudeOpus where
  modelName _ = "claude-3-opus-20240229"

instance HasTools ClaudeOpus Anthropic where
  toolsComposableProvider = UniversalLLM.Providers.Anthropic.toolsComposableProvider

instance ProviderImplementation Anthropic ClaudeOpus where
  getComposableProvider =
    UniversalLLM.Providers.Anthropic.baseComposableProvider
    <> UniversalLLM.Providers.Anthropic.toolsComposableProvider

-- Claude Haiku - Fastest, most compact model
data ClaudeHaiku = ClaudeHaiku deriving (Show, Eq)

instance ModelName Anthropic ClaudeHaiku where
  modelName _ = "claude-3-5-haiku-20241022"

instance HasTools ClaudeHaiku Anthropic where
  toolsComposableProvider = UniversalLLM.Providers.Anthropic.toolsComposableProvider

instance ProviderImplementation Anthropic ClaudeHaiku where
  getComposableProvider =
    UniversalLLM.Providers.Anthropic.baseComposableProvider
    <> UniversalLLM.Providers.Anthropic.toolsComposableProvider
