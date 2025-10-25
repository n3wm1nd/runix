{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runix.LLM.OpenAI
  ( -- * Provider
    OpenAI
  , openAIConfig
  , interpretOpenAI
    -- * Models
  , GPT4o(..)
  , GPT4oMini(..)
  , O1Preview(..)
  , O1Mini(..)
    -- * Re-exports from universal-llm
  , module UniversalLLM.Providers.OpenAI
  ) where

import Polysemy
import Polysemy.Fail
import Data.Text (Text)
import qualified Data.Aeson as Aeson

import UniversalLLM.Core.Types
import UniversalLLM.Providers.OpenAI
import UniversalLLM.Protocols.OpenAI (OpenAIRequest, OpenAIResponse)

import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter (LLMConfig(..), interpretLLM)
import Runix.HTTP.Effects (HTTP)

-- Convenience constructor for OpenAI config
openAIConfig :: String -> LLMConfig OpenAI
openAIConfig apiKey = LLMConfig
    { llmProvider = OpenAI
    , llmEndpoint = "https://api.openai.com/v1/chat/completions"
    , llmHeaders = [("Authorization", "Bearer " <> apiKey), ("Content-Type", "application/json")]
    }

-- Convenience interpreter for OpenAI
interpretOpenAI :: forall model r a.
                   ( ProviderImplementation OpenAI model
                   , ModelName OpenAI model
                   , Members '[HTTP, Fail] r
                   )
                => LLMConfig OpenAI
                -> model
                -> Sem (LLM OpenAI model : r) a
                -> Sem r a
interpretOpenAI = interpretLLM

-- ============================================================================
-- Common OpenAI Models
-- ============================================================================

-- GPT-4o - Latest flagship model with vision, tools, and reasoning
data GPT4o = GPT4o deriving (Show, Eq)

instance ModelName OpenAI GPT4o where
  modelName _ = "gpt-4o"

instance HasTools GPT4o OpenAI where
  toolsComposableProvider = UniversalLLM.Providers.OpenAI.toolsComposableProvider

instance HasJSON GPT4o OpenAI where
  jsonComposableProvider = UniversalLLM.Providers.OpenAI.jsonComposableProvider

instance HasReasoning GPT4o OpenAI where
  reasoningComposableProvider = UniversalLLM.Providers.OpenAI.reasoningComposableProvider

instance ProviderImplementation OpenAI GPT4o where
  getComposableProvider =
    UniversalLLM.Providers.OpenAI.baseComposableProvider
    <> UniversalLLM.Providers.OpenAI.toolsComposableProvider
    <> UniversalLLM.Providers.OpenAI.jsonComposableProvider
    <> UniversalLLM.Providers.OpenAI.reasoningComposableProvider

-- GPT-4o Mini - Smaller, faster, cheaper variant
data GPT4oMini = GPT4oMini deriving (Show, Eq)

instance ModelName OpenAI GPT4oMini where
  modelName _ = "gpt-4o-mini"

instance HasTools GPT4oMini OpenAI where
  toolsComposableProvider = UniversalLLM.Providers.OpenAI.toolsComposableProvider

instance HasJSON GPT4oMini OpenAI where
  jsonComposableProvider = UniversalLLM.Providers.OpenAI.jsonComposableProvider

instance ProviderImplementation OpenAI GPT4oMini where
  getComposableProvider =
    UniversalLLM.Providers.OpenAI.baseComposableProvider
    <> UniversalLLM.Providers.OpenAI.toolsComposableProvider
    <> UniversalLLM.Providers.OpenAI.jsonComposableProvider

-- O1 Preview - Reasoning model (no tool support yet)
data O1Preview = O1Preview deriving (Show, Eq)

instance ModelName OpenAI O1Preview where
  modelName _ = "o1-preview"

instance HasReasoning O1Preview OpenAI where
  reasoningComposableProvider = UniversalLLM.Providers.OpenAI.reasoningComposableProvider

instance ProviderImplementation OpenAI O1Preview where
  getComposableProvider =
    UniversalLLM.Providers.OpenAI.baseComposableProvider
    <> UniversalLLM.Providers.OpenAI.reasoningComposableProvider

-- O1 Mini - Smaller reasoning model
data O1Mini = O1Mini deriving (Show, Eq)

instance ModelName OpenAI O1Mini where
  modelName _ = "o1-mini"

instance HasReasoning O1Mini OpenAI where
  reasoningComposableProvider = UniversalLLM.Providers.OpenAI.reasoningComposableProvider

instance ProviderImplementation OpenAI O1Mini where
  getComposableProvider =
    UniversalLLM.Providers.OpenAI.baseComposableProvider
    <> UniversalLLM.Providers.OpenAI.reasoningComposableProvider
