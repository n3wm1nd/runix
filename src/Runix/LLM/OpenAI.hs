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
    openAIConfig
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

import UniversalLLM.Core.Types
import UniversalLLM.Providers.OpenAI

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
  withTools = UniversalLLM.Providers.OpenAI.openAIWithTools

instance HasJSON GPT4o OpenAI where
  withJSON = UniversalLLM.Providers.OpenAI.openAIWithJSON

instance HasReasoning GPT4o OpenAI where
  withReasoning = UniversalLLM.Providers.OpenAI.openAIWithReasoning

instance ProviderImplementation OpenAI GPT4o where
  getComposableProvider = withReasoning . withJSON . withTools $ UniversalLLM.Providers.OpenAI.baseComposableProvider

-- GPT-4o Mini - Smaller, faster, cheaper variant
data GPT4oMini = GPT4oMini deriving (Show, Eq)

instance ModelName OpenAI GPT4oMini where
  modelName _ = "gpt-4o-mini"

instance HasTools GPT4oMini OpenAI where
  withTools = UniversalLLM.Providers.OpenAI.openAIWithTools

instance HasJSON GPT4oMini OpenAI where
  withJSON = UniversalLLM.Providers.OpenAI.openAIWithJSON

instance ProviderImplementation OpenAI GPT4oMini where
  getComposableProvider = withJSON . withTools $ UniversalLLM.Providers.OpenAI.baseComposableProvider

-- O1 Preview - Reasoning model (no tool support yet)
data O1Preview = O1Preview deriving (Show, Eq)

instance ModelName OpenAI O1Preview where
  modelName _ = "o1-preview"

instance HasReasoning O1Preview OpenAI where
  withReasoning = UniversalLLM.Providers.OpenAI.openAIWithReasoning

instance ProviderImplementation OpenAI O1Preview where
  getComposableProvider = withReasoning $ UniversalLLM.Providers.OpenAI.baseComposableProvider

-- O1 Mini - Smaller reasoning model
data O1Mini = O1Mini deriving (Show, Eq)

instance ModelName OpenAI O1Mini where
  modelName _ = "o1-mini"

instance HasReasoning O1Mini OpenAI where
  withReasoning = UniversalLLM.Providers.OpenAI.openAIWithReasoning

instance ProviderImplementation OpenAI O1Mini where
  getComposableProvider = withReasoning $ UniversalLLM.Providers.OpenAI.baseComposableProvider
