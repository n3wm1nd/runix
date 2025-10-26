{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runix.LLM.OpenRouter
  ( -- * Provider (re-exported from universal-llm)
    OpenRouter
  , openRouterConfig
  , interpretOpenRouter
    -- * Generic model for any OpenRouter model string
  , OpenRouterModel(..)
  ) where

import Polysemy
import Polysemy.Fail
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<|>))

import UniversalLLM.Core.Types
import UniversalLLM.Providers.OpenAI (OpenRouter(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI
import UniversalLLM.Protocols.OpenAI

import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter (LLMConfig(..), interpretLLM)
import Runix.HTTP.Effects (HTTP)

-- Convenience constructor for OpenRouter config
openRouterConfig :: String -> LLMConfig OpenRouter
openRouterConfig apiKey = LLMConfig
    { llmProvider = OpenRouter
    , llmEndpoint = "https://openrouter.ai/api/v1/chat/completions"
    , llmHeaders = [("Authorization", "Bearer " <> apiKey), ("Content-Type", "application/json")]
    }

-- Convenience interpreter for OpenRouter
interpretOpenRouter :: forall model r a.
                       ( ProviderImplementation OpenRouter model
                       , ModelName OpenRouter model
                       , Members '[HTTP, Fail] r
                       )
                    => LLMConfig OpenRouter
                    -> model
                    -> Sem (LLM OpenRouter model : r) a
                    -> Sem r a
interpretOpenRouter = interpretLLM

-- ============================================================================
-- Generic OpenRouter Model (for any model string)
-- ============================================================================

-- Generic model that accepts any model name
-- Useful for free/experimental models on OpenRouter like "deepseek/deepseek-chat-v3-0324:free"
data OpenRouterModel = OpenRouterModel
    { modelId :: Text
    } deriving (Show, Eq)

instance ModelName OpenRouter OpenRouterModel where
  modelName m = modelId m

-- OpenRouter uses the exact same protocol as OpenAI
-- We use the OpenAI base provider since they share the same protocol

-- Basic text-only implementation (uses OpenAI base provider)
instance ProviderImplementation OpenRouter OpenRouterModel where
  getComposableProvider = OpenAI.baseComposableProvider
