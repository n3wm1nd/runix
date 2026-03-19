{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Named model registry and RUNIX_MODEL selection.
--
-- Each provider contributes a 'ModelParser' — a function from the full
-- @RUNIX_MODEL@ string to a probe, plus a list of accepted model strings
-- for help text generation. Parsers do their own prefix matching, so they
-- compose freely and can be distributed across packages.
--
-- Example:
--
-- @
-- -- Select by RUNIX_MODEL env var, fall back to first available:
-- mInterp <- modelFromEnv standardParsers >>= \case
--   Just probe -> probe
--   Nothing    -> firstAvailableLLM (allModels standardParsers)
-- @
module Runix.LLM.Models
  ( -- * Model parsers
    ModelParser(..)
  , runParsers
    -- * Standard parsers
  , standardParsers
    -- * Help text
  , acceptedModelStrings
    -- * Selection
  , selectModel
  , modelFromEnv
    -- * All probes (for firstAvailableLLM)
  , allModels
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Polysemy
import Polysemy.Fail (Fail)

import UniversalLLM (route, via)
import UniversalLLM.Models.Anthropic.Claude (ClaudeSonnet45(..), ClaudeHaiku45(..), ClaudeOpus46(..))
import UniversalLLM.Models.ZhipuAI.GLM (GLM45Air(..), GLM46(..), GLM47(..), GLM5(..), ZAI(..))
import UniversalLLM.Models.Minimax.M (MinimaxM25(..))
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_122B(..), Qwen3CoderNext(..), Qwen35Plus(..))
import UniversalLLM.Models.Moonshot.Kimi (KimiK25(..))
import UniversalLLM.Models.OpenRouter (Universal(..))
import UniversalLLM.Providers.Anthropic (AnthropicOAuth(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..), AlibabaCloud(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI

import Runix.HTTP (HTTP)
import Runix.LLM.Provider
  ( SomeLLMInterpreter(..), candidate
  , zaiLLM, llamaCppLLM, alibabaCloudLLM, openRouterLLM, anthropicOAuthLLM
  )

-- ============================================================================
-- ModelParser
-- ============================================================================

-- | A provider's contribution to the model registry.
--
-- 'parseModel' receives the full @RUNIX_MODEL@ string and returns a probe
-- if it recognises it — providers do their own prefix matching.
-- 'acceptedModels' lists the strings this parser accepts, for help text.
-- Placeholders like @\<model\>@ are conventional for pass-through providers.
data ModelParser r a = ModelParser
  { acceptedModels :: [Text]
  , parseModel     :: Text -> Maybe (Sem r (Maybe (SomeLLMInterpreter r a)))
  }

-- | Run a list of parsers against a model string, return the first match.
runParsers :: Text -> [ModelParser r a] -> Maybe (Sem r (Maybe (SomeLLMInterpreter r a)))
runParsers modelStr parsers = foldr step Nothing parsers
  where
    step parser acc = case parseModel parser modelStr of
      Just x  -> Just x
      Nothing -> acc

-- ============================================================================
-- Standard parsers
-- ============================================================================

-- | All standard provider parsers.
standardParsers :: Members '[HTTP, Fail, Embed IO] r => [ModelParser r a]
standardParsers =
  [ parserAnthropicOAuth
  , parserZAI
  , parserAlibabaCloud
  , parserLlamaCpp
  , parserOpenRouter
  ]

parserAnthropicOAuth :: Members '[HTTP, Fail, Embed IO] r => ModelParser r a
parserAnthropicOAuth = ModelParser
  { acceptedModels =
      [ "anthropic/sonnet"
      , "anthropic/haiku"
      , "anthropic/opus"
      ]
  , parseModel = \case
      "anthropic/sonnet" -> Just . candidate $ anthropicOAuthLLM route (ClaudeSonnet45 `via` AnthropicOAuth) []
      "anthropic/haiku"  -> Just . candidate $ anthropicOAuthLLM route (ClaudeHaiku45  `via` AnthropicOAuth) []
      "anthropic/opus"   -> Just . candidate $ anthropicOAuthLLM route (ClaudeOpus46   `via` AnthropicOAuth) []
      _                  -> Nothing
  }

parserZAI :: Members '[HTTP, Fail, Embed IO] r => ModelParser r a
parserZAI = ModelParser
  { acceptedModels =
      [ "zai/glm-4.5-air"
      , "zai/glm-4.6"
      , "zai/glm-4.7"
      , "zai/glm-5"
      ]
  , parseModel = \case
      "zai/glm-4.5-air" -> Just . candidate $ zaiLLM route (GLM45Air `via` ZAI) []
      "zai/glm-4.6"     -> Just . candidate $ zaiLLM route (GLM46    `via` ZAI) []
      "zai/glm-4.7"     -> Just . candidate $ zaiLLM route (GLM47    `via` ZAI) []
      "zai/glm-5"       -> Just . candidate $ zaiLLM route (GLM5     `via` ZAI) []
      _                 -> Nothing
  }

parserAlibabaCloud :: Members '[HTTP, Fail, Embed IO] r => ModelParser r a
parserAlibabaCloud = ModelParser
  { acceptedModels =
      [ "alibabacloud/minimax"
      , "alibabacloud/kimi"
      , "alibabacloud/qwen-3.5"
      ]
  , parseModel = \case
      "alibabacloud/minimax"  -> Just . candidate $ alibabaCloudLLM route (MinimaxM25  `via` AlibabaCloud) []
      "alibabacloud/kimi"     -> Just . candidate $ alibabaCloudLLM route (KimiK25     `via` AlibabaCloud) []
      "alibabacloud/qwen-3.5" -> Just . candidate $ alibabaCloudLLM route (Qwen35Plus  `via` AlibabaCloud) []
      _                       -> Nothing
  }

parserLlamaCpp :: Members '[HTTP, Fail, Embed IO] r => ModelParser r a
parserLlamaCpp = ModelParser
  { acceptedModels =
      [ "llamacpp/glm-4.5-air"
      , "llamacpp/minimax"
      , "llamacpp/qwen-3.5"
      , "llamacpp/qwen3-coder"
      ]
  , parseModel = \case
      "llamacpp/glm-4.5-air"  -> Just . candidate $ llamaCppLLM route (GLM45Air       `via` LlamaCpp) []
      "llamacpp/minimax"      -> Just . candidate $ llamaCppLLM route (MinimaxM25     `via` LlamaCpp) []
      "llamacpp/qwen-3.5"     -> Just . candidate $ llamaCppLLM route (Qwen35_122B    `via` LlamaCpp) []
      "llamacpp/qwen3-coder"  -> Just . candidate $ llamaCppLLM route (Qwen3CoderNext `via` LlamaCpp) []
      _                       -> Nothing
  }

-- | OpenRouter passes the model string (after "openrouter/") through directly.
parserOpenRouter :: Members '[HTTP, Fail, Embed IO] r => ModelParser r a
parserOpenRouter = ModelParser
  { acceptedModels = ["openrouter/<model>"]
  , parseModel = \str ->
      case T.stripPrefix "openrouter/" str of
        Nothing    -> Nothing
        Just model -> Just . candidate $ openRouterLLM OpenAI.baseComposableProvider (Universal model `via` OpenRouter) []
  }

-- ============================================================================
-- Help text
-- ============================================================================

-- | Collect all accepted model strings from a list of parsers.
-- Useful for generating help text or error messages.
acceptedModelStrings :: [ModelParser r a] -> [Text]
acceptedModelStrings = concatMap acceptedModels

-- ============================================================================
-- Selection
-- ============================================================================

-- | Select a model by exact string, trying each parser in order.
selectModel :: Text -> [ModelParser r a] -> Maybe (Sem r (Maybe (SomeLLMInterpreter r a)))
selectModel name parsers = runParsers name parsers

-- | Read @RUNIX_MODEL@ and select from the given parsers.
-- Returns @Nothing@ if the variable is unset or unrecognized.
modelFromEnv :: Member (Embed IO) r
             => [ModelParser r a]
             -> Sem r (Maybe (Sem r (Maybe (SomeLLMInterpreter r a))))
modelFromEnv parsers = do
  mName <- embed $ lookupEnv "RUNIX_MODEL"
  return $ case mName of
    Nothing   -> Nothing
    Just name -> selectModel (T.pack name) parsers

-- ============================================================================
-- All probes (for firstAvailableLLM)
-- ============================================================================

-- | Extract all probes from a parser list, for use with 'firstAvailableLLM'.
-- Each accepted model string is turned into a probe via its parser.
allModels :: [ModelParser r a] -> [Sem r (Maybe (SomeLLMInterpreter r a))]
allModels parsers =
  [ probe
  | parser  <- parsers
  , modelStr <- acceptedModels parser
  , not (T.isInfixOf "<" modelStr)  -- skip placeholder entries
  , Just probe <- [parseModel parser modelStr]
  ]
