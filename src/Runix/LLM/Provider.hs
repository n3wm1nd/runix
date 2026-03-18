{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module Runix.LLM.Provider
  ( -- * Probe helpers
    fromEnv
  , orElse
  , withLLMInterpreter
    -- * Generic RestAPI helper
  , withRestAPI
    -- * Auth/Config marker types
  , ZAIAuth(..)
  , AlibabaCloudAuth(..)
  , OpenRouterAuth(..)
  , AnthropicAPIKeyAuth(..)
  , AnthropicOAuthAuth(..)
  , LlamaCppEndpoint(..)
    -- * Provider endpoint types
  , ZAI(..)
  , LlamaCpp(..)
  , AlibabaCloud(..)
  , OpenRouter(..)
  , Anthropic(..)
  , AnthropicOAuth(..)
    -- * RestAPI interpreters
  , zaiRestAPI
  , llamaCppRestAPI
  , alibabaCloudRestAPI
  , openRouterRestAPI
  , anthropicAPIKeyRestAPI
  , anthropicOAuthRestAPI
    -- * auth -> interpreter builders
  , zaiLLMWith
  , llamaCppLLMWith
  , alibabaCloudLLMWith
  , openRouterLLMWith
  , anthropicAPIKeyLLMWith
  , anthropicOAuthLLMWith
    -- * Canonical defaults (probe from standard env vars)
  , zaiLLM
  , llamaCppLLM
  , alibabaCloudLLM
  , openRouterLLM
  , anthropicAPIKeyLLM
  , anthropicOAuthLLM
    -- * Model selection
  , SomeLLMInterpreter(..)
  , candidate
  , firstAvailableLLM
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Polysemy
import Polysemy.Fail (Fail)
import Data.Default (Default)
import Autodocodec (HasCodec)
import Runix.Secret (Secret, getSecret, runSecret)
import Runix.Config (Config, getConfig, runConfig)
import Runix.RestAPI (RestEndpoint(..), restapiHTTP, RestAPI)
import Runix.HTTP (HTTP)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLM, ProviderProtocol)
import UniversalLLM (ComposableProvider, ModelConfig, ModelName, ProviderRequest, ProviderResponse)
import UniversalLLM.Providers.Anthropic (oauthHeaders)

-- ============================================================================
-- Probe helpers
-- ============================================================================

-- | Read an optional value from an environment variable.
fromEnv :: Member (Embed IO) r => (Text -> s) -> String -> Sem r (Maybe s)
fromEnv wrap envVar = fmap (wrap . T.pack) <$> embed (lookupEnv envVar)

-- | Try the first probe, fall back to the second if it returns Nothing.
orElse :: Sem r (Maybe a) -> Sem r (Maybe a) -> Sem r (Maybe a)
orElse a b = a >>= maybe b (return . Just)

-- | Combine a probe with an interpreter builder.
-- If the probe succeeds, apply the builder to produce an interpreter.
withLLMInterpreter :: Sem r (Maybe auth)
                   -> (auth -> Sem (LLM model : r) a -> Sem r a)
                   -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
withLLMInterpreter probe llmWith = fmap (fmap llmWith) probe

-- ============================================================================
-- Generic RestAPI helper
-- ============================================================================

withRestAPI :: (RestEndpoint p, Members '[HTTP, Fail] r)
            => Sem r p
            -> Sem (RestAPI p : r) a
            -> Sem r a
withRestAPI getEndpoint action = getEndpoint >>= \ep -> restapiHTTP ep action

-- ============================================================================
-- Auth/Config marker newtypes
-- ============================================================================

newtype ZAIAuth             = ZAIAuth Text            deriving (Show, Eq)
newtype AlibabaCloudAuth    = AlibabaCloudAuth Text    deriving (Show, Eq)
newtype OpenRouterAuth      = OpenRouterAuth Text      deriving (Show, Eq)
newtype AnthropicAPIKeyAuth = AnthropicAPIKeyAuth Text deriving (Show, Eq)
newtype AnthropicOAuthAuth  = AnthropicOAuthAuth Text  deriving (Show, Eq)

newtype LlamaCppEndpoint    = LlamaCppEndpoint Text    deriving (Show, Eq)

-- ============================================================================
-- Provider endpoint types
-- ============================================================================

newtype ZAI = ZAI Text

instance RestEndpoint ZAI where
    apiroot _ = "https://api.z.ai/api/coding/paas/v4"
    authheaders (ZAI key) = [("Authorization", "Bearer " <> T.unpack key)]

newtype LlamaCpp = LlamaCpp Text

instance RestEndpoint LlamaCpp where
    apiroot (LlamaCpp url) = T.unpack url
    authheaders _ = []

newtype AlibabaCloud = AlibabaCloud Text

instance RestEndpoint AlibabaCloud where
    apiroot _ = "https://coding-intl.dashscope.aliyuncs.com/v1"
    authheaders (AlibabaCloud key) = [("Authorization", "Bearer " <> T.unpack key)]

newtype OpenRouter = OpenRouter Text

instance RestEndpoint OpenRouter where
    apiroot _ = "https://openrouter.ai/api/v1"
    authheaders (OpenRouter key) = [("Authorization", "Bearer " <> T.unpack key)]

newtype Anthropic = Anthropic Text

instance RestEndpoint Anthropic where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders (Anthropic key) = [("x-api-key", T.unpack key), ("anthropic-version", "2023-06-01")]

newtype AnthropicOAuth = AnthropicOAuth Text

instance RestEndpoint AnthropicOAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders (AnthropicOAuth token) = map (\(a, b) -> (T.unpack a, T.unpack b)) $ oauthHeaders token

-- ============================================================================
-- RestAPI interpreters (require auth on stack via Secret/Config)
-- ============================================================================

zaiRestAPI :: Members '[HTTP, Fail, Secret ZAIAuth] r => Sem (RestAPI ZAI : r) a -> Sem r a
zaiRestAPI = withRestAPI ((\(ZAIAuth key) -> ZAI key) <$> getSecret)

llamaCppRestAPI :: Members '[HTTP, Fail, Config LlamaCppEndpoint] r => Sem (RestAPI LlamaCpp : r) a -> Sem r a
llamaCppRestAPI = withRestAPI ((\(LlamaCppEndpoint url) -> LlamaCpp url) <$> getConfig)

alibabaCloudRestAPI :: Members '[HTTP, Fail, Secret AlibabaCloudAuth] r => Sem (RestAPI AlibabaCloud : r) a -> Sem r a
alibabaCloudRestAPI = withRestAPI ((\(AlibabaCloudAuth key) -> AlibabaCloud key) <$> getSecret)

openRouterRestAPI :: Members '[HTTP, Fail, Secret OpenRouterAuth] r => Sem (RestAPI OpenRouter : r) a -> Sem r a
openRouterRestAPI = withRestAPI ((\(OpenRouterAuth key) -> OpenRouter key) <$> getSecret)

anthropicAPIKeyRestAPI :: Members '[HTTP, Fail, Secret AnthropicAPIKeyAuth] r => Sem (RestAPI Anthropic : r) a -> Sem r a
anthropicAPIKeyRestAPI = withRestAPI ((\(AnthropicAPIKeyAuth key) -> Anthropic key) <$> getSecret)

anthropicOAuthRestAPI :: Members '[HTTP, Fail, Secret AnthropicOAuthAuth] r => Sem (RestAPI AnthropicOAuth : r) a -> Sem r a
anthropicOAuthRestAPI = withRestAPI ((\(AnthropicOAuthAuth token) -> AnthropicOAuth token) <$> getSecret)

-- ============================================================================
-- auth -> interpreter builders
-- These take a resolved auth value and produce a clean LLM interpreter,
-- introducing and eliminating all intermediate effects internally.
-- ============================================================================

type LLMConstraints model s r =
  ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
  , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
  , Default s, Members '[HTTP, Fail] r
  )

zaiLLMWith :: forall model s r a. LLMConstraints model s r
           => ZAIAuth -> ComposableProvider model s -> model -> [ModelConfig model]
           -> Sem (LLM model : r) a -> Sem r a
zaiLLMWith auth provider model configs =
    runSecret (pure auth) . zaiRestAPI . interpretLLM @ZAI provider model configs . raiseUnder . raiseUnder

llamaCppLLMWith :: forall model s r a. LLMConstraints model s r
                => LlamaCppEndpoint -> ComposableProvider model s -> model -> [ModelConfig model]
                -> Sem (LLM model : r) a -> Sem r a
llamaCppLLMWith endpoint provider model configs =
    runConfig endpoint . llamaCppRestAPI . interpretLLM @LlamaCpp provider model configs . raiseUnder . raiseUnder

alibabaCloudLLMWith :: forall model s r a. LLMConstraints model s r
                    => AlibabaCloudAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                    -> Sem (LLM model : r) a -> Sem r a
alibabaCloudLLMWith auth provider model configs =
    runSecret (pure auth) . alibabaCloudRestAPI . interpretLLM @AlibabaCloud provider model configs . raiseUnder . raiseUnder

openRouterLLMWith :: forall model s r a. LLMConstraints model s r
                  => OpenRouterAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                  -> Sem (LLM model : r) a -> Sem r a
openRouterLLMWith auth provider model configs =
    runSecret (pure auth) . openRouterRestAPI . interpretLLM @OpenRouter provider model configs . raiseUnder . raiseUnder

anthropicAPIKeyLLMWith :: forall model s r a. LLMConstraints model s r
                       => AnthropicAPIKeyAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                       -> Sem (LLM model : r) a -> Sem r a
anthropicAPIKeyLLMWith auth provider model configs =
    runSecret (pure auth) . anthropicAPIKeyRestAPI . interpretLLM @Anthropic provider model configs . raiseUnder . raiseUnder

anthropicOAuthLLMWith :: forall model s r a. LLMConstraints model s r
                      => AnthropicOAuthAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                      -> Sem (LLM model : r) a -> Sem r a
anthropicOAuthLLMWith auth provider model configs =
    runSecret (pure auth) . anthropicOAuthRestAPI . interpretLLM @AnthropicOAuth provider model configs . raiseUnder . raiseUnder

-- ============================================================================
-- Canonical defaults
-- Probe standard env vars, produce a Maybe interpreter.
-- These are the common case; use withLLMInterpreter + orElse for custom probing.
-- ============================================================================

zaiLLM :: forall model s r a. (LLMConstraints model s r, Member (Embed IO) r)
       => ComposableProvider model s -> model -> [ModelConfig model]
       -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
zaiLLM provider model configs =
    fromEnv ZAIAuth "ZAI_API_KEY" `withLLMInterpreter` \auth -> zaiLLMWith auth provider model configs

llamaCppLLM :: forall model s r a. (LLMConstraints model s r, Member (Embed IO) r)
            => ComposableProvider model s -> model -> [ModelConfig model]
            -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
llamaCppLLM provider model configs =
    fromEnv LlamaCppEndpoint "LLAMACPP_ENDPOINT" `withLLMInterpreter` \ep -> llamaCppLLMWith ep provider model configs

alibabaCloudLLM :: forall model s r a. (LLMConstraints model s r, Member (Embed IO) r)
                => ComposableProvider model s -> model -> [ModelConfig model]
                -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
alibabaCloudLLM provider model configs =
    fromEnv AlibabaCloudAuth "ALIBABACLOUD_API_KEY" `withLLMInterpreter` \auth -> alibabaCloudLLMWith auth provider model configs

openRouterLLM :: forall model s r a. (LLMConstraints model s r, Member (Embed IO) r)
              => ComposableProvider model s -> model -> [ModelConfig model]
              -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
openRouterLLM provider model configs =
    fromEnv OpenRouterAuth "OPENROUTER_API_KEY" `withLLMInterpreter` \auth -> openRouterLLMWith auth provider model configs

anthropicAPIKeyLLM :: forall model s r a. (LLMConstraints model s r, Member (Embed IO) r)
                   => ComposableProvider model s -> model -> [ModelConfig model]
                   -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
anthropicAPIKeyLLM provider model configs =
    fromEnv AnthropicAPIKeyAuth "ANTHROPIC_API_KEY" `withLLMInterpreter` \auth -> anthropicAPIKeyLLMWith auth provider model configs

anthropicOAuthLLM :: forall model s r a. (LLMConstraints model s r, Member (Embed IO) r)
                  => ComposableProvider model s -> model -> [ModelConfig model]
                  -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
anthropicOAuthLLM provider model configs =
    fromEnv AnthropicOAuthAuth "ANTHROPIC_OAUTH_TOKEN" `withLLMInterpreter` \auth -> anthropicOAuthLLMWith auth provider model configs

-- ============================================================================
-- Model Selection
-- ============================================================================

-- | An LLM interpreter for some model we've committed to but no longer name.
-- The model type is erased — the interpreter is the only thing that remains.
data SomeLLMInterpreter r a where
  SomeLLMInterpreter :: (Sem (LLM model : r) a -> Sem r a) -> SomeLLMInterpreter r a

-- | Wrap a provider probe into a selection candidate, erasing the model type.
candidate :: Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
          -> Sem r (Maybe (SomeLLMInterpreter r a))
candidate = fmap (fmap SomeLLMInterpreter)

-- | Try each candidate in order, return the first available interpreter.
firstAvailableLLM :: [Sem r (Maybe (SomeLLMInterpreter r a))] -> Sem r (Maybe (SomeLLMInterpreter r a))
firstAvailableLLM [] = return Nothing
firstAvailableLLM (probe:rest) = probe >>= \case
  Just interp -> return (Just interp)
  Nothing     -> firstAvailableLLM rest

-- $example
--
-- Common case — use canonical defaults:
--
-- @
-- firstAvailableLLM
--   [ candidate $ zaiLLM          route glm47   []
--   , candidate $ anthropicOAuthLLM route sonnet []
--   , candidate $ llamaCppLLM     route glm45air []
--   ]
-- @
--
-- Custom probing — combine multiple sources with 'orElse':
--
-- @
-- firstAvailableLLM
--   [ candidate $ fromEnv ZAIAuth "MY_ZAI_KEY" `orElse` fromEnv ZAIAuth "ZAI_API_KEY"
--       `withLLMInterpreter` \auth -> zaiLLMWith auth route glm47 []
--   ]
-- @
