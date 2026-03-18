{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Runix.LLM.Provider
  ( -- * Generic helpers
    fromEnv
  , withLLMInterpreter
    -- * Generic RestAPI helper
  , withRestAPI
    -- * Auth marker types
  , ZAIAuth(..)
  , AlibabaCloudAuth(..)
  , OpenRouterAuth(..)
  , AnthropicAPIKeyAuth(..)
  , AnthropicOAuthAuth(..)
    -- * Config marker types
  , LlamaCppEndpoint(..)
    -- * Provider endpoint types
  , ZAI(..)
  , LlamaCpp(..)
    -- * ZAI interpreters
  , zaiRestAPI
  , zaiLLMWith
  , zaiLLMInterpreter
    -- * LlamaCpp interpreters
  , llamaCppRestAPI
  , llamaCppLLMWith
  , llamaCppLLMInterpreter
    -- * AlibabaCloud interpreters
  , AlibabaCloud(..)
  , alibabaCloudRestAPI
  , alibabaCloudLLMWith
  , alibabaCloudLLMInterpreter
    -- * OpenRouter interpreters
  , OpenRouter(..)
  , openRouterRestAPI
  , openRouterLLMWith
  , openRouterLLMInterpreter
    -- * Anthropic interpreters
  , Anthropic(..)
  , anthropicAPIKeyRestAPI
  , anthropicAPIKeyLLMWith
  , anthropicAPIKeyLLMInterpreter
  , anthropicOAuthRestAPI
  , anthropicOAuthLLMWith
  , anthropicOAuthLLMInterpreter
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

-- | Read an optional value from an environment variable.
fromEnv :: Member (Embed IO) r => (Text -> s) -> String -> Sem r (Maybe s)
fromEnv wrap envVar = fmap (wrap . T.pack) <$> embed (lookupEnv envVar)

-- ============================================================================
-- Generic helpers
-- ============================================================================

withLLMInterpreter :: Sem r (Maybe auth)
                   -> (auth -> Sem (LLM model : r) a -> Sem r a)
                   -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
withLLMInterpreter authLoader llmWith = fmap (fmap llmWith) authLoader

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
-- Provider endpoint types (runix-level, implement RestEndpoint)
-- ============================================================================

newtype ZAI = ZAI Text

instance RestEndpoint ZAI where
    apiroot _ = "https://api.z.ai/api/coding/paas/v4"
    authheaders (ZAI key) = [("Authorization", "Bearer " <> T.unpack key)]

newtype LlamaCpp = LlamaCpp Text

instance RestEndpoint LlamaCpp where
    apiroot (LlamaCpp url) = T.unpack url
    authheaders _ = []

-- ============================================================================
-- ZAI
-- ============================================================================

zaiRestAPI :: Members '[HTTP, Fail, Secret ZAIAuth] r => Sem (RestAPI ZAI : r) a -> Sem r a
zaiRestAPI = withRestAPI ((\(ZAIAuth key) -> ZAI key) <$> getSecret)

zaiLLMWith :: forall model s r a.
              ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
              , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
              , Default s, Members '[HTTP, Fail] r
              )
           => ZAIAuth -> ComposableProvider model s -> model -> [ModelConfig model]
           -> Sem (LLM model : r) a -> Sem r a
zaiLLMWith auth provider model configs =
    runSecret (pure auth) . zaiRestAPI . interpretLLM @ZAI provider model configs . raiseUnder . raiseUnder

zaiLLMInterpreter :: forall model s r a.
                     ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                     , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                     , Default s, Members '[HTTP, Fail, Embed IO] r
                     )
                  => ComposableProvider model s -> model -> [ModelConfig model]
                  -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
zaiLLMInterpreter provider model configs =
    withLLMInterpreter (fromEnv ZAIAuth "ZAI_API_KEY") (\auth -> zaiLLMWith auth provider model configs)

-- ============================================================================
-- LlamaCpp
-- ============================================================================

llamaCppRestAPI :: Members '[HTTP, Fail, Config LlamaCppEndpoint] r => Sem (RestAPI LlamaCpp : r) a -> Sem r a
llamaCppRestAPI = withRestAPI ((\(LlamaCppEndpoint url) -> LlamaCpp url) <$> getConfig)

llamaCppLLMWith :: forall model s r a.
                   ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                   , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                   , Default s, Members '[HTTP, Fail] r
                   )
                => LlamaCppEndpoint -> ComposableProvider model s -> model -> [ModelConfig model]
                -> Sem (LLM model : r) a -> Sem r a
llamaCppLLMWith endpoint provider model configs =
    runConfig endpoint . llamaCppRestAPI . interpretLLM @LlamaCpp provider model configs . raiseUnder . raiseUnder

llamaCppLLMInterpreter :: forall model s r a.
                          ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                          , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                          , Default s, Members '[HTTP, Fail, Embed IO] r
                          )
                       => ComposableProvider model s -> model -> [ModelConfig model]
                       -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
llamaCppLLMInterpreter provider model configs =
    withLLMInterpreter (fromEnv LlamaCppEndpoint "LLAMACPP_ENDPOINT") (\ep -> llamaCppLLMWith ep provider model configs)

-- ============================================================================
-- AlibabaCloud
-- ============================================================================

newtype AlibabaCloud = AlibabaCloud Text

instance RestEndpoint AlibabaCloud where
    apiroot _ = "https://coding-intl.dashscope.aliyuncs.com/v1"
    authheaders (AlibabaCloud key) = [("Authorization", "Bearer " <> T.unpack key)]

alibabaCloudRestAPI :: Members '[HTTP, Fail, Secret AlibabaCloudAuth] r => Sem (RestAPI AlibabaCloud : r) a -> Sem r a
alibabaCloudRestAPI = withRestAPI ((\(AlibabaCloudAuth key) -> AlibabaCloud key) <$> getSecret)

alibabaCloudLLMWith :: forall model s r a.
                       ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                       , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                       , Default s, Members '[HTTP, Fail] r
                       )
                    => AlibabaCloudAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                    -> Sem (LLM model : r) a -> Sem r a
alibabaCloudLLMWith auth provider model configs =
    runSecret (pure auth) . alibabaCloudRestAPI . interpretLLM @AlibabaCloud provider model configs . raiseUnder . raiseUnder

alibabaCloudLLMInterpreter :: forall model s r a.
                              ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                              , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                              , Default s, Members '[HTTP, Fail, Embed IO] r
                              )
                           => ComposableProvider model s -> model -> [ModelConfig model]
                           -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
alibabaCloudLLMInterpreter provider model configs =
    withLLMInterpreter (fromEnv AlibabaCloudAuth "ALIBABACLOUD_API_KEY") (\auth -> alibabaCloudLLMWith auth provider model configs)

-- ============================================================================
-- OpenRouter
-- ============================================================================

newtype OpenRouter = OpenRouter Text

instance RestEndpoint OpenRouter where
    apiroot _ = "https://openrouter.ai/api/v1"
    authheaders (OpenRouter key) = [("Authorization", "Bearer " <> T.unpack key)]

openRouterRestAPI :: Members '[HTTP, Fail, Secret OpenRouterAuth] r => Sem (RestAPI OpenRouter : r) a -> Sem r a
openRouterRestAPI = withRestAPI ((\(OpenRouterAuth key) -> OpenRouter key) <$> getSecret)

openRouterLLMWith :: forall model s r a.
                     ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                     , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                     , Default s, Members '[HTTP, Fail] r
                     )
                  => OpenRouterAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                  -> Sem (LLM model : r) a -> Sem r a
openRouterLLMWith auth provider model configs =
    runSecret (pure auth) . openRouterRestAPI . interpretLLM @OpenRouter provider model configs . raiseUnder . raiseUnder

openRouterLLMInterpreter :: forall model s r a.
                            ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                            , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                            , Default s, Members '[HTTP, Fail, Embed IO] r
                            )
                         => ComposableProvider model s -> model -> [ModelConfig model]
                         -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
openRouterLLMInterpreter provider model configs =
    withLLMInterpreter (fromEnv OpenRouterAuth "OPENROUTER_API_KEY") (\auth -> openRouterLLMWith auth provider model configs)

-- ============================================================================
-- Anthropic
-- ============================================================================

newtype Anthropic = Anthropic Text

instance RestEndpoint Anthropic where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders (Anthropic key) = [("x-api-key", T.unpack key), ("anthropic-version", "2023-06-01")]

newtype AnthropicOAuth = AnthropicOAuth Text

instance RestEndpoint AnthropicOAuth where
    apiroot _ = "https://api.anthropic.com/v1"
    authheaders (AnthropicOAuth token) = map (\(a, b) -> (T.unpack a, T.unpack b)) $ oauthHeaders token

anthropicAPIKeyRestAPI :: Members '[HTTP, Fail, Secret AnthropicAPIKeyAuth] r => Sem (RestAPI Anthropic : r) a -> Sem r a
anthropicAPIKeyRestAPI = withRestAPI ((\(AnthropicAPIKeyAuth key) -> Anthropic key) <$> getSecret)

anthropicAPIKeyLLMWith :: forall model s r a.
                          ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                          , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                          , Default s, Members '[HTTP, Fail] r
                          )
                       => AnthropicAPIKeyAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                       -> Sem (LLM model : r) a -> Sem r a
anthropicAPIKeyLLMWith auth provider model configs =
    runSecret (pure auth) . anthropicAPIKeyRestAPI . interpretLLM @Anthropic provider model configs . raiseUnder . raiseUnder

anthropicAPIKeyLLMInterpreter :: forall model s r a.
                                 ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                                 , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                                 , Default s, Members '[HTTP, Fail, Embed IO] r
                                 )
                              => ComposableProvider model s -> model -> [ModelConfig model]
                              -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
anthropicAPIKeyLLMInterpreter provider model configs =
    withLLMInterpreter (fromEnv AnthropicAPIKeyAuth "ANTHROPIC_API_KEY") (\auth -> anthropicAPIKeyLLMWith auth provider model configs)

anthropicOAuthRestAPI :: Members '[HTTP, Fail, Secret AnthropicOAuthAuth] r => Sem (RestAPI AnthropicOAuth : r) a -> Sem r a
anthropicOAuthRestAPI = withRestAPI ((\(AnthropicOAuthAuth token) -> AnthropicOAuth token) <$> getSecret)

anthropicOAuthLLMWith :: forall model s r a.
                         ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                         , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                         , Default s, Members '[HTTP, Fail] r
                         )
                      => AnthropicOAuthAuth -> ComposableProvider model s -> model -> [ModelConfig model]
                      -> Sem (LLM model : r) a -> Sem r a
anthropicOAuthLLMWith auth provider model configs =
    runSecret (pure auth) . anthropicOAuthRestAPI . interpretLLM @AnthropicOAuth provider model configs . raiseUnder . raiseUnder

anthropicOAuthLLMInterpreter :: forall model s r a.
                                ( ModelName model, HasCodec (ProviderRequest model), HasCodec (ProviderResponse model)
                                , Monoid (ProviderRequest model), ProviderProtocol (ProviderResponse model)
                                , Default s, Members '[HTTP, Fail, Embed IO] r
                                )
                             => ComposableProvider model s -> model -> [ModelConfig model]
                             -> Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
anthropicOAuthLLMInterpreter provider model configs =
    withLLMInterpreter (fromEnv AnthropicOAuthAuth "ANTHROPIC_OAUTH_TOKEN") (\auth -> anthropicOAuthLLMWith auth provider model configs)

-- ============================================================================
-- Model Selection
-- ============================================================================

-- | An LLM interpreter for some model we've committed to but no longer name.
-- The model type is erased — the interpreter is the only thing that remains.
data SomeLLMInterpreter r a where
  SomeLLMInterpreter :: (Sem (LLM model : r) a -> Sem r a) -> SomeLLMInterpreter r a

-- | Wrap a provider's interpreter probe into a selection candidate.
candidate :: Sem r (Maybe (Sem (LLM model : r) a -> Sem r a))
          -> Sem r (Maybe (SomeLLMInterpreter r a))
candidate = fmap (fmap SomeLLMInterpreter)

-- | Try each candidate in order, return the first available interpreter.
-- Each candidate is a probe that returns Just an interpreter if auth is available.
firstAvailableLLM :: [Sem r (Maybe (SomeLLMInterpreter r a))] -> Sem r (Maybe (SomeLLMInterpreter r a))
firstAvailableLLM [] = return Nothing
firstAvailableLLM (probe:rest) = probe >>= \case
  Just interp -> return (Just interp)
  Nothing     -> firstAvailableLLM rest

-- $example
--
-- Example usage — pick the first available model from a priority list,
-- then run an action against it:
--
-- @
-- runWithFirstAvailable
--   :: Members '[HTTP, Fail, Embed IO] r
--   => Sem r Text
-- runWithFirstAvailable = do
--   mInterp <- firstAvailableLLM
--     [ candidate $ zaiLLMInterpreter          route glm47   []
--     , candidate $ anthropicOAuthLLMInterpreter route sonnet []
--     , candidate $ llamaCppLLMInterpreter     route glm45air []
--     ]
--   case mInterp of
--     Nothing                       -> fail "no model available"
--     Just (SomeLLMInterpreter run) -> run $ askLLM "hello"
-- @
--
-- 'candidate' wraps a provider probe so all entries share the same list type,
-- erasing the concrete @model@. Pattern-matching on @SomeLLMInterpreter run@
-- gives back the interpreter; the action passed to @run@ must work for any @model@.
