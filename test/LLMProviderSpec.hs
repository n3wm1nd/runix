{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Compilation + behaviour test for SomeLLMInterpreter / firstAvailableLLM.
--
-- No real network calls — stub probes return Nothing/Just directly,
-- exercising the selection logic and the existential plumbing.
module LLMProviderSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Fail (Fail, runFail)

import UniversalLLM
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import UniversalLLM.Providers.OpenAI (OpenAI(..))

import Runix.LLM (LLM(..))
import Runix.LLM.Provider (SomeLLMInterpreter(..), candidate, firstAvailableLLM)

-- ============================================================================
-- Minimal test models
-- ============================================================================

data ModelA = ModelA deriving (Show, Eq)
data ModelB = ModelB deriving (Show, Eq)

instance ModelName (Model ModelA Anthropic) where
  modelName _ = "model-a"

instance ModelName (Model ModelB OpenAI) where
  modelName _ = "model-b"

-- ============================================================================
-- Stub probes — polymorphic over r, just like real provider interpreters
-- ============================================================================

unavailableA :: Sem r (Maybe (Sem (LLM (Model ModelA Anthropic) : r) a -> Sem r a))
unavailableA = return Nothing

unavailableB :: Sem r (Maybe (Sem (LLM (Model ModelB OpenAI) : r) a -> Sem r a))
unavailableB = return Nothing

-- | Trivial interpreter: just fails, never actually called in these tests
passthroughA :: Member Fail r => Sem (LLM (Model ModelA Anthropic) : r) a -> Sem r a
passthroughA = interpret $ \case
  QueryLLM _ _ -> fail "stub: passthroughA"

passthroughB :: Member Fail r => Sem (LLM (Model ModelB OpenAI) : r) a -> Sem r a
passthroughB = interpret $ \case
  QueryLLM _ _ -> fail "stub: passthroughB"

availableA :: Member Fail r => Sem r (Maybe (Sem (LLM (Model ModelA Anthropic) : r) a -> Sem r a))
availableA = return (Just passthroughA)

availableB :: Member Fail r => Sem r (Maybe (Sem (LLM (Model ModelB OpenAI) : r) a -> Sem r a))
availableB = return (Just passthroughB)

-- ============================================================================
-- Spec
-- ============================================================================

spec :: Spec
spec = describe "SomeLLMInterpreter / firstAvailableLLM" $ do

  it "returns Nothing when all candidates are unavailable" $ do
    result <- runM . runFail $
      firstAvailableLLM
        [ candidate unavailableA
        , candidate unavailableB
        ]
    case result of
      Right Nothing -> pure ()
      _             -> expectationFailure "expected Right Nothing"

  it "skips unavailable and returns the first available interpreter" $ do
    result <- runM . runFail $
      firstAvailableLLM
        [ candidate unavailableA
        , candidate availableB
        ]
    case result of
      Right (Just _) -> pure ()
      other          -> expectationFailure $ "expected Just interpreter, got: " <> show (fmap (const "...") other)

  it "picks the first available and runs an action through it" $ do
    picked <- runM . runFail $ do
      mInterp <- firstAvailableLLM
        [ candidate availableA
        , candidate availableB
        ]
      case mInterp of
        Nothing                       -> fail "no interpreter"
        Just (SomeLLMInterpreter run) -> run $ return "ok"
    picked `shouldBe` Right "ok"
