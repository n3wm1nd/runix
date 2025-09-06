{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.LLM.Effects where
import Polysemy
import Data.Kind (Type)
import Data.Text (Text)

type MessageHistory = [Text]
newtype LLMInstructions = LLMInstructions Text
data LLM model (m :: Type -> Type) a where
    AskLLM :: Text -> LLM model m Text
    QueryLLM :: LLMInstructions -> Text -> LLM model m Text 
    QueryLLMWithHistory :: LLMInstructions -> MessageHistory -> Text -> LLM model m (MessageHistory, Text)
makeSem ''LLM

class LLMModel model where
    modelidentifier :: model -> String