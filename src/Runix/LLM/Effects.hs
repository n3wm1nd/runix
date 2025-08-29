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
import qualified Data.Text.Lazy as TL
import Data.Kind (Type)
import Data.Text (Text)

type MessageHistory = [TL.Text]
newtype LLMInstructions = LLMInstructions TL.Text
data LLM (m :: Type -> Type) a where
    AskLLM :: TL.Text -> LLM m TL.Text
    QueryLLM :: LLMInstructions -> TL.Text -> LLM m TL.Text 
    QueryLLMWithHistory :: LLMInstructions -> MessageHistory -> Text -> LLM m (MessageHistory, Text)
makeSem ''LLM
