{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Runix.LLM.OpenAICompatible (
    OpenAICompatible,
    OpenAICompatibleKey(..),
    openaiCompatibleAPI,
    llmOpenAICompatible,
    ModelName(..)
) where

import Runix.Secret.Effects
import Runix.LLM.Effects
import Runix.LLM.Protocol.OpenAICompatible
import Runix.RestAPI.Effects
import Polysemy
import Polysemy.Fail
import GHC.Generics
import Data.Aeson
import GHC.Stack (HasCallStack)
import Runix.HTTP.Effects

newtype OpenAICompatibleKey = OpenAICompatibleKey String

-- Simple newtype wrapper for model name (optional but cleaner)
newtype ModelName = ModelName String
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OpenAICompatible = OpenAICompatible
    { apikey :: String
    , endpoint :: String
    }

instance RestEndpoint OpenAICompatible where
    apiroot a = a.endpoint
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenAICompatible :: HasCallStack => Members [Fail, HTTP, RestAPI OpenAICompatible] r => String -> Sem (LLM ModelName: r) a -> Sem r a
llmOpenAICompatible modelId = interpret $ \case
    AskLLM query -> do
        resp :: OpenAIResponse <- post
            (Endpoint "chat/completions")
            OpenAIQuery { model=modelId, messages=[OpenAIMessage "user" query], stream=False}
        case resp.choices of
            c:_ -> return c.message.content
            [] -> fail "openai-compatible: no choices returned"

    QueryLLM (LLMInstructions instructions) history inputData -> do
        let historyMessages = map messageToOpenAI history
        let currentMessages = historyMessages ++
                             [messageToOpenAI (SystemPrompt instructions),
                              messageToOpenAI (UserQuery inputData)]
        resp :: OpenAIResponse <- post
            (Endpoint "chat/completions")
            OpenAIQuery { model=modelId, messages=currentMessages, stream=False}
        case resp.choices of
            c:_ -> do
                let responseMessage = openAIToMessage c.message
                let newHistory = history ++ [responseMessage]
                return (newHistory, responseMessage)
            [] -> fail "openai-compatible: no choices returned"

openaiCompatibleAPI :: HasCallStack => Members [Fail, HTTP] r => String -> Sem (RestAPI OpenAICompatible:r) a -> Sem (Secret OpenAICompatibleKey:r) a
openaiCompatibleAPI endpoint a = do
    OpenAICompatibleKey key <- getSecret
    restapiHTTP (OpenAICompatible { apikey = key, endpoint = endpoint }) (raiseUnder a)