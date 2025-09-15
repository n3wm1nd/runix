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

module Runix.LLM.OpenAI (
    OpenAI,
    OpenAIKey(..),
    openaiAPI,
    llmOpenAI,
    OpenAIModel(..)
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

newtype OpenAIKey = OpenAIKey String

-- Provider-specific typeclass for OpenAI model lookup
class OpenAIModel model where
    openaiModelId :: model -> String

-- Simple model type for string identifiers
newtype SimpleModel = SimpleModel String
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance OpenAIModel SimpleModel where
    openaiModelId (SimpleModel name) = name

newtype OpenAI = OpenAI
    { apikey :: String
    }

instance RestEndpoint OpenAI where
    apiroot _ = "https://api.openai.com/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenAI :: HasCallStack => OpenAIModel model => Members [Fail, HTTP, RestAPI OpenAI] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenAI model = interpret $ \case
    AskLLM query -> do
        resp :: OpenAIResponse <- post
            (Endpoint "chat/completions")
            OpenAIQuery { model=openaiModelId model, messages=[OpenAIMessage "user" query], stream=False}
        case resp.choices of
            c:_ -> return c.message.content
            [] -> fail "openai: no choices returned"

    QueryLLM (LLMInstructions instructions) history inputData -> do
        let historyMessages = map messageToOpenAI history
        let currentMessages = historyMessages ++
                             [messageToOpenAI (SystemPrompt instructions),
                              messageToOpenAI (UserQuery inputData)]
        resp :: OpenAIResponse <- post
            (Endpoint "chat/completions")
            OpenAIQuery { model=openaiModelId model, messages=currentMessages, stream=False}
        case resp.choices of
            c:_ -> do
                let responseMessage = openAIToMessage c.message
                let newHistory = history ++ [responseMessage]
                return (newHistory, responseMessage)
            [] -> fail "openai: no choices returned"

openaiAPI :: HasCallStack => Members [Fail, HTTP] r => Sem (RestAPI OpenAI:r) a -> Sem (Secret OpenAIKey:r) a
openaiAPI a = do
    OpenAIKey key <- getSecret
    restapiHTTP (OpenAI { apikey = key }) (raiseUnder a)