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
    openaiSetParameters :: model -> OpenAIQuery -> OpenAIQuery


newtype OpenAI = OpenAI
    { apikey :: String
    }

instance RestEndpoint OpenAI where
    apiroot _ = "https://api.openai.com/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenAI :: HasCallStack => OpenAIModel model => Members [Fail, HTTP, RestAPI OpenAI] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenAI model = interpret $ \case
    QueryLLMWithModel modelModifier (LLMInstructions instructions) history inputData -> do
        let currentModel = modelModifier model
        let historyMessages = map messageToOpenAI history
        let currentMessages = historyMessages ++
                             [messageToOpenAI (SystemPrompt instructions),
                              messageToOpenAI (UserQuery inputData)]
        let baseQuery = OpenAIQuery { model=openaiModelId currentModel, messages=currentMessages, stream=False, max_tokens=Nothing, reasoning=Nothing}
        let finalQuery = openaiSetParameters currentModel baseQuery
        resp :: OpenAIResponse <- post (Endpoint "chat/completions") finalQuery
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