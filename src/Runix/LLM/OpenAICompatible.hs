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
    OpenAICompatibleModel(..),
    openaiCompatibleAPI,
    llmOpenAICompatible
) where

import Runix.Secret.Effects
import Runix.LLM.Effects
import Runix.LLM.Protocol.OpenAICompatible
import Runix.RestAPI.Effects
import Runix.Logging.Effects
import Polysemy
import Polysemy.Fail
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Runix.HTTP.Effects

newtype OpenAICompatibleKey = OpenAICompatibleKey String

data OpenAICompatible = OpenAICompatible
    { apikey :: String
    , endpoint :: String
    }

instance RestEndpoint OpenAICompatible where
    apiroot a = a.endpoint
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

-- Provider-specific typeclass for OpenAI compatible model lookup
class OpenAICompatibleModel model where
    openaiCompatibleModelId :: model -> String
    openaiCompatibleSetParameters :: model -> OpenAIQuery -> OpenAIQuery

llmOpenAICompatible :: HasCallStack => OpenAICompatibleModel model => Members [Fail, HTTP, RestAPI OpenAICompatible, Logging] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenAICompatible model = interpret $ \case
    QueryLLMWithModel modelModifier (LLMInstructions instructions) history inputData -> do
        let currentModel = modelModifier model
        info $ "LLM query: " <> inputData
        let historyMessages = map messageToOpenAI history
        let currentMessages = historyMessages ++
                             [messageToOpenAI (SystemPrompt instructions),
                              messageToOpenAI (UserQuery inputData)]
        let baseQuery = OpenAIQuery { model=openaiCompatibleModelId currentModel, messages=currentMessages, stream=False, max_tokens=Nothing, reasoning=Nothing}
        let finalQuery = openaiCompatibleSetParameters currentModel baseQuery
        resp :: OpenAIResponse <- post (Endpoint "chat/completions") finalQuery
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