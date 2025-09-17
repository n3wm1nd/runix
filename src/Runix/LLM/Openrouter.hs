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


module Runix.LLM.Openrouter (
    Openrouter,
    openrouterAPI,
    llmOpenrouter,
    OpenrouterKey(..),
    OpenrouterModel(..)
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

newtype OpenrouterKey = OpenrouterKey String

-- Provider-specific typeclass for OpenRouter model lookup
class OpenrouterModel model where
    openrouterModelId :: model -> String
    openrouterSetParameters :: model -> OpenAIQuery -> OpenAIQuery



newtype Openrouter = Openrouter {
    apikey :: String
}

instance RestEndpoint Openrouter where
    apiroot _ = "https://openrouter.ai/api/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenrouter :: HasCallStack => OpenrouterModel model => Members [Fail, HTTP, RestAPI Openrouter] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenrouter model = interpret $ \case
    QueryLLMWithModel modelModifier (LLMInstructions instructions) history inputData -> do
        let currentModel = modelModifier model
        let historyMessages = map messageToOpenAI history
        let currentMessages = historyMessages ++
                             [messageToOpenAI (SystemPrompt instructions),
                              messageToOpenAI (UserQuery inputData)]
        let baseQuery = OpenAIQuery { model=openrouterModelId currentModel, messages=currentMessages, stream=False, max_tokens=Nothing, reasoning=Nothing}
        let finalQuery = openrouterSetParameters currentModel baseQuery
        resp :: OpenAIResponse <- post (Endpoint "chat/completions") finalQuery
        case resp.choices of
            c:_ -> do
                let responseMessage = openAIToMessage c.message
                let newHistory = history ++ [responseMessage]
                return (newHistory, responseMessage)
            [] -> fail "openrouter: no choices returned"
    


openrouterAPI :: HasCallStack => Members [Fail, HTTP] r => Sem (RestAPI Openrouter:r) a -> Sem (Secret OpenrouterKey:r) a
openrouterAPI a = do
    OpenrouterKey key <- getSecret
    restapiHTTP (Openrouter { apikey = key }) (raiseUnder a)