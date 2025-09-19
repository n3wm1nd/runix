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
import Runix.LLM.OpenAIUtils
import Runix.RestAPI.Effects
import Polysemy
import Polysemy.Fail
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Runix.HTTP.Effects

newtype OpenrouterKey = OpenrouterKey String

-- Provider-specific typeclass for OpenRouter model lookup
class OpenrouterModel model where
    openrouterModelId :: model -> Text
    openrouterSetParameters :: model -> OpenAIQuery -> OpenAIQuery



newtype Openrouter = Openrouter {
    apikey :: String
}

instance RestEndpoint Openrouter where
    apiroot _ = "https://openrouter.ai/api/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenrouter :: HasCallStack => (OpenrouterModel model, HasSystemPrompt model) => Members [Fail, HTTP, RestAPI Openrouter] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenrouter defaultModel = interpret $ \case
    GetModel -> return defaultModel
    QueryLLM model history userInput -> do
        -- Normalize history and convert to OpenAI format
        let normalized = normalizeOpenAIMessages history
        let historyMessages = concatMap messageToOpenAI normalized

        -- Add system prompt if available
        let systemMessage = case getSystemPrompt model of
                Just prompt -> [OpenAIMessage "system" (Just prompt) Nothing Nothing]
                Nothing -> []

        -- Add current user input
        let userMessages = case userInput of
                UserQuery text -> [OpenAIMessage "user" (Just text) Nothing Nothing]
                ToolCallResults results -> toolResultsToOpenAI results
        let allMessages = systemMessage ++ historyMessages ++ userMessages

        let baseQuery = OpenAIQuery
                { model = openrouterModelId model
                , messages = allMessages
                , stream = False
                , max_tokens = Nothing
                , reasoning = Nothing
                , tools = Nothing
                }
        let finalQuery = openrouterSetParameters model baseQuery
        resp :: OpenAIResponse <- post (Endpoint "chat/completions") finalQuery

        case resp.choices of
            c:_ -> do
                let response = c.message

                -- Parse tool calls from response
                let toolCalls = maybe [] (map openAIToToolCall) (tool_calls response)

                -- Create response message
                let responseText = fromMaybe "" (response.content)
                let assistantOutput = AssistantResponse responseText toolCalls
                let newHistory = history ++ [UserMessage userInput, AssistantMessage assistantOutput]

                return (newHistory, assistantOutput)
            [] -> fail "openrouter: no choices returned"


openrouterAPI :: HasCallStack => Members [Fail, HTTP] r => Sem (RestAPI Openrouter:r) a -> Sem (Secret OpenrouterKey:r) a
openrouterAPI a = do
    OpenrouterKey key <- getSecret
    restapiHTTP (Openrouter { apikey = key }) (raiseUnder a)