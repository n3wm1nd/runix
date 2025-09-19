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
import Runix.LLM.OpenAIUtils
import Runix.RestAPI.Effects
import Runix.Logging.Effects
import Polysemy
import Polysemy.Fail
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
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
    openaiCompatibleModelId :: model -> Text
    openaiCompatibleSetParameters :: model -> OpenAIQuery -> OpenAIQuery

llmOpenAICompatible :: HasCallStack => (OpenAICompatibleModel model, HasSystemPrompt model) => Members [Fail, HTTP, RestAPI OpenAICompatible, Logging] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenAICompatible defaultModel = interpret $ \case
    GetModel -> return defaultModel
    QueryLLM model history userInput -> do
        info $ "LLM query: " <> case userInput of
            UserQuery q -> q
            ToolCallResults _ -> "Tool call results"

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
                { model = openaiCompatibleModelId model
                , messages = allMessages
                , stream = False
                , max_tokens = Nothing
                , reasoning = Nothing
                , tools = Nothing
                }
        let finalQuery = openaiCompatibleSetParameters model baseQuery
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
            [] -> fail "openai-compatible: no choices returned"


openaiCompatibleAPI :: HasCallStack => Members [Fail, HTTP] r => String -> Sem (RestAPI OpenAICompatible:r) a -> Sem (Secret OpenAICompatibleKey:r) a
openaiCompatibleAPI endpoint a = do
    OpenAICompatibleKey key <- getSecret
    restapiHTTP (OpenAICompatible { apikey = key, endpoint = endpoint }) (raiseUnder a)