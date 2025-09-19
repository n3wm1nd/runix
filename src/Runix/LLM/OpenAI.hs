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

newtype OpenAIKey = OpenAIKey String

-- Provider-specific typeclass for OpenAI model lookup
class OpenAIModel model where
    openaiModelId :: model -> Text
    openaiSetParameters :: model -> OpenAIQuery -> OpenAIQuery


newtype OpenAI = OpenAI
    { apikey :: String
    }

instance RestEndpoint OpenAI where
    apiroot _ = "https://api.openai.com/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenAI :: HasCallStack => (OpenAIModel model, HasSystemPrompt model) => Members [Fail, HTTP, RestAPI OpenAI] r => model -> Sem (LLM model: r) a -> Sem r a
llmOpenAI defaultModel = interpret $ \case
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
                { model = openaiModelId model
                , messages = allMessages
                , stream = False
                , max_tokens = Nothing
                , reasoning = Nothing
                , tools = Nothing
                }
        let finalQuery = openaiSetParameters model baseQuery
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
            [] -> fail "openai: no choices returned"


openaiAPI :: HasCallStack => Members [Fail, HTTP] r => Sem (RestAPI OpenAI:r) a -> Sem (Secret OpenAIKey:r) a
openaiAPI a = do
    OpenAIKey key <- getSecret
    restapiHTTP (OpenAI { apikey = key }) (raiseUnder a)