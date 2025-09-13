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


module Runix.LLM.Openrouter (Openrouter, openrouterapi, llmOpenrouter, OpenrouterKey(OpenrouterKey)) where
import Runix.Secret.Effects
import Runix.LLM.Effects
import Runix.LLM.Protocol.OpenAICompatible
import Runix.RestAPI.Effects
import Polysemy
import Polysemy.Fail
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Runix.HTTP.Effects

newtype OpenrouterKey = OpenrouterKey String


newtype Openrouter = Openrouter {
    apikey :: String
}

instance RestEndpoint Openrouter where
    apiroot _ = "https://openrouter.ai/api/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenrouter :: HasCallStack => Members [Fail, HTTP, RestAPI Openrouter] r => model -> String -> Sem (LLM model: r) a -> Sem r a
llmOpenrouter _model modelidentifier = interpret $ \case
    AskLLM query -> do
        resp :: OpenAIResponse <- post
            (Endpoint "chat/completions")
            OpenAIQuery { model=modelidentifier, messages=[OpenAIMessage "user" query], stream=False}
        case resp.choices of
            c:_ -> return c.message.content
            [] -> fail "openrouter: no choices returned"
    
    QueryLLM (LLMInstructions instructions) history inputData -> do
        let historyMessages = map messageToOpenAI history
        let currentMessages = historyMessages ++ 
                             [messageToOpenAI (SystemPrompt instructions), 
                              messageToOpenAI (UserQuery inputData)]
        resp :: OpenAIResponse <- post
            (Endpoint "chat/completions")
            OpenAIQuery { model=modelidentifier, messages=currentMessages, stream=False}
        case resp.choices of
            c:_ -> do
                let responseMessage = openAIToMessage c.message
                let newHistory = history ++ [responseMessage]
                return (newHistory, responseMessage)
            [] -> fail "openrouter: no choices returned"

openrouterapi :: HasCallStack => Members [Fail, HTTP] r => Sem (RestAPI Openrouter:r) a -> Sem (Secret OpenrouterKey:r) a
openrouterapi a = do
    OpenrouterKey key <- getSecret
    restapiHTTP (Openrouter { apikey = key }) (raiseUnder a)