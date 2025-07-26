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


module Runix.Openrouter (Openrouter, openrouterapi, llmOpenrouter, OpenrouterKey(OpenrouterKey)) where
import Runix.Effects
import Polysemy
import Polysemy.Fail
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

data OpenrouterMessage = OpenrouterMessage {role :: String, content :: T.Text}
    deriving (Generic, ToJSON, FromJSON)
data OpenrouterQuery = OpenrouterQuery {model :: String, messages :: [OpenrouterMessage]}
    deriving (Generic, ToJSON, FromJSON)
newtype OpenrouterKey = OpenrouterKey String

data OpenrouterResponse = OpenrouterResponse {
    id :: String,
    model :: String,
    choices :: [OpenrouterChoice],
    usage :: OpenrouterUsage
    }
    deriving (Generic, ToJSON, FromJSON)
data OpenrouterUsage = OpenrouterUsage {
    prompt_tokens :: Int,
    completion_tokens :: Int,
    total_tokens :: Int
}
    deriving (Generic, ToJSON, FromJSON)

data OpenrouterChoice = OpenrouterChoice {
    finish_reason :: String,
    native_finish_reason :: String,
    message :: OpenrouterMessage
}
    deriving (Generic, ToJSON, FromJSON)


newtype Openrouter = Openrouter {
    apikey :: String
}

instance RestEndpoint Openrouter where
    apiroot _ = "https://openrouter.ai/api/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenrouter :: HasCallStack => Members [Fail, HTTP, RestAPI Openrouter] r => String -> Sem (LLM : r) a -> Sem r a
llmOpenrouter model = interpret $ \case
    AskLLM query -> do
        resp :: OpenrouterResponse <- post
            (Endpoint "chat/completions")
            OpenrouterQuery { model=model, messages=[OpenrouterMessage "user" query]}
        case resp.choices of
            c:_ -> return c.message.content
            [] -> fail "openrouter: no choices returned"

openrouterapi :: HasCallStack => Members [Fail, HTTP] r => Sem (RestAPI Openrouter:r) a -> Sem (Secret OpenrouterKey:r) a
openrouterapi a = do
    OpenrouterKey key <- getSecret
    restapiHTTP (Openrouter { apikey = key }) (raiseUnder a)