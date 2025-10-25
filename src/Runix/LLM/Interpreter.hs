{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Runix.LLM.Interpreter where

import Polysemy
import Polysemy.Fail
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as TE
import Control.Monad (when)
import Autodocodec (HasCodec, toJSONViaCodec, parseJSONViaCodec)
import Data.Aeson.Types (parseEither)

import UniversalLLM
  ( Message(..)
  , ModelConfig(..)
  , Provider(..)
  , ProviderImplementation(..)
  , toProviderRequest
  , fromProviderResponse
  , ModelName(..)
  )

import Runix.LLM.Effects (LLM(..))
import Runix.HTTP.Effects (HTTP(..), HTTPRequest(..), HTTPResponse(..), httpRequest)

-- Generic config for any provider
data LLMConfig provider = LLMConfig
    { llmProvider :: provider
    , llmEndpoint :: String
    , llmHeaders :: [(String, String)]  -- Auth headers, etc.
    }

-- Single interpreter for ALL providers
-- Uses universal-llm's ComposableProvider system to handle provider differences
interpretLLM :: forall provider model r a.
                ( ProviderImplementation provider model
                , ModelName provider model
                , Monoid (ProviderRequest provider)
                , HasCodec (ProviderRequest provider)
                , HasCodec (ProviderResponse provider)
                , Members '[HTTP, Fail] r
                )
             => LLMConfig provider
             -> model
             -> Sem (LLM provider model : r) a
             -> Sem r a
interpretLLM config defaultModel = interpret $ \case
    GetModel -> return defaultModel

    QueryLLM configs messages -> do
        let provider = llmProvider config

        -- Use universal-llm to encode (works for ALL providers)
        let request = toProviderRequest provider defaultModel configs messages

        -- Encode to JSON using autodocodec
        let requestBody = Aeson.encode $ toJSONViaCodec request

        -- Make HTTP call with retry logic (transparent)
        HTTPResponse statusCode _headers responseBody <- retryWithBackoff 3 $ do
            httpRequest $ HTTPRequest "POST" (llmEndpoint config) (llmHeaders config) (Just requestBody)

        -- Check HTTP status code
        when (statusCode >= 400) $
            fail $ "HTTP error " ++ show statusCode ++ ": " ++
                   T.unpack (TE.decodeUtf8With (\_ _ -> Just ' ') (BSL.toStrict responseBody))

        -- Decode response using autodocodec
        case Aeson.eitherDecode responseBody of
            Left err -> fail $ "Failed to decode JSON: " ++ err
            Right jsonValue -> case parseEither parseJSONViaCodec jsonValue of
                Left err -> fail $ "Failed to parse response: " ++ err
                Right providerResponse -> do
                    -- Use universal-llm to decode (works for ALL providers)
                    let resultMessages = fromProviderResponse provider defaultModel configs messages providerResponse
                    return resultMessages

-- Retry helper (transparent error handling)
retryWithBackoff :: forall r a. Member Fail r => Int -> Sem r a -> Sem r a
retryWithBackoff maxRetries action = go maxRetries
  where
    go :: Int -> Sem r a
    go 0 = fail "Max retries exceeded"
    go n = action  -- TODO: Add proper retry logic with exception catching
        -- For now, just attempt once. Full retry logic requires exception handling
        -- which we can add when we integrate with the exception effect
