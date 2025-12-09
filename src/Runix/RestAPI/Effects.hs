{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Runix.RestAPI.Effects where
import Polysemy
import Data.Aeson
import Data.Kind (Type)
import GHC.Stack
import Runix.HTTP.Effects
import Polysemy.Fail
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath

newtype Endpoint = Endpoint String
newtype RestData a = RestData a
newtype RestResponse a = RestResponse a

data RestAPI p (m :: Type -> Type) a where
    RestRequest :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m s
    RestRequestStreaming :: (ToJSON r) => String -> Endpoint -> Maybe r -> RestAPI p m HTTPResponse

makeSem ''RestAPI

-- Convenience functions for common HTTP methods
get :: (FromJSON s, Member (RestAPI p) r) => Endpoint -> Sem r s
get endpoint = restRequest "GET" endpoint (Nothing :: Maybe ())

post :: (ToJSON req, FromJSON s, Member (RestAPI p) r) => Endpoint -> req -> Sem r s
post endpoint body = restRequest "POST" endpoint (Just body)

put :: (ToJSON req, FromJSON s, Member (RestAPI p) r) => Endpoint -> req -> Sem r s
put endpoint body = restRequest "PUT" endpoint (Just body)

delete :: (FromJSON s, Member (RestAPI p) r) => Endpoint -> Sem r s
delete endpoint = restRequest "DELETE" endpoint (Nothing :: Maybe ())

patch :: (ToJSON req, FromJSON s, Member (RestAPI p) r) => Endpoint -> req -> Sem r s
patch endpoint body = restRequest "PATCH" endpoint (Just body)

-- Streaming variants (explicit opt-in) - return raw HTTPResponse
postStreaming :: (ToJSON req, Member (RestAPI p) r) => Endpoint -> req -> Sem r HTTPResponse
postStreaming endpoint body = restRequestStreaming "POST" endpoint (Just body)

putStreaming :: (ToJSON req, Member (RestAPI p) r) => Endpoint -> req -> Sem r HTTPResponse
putStreaming endpoint body = restRequestStreaming "PUT" endpoint (Just body)

patchStreaming :: (ToJSON req, Member (RestAPI p) r) => Endpoint -> req -> Sem r HTTPResponse
patchStreaming endpoint body = restRequestStreaming "PATCH" endpoint (Just body)
class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]

restapiHTTP :: HasCallStack => (RestEndpoint p, Members [HTTP, HTTPStreaming, Fail] r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    RestRequest method e maybeData -> do
        let httpReq = makeHTTPRequest method e maybeData
        response <- httpRequest httpReq
        parseResponse response

    RestRequestStreaming method e maybeData -> do
        let httpReq = makeHTTPRequest method e maybeData
        -- For streaming, return the raw HTTPResponse without parsing
        httpRequestStreaming httpReq
  where
    makeHTTPRequest :: ToJSON r => String -> Endpoint -> Maybe r -> HTTPRequest
    makeHTTPRequest method (Endpoint endpoint) body =
        HTTPRequest
            { method = method
            , uri = apiroot api </> endpoint
            , headers = ("Content-Type", "application/json") : authheaders api
            , body = fmap encode body
            }

    parseResponse :: (FromJSON a, Member Fail r) =>
        HTTPResponse -> Sem r a
    parseResponse response =
        -- Check HTTP status code first
        if response.code >= 200 && response.code < 300
        then case decode response.body of
            Just result -> return result
            Nothing -> fail $ "Failed to parse JSON response: " <> BSL.unpack response.body <> "\n"
        else fail $ "HTTP error " <> show response.code <> ": " <> BSL.unpack response.body <> "\n"
