{-# LANGUAGE Trustworthy #-}
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

module Runix.RestAPI where
import Polysemy
import Polysemy.Fail
import Data.Aeson
import Data.Kind (Type)
import GHC.Stack
import Runix.HTTP
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath

newtype Endpoint = Endpoint String
newtype RestData a = RestData a
newtype RestResponse a = RestResponse a

-- | Basic REST API effect for non-streaming requests
data RestAPI p (m :: Type -> Type) a where
    RestRequest :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m s

makeSem ''RestAPI

-- | REST API streaming effect for streaming requests
data RestAPIStreaming p (m :: Type -> Type) a where
    RestRequestStreaming :: (ToJSON r) => String -> Endpoint -> Maybe r -> RestAPIStreaming p m HTTPResponse

makeSem ''RestAPIStreaming

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
postStreaming :: (ToJSON req, Member (RestAPIStreaming p) r) => Endpoint -> req -> Sem r HTTPResponse
postStreaming endpoint body = restRequestStreaming "POST" endpoint (Just body)

putStreaming :: (ToJSON req, Member (RestAPIStreaming p) r) => Endpoint -> req -> Sem r HTTPResponse
putStreaming endpoint body = restRequestStreaming "PUT" endpoint (Just body)

patchStreaming :: (ToJSON req, Member (RestAPIStreaming p) r) => Endpoint -> req -> Sem r HTTPResponse
patchStreaming endpoint body = restRequestStreaming "PATCH" endpoint (Just body)
class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]

-- | Basic REST API interpreter (non-streaming)
restapiHTTP :: HasCallStack => (RestEndpoint p, Members [HTTP, Fail] r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    RestRequest method e maybeData -> do
        let httpReq = makeHTTPRequest method e maybeData
        response <- httpRequest httpReq
        parseResponse response
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

-- | REST API streaming interpreter
restapiHTTPStreaming :: HasCallStack => (RestEndpoint p, Members [HTTPStreaming, Fail] r) => p -> Sem (RestAPIStreaming p : r) a -> Sem r a
restapiHTTPStreaming api = interpret $ \case
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
