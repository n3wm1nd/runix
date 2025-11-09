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
class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]

restapiHTTP :: HasCallStack => (RestEndpoint p, Members [HTTP, Fail] r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    RestRequest method e maybeData -> request method e (fmap encode maybeData)
  where
    request :: (FromJSON a, Members [Fail, HTTP] r) =>
        String -> Endpoint -> Maybe ByteString -> Sem r a
    request method (Endpoint endpoint) body =
        httpRequest (HTTPRequest
            { method = method
            , uri = apiroot api </> endpoint
            , headers = ("Content-Type", "application/json") : authheaders api
            , body = body
            } ) >>= parseResponse
    parseResponse :: (FromJSON a, Member Fail r) =>
        HTTPResponse -> Sem r a
    parseResponse response =
        -- Check HTTP status code first
        if response.code >= 200 && response.code < 300
        then case decode response.body of
            Just result -> return result
            Nothing -> fail $ "Failed to parse JSON response: " <> BSL.unpack response.body <> "\n"
        else fail $ "HTTP error " <> show response.code <> ": " <> BSL.unpack response.body <> "\n"
