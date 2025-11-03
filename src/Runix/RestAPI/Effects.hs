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
    Get :: (FromJSON s) => Endpoint -> RestAPI p m s
    Post :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    Put :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    Delete :: (FromJSON s) => Endpoint -> RestAPI p m s
    Patch :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    Rest :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m s
makeSem ''RestAPI
class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]

restapiHTTP :: HasCallStack => (RestEndpoint p, Members [HTTP, Fail] r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    Get e -> request "GET" e Nothing
    Post e d -> request "POST" e (Just $ encode d)
    Put e d -> request "PUT" e (Just $ encode d)
    Delete e -> request "DELETE" e Nothing
    Patch e d -> request "PATCH" e (Just $ encode d)
    Rest method e d -> request method e (fmap encode d)
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
