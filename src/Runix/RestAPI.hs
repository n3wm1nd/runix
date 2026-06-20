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

-- | Structured REST error, preserving enough information for retry logic and diagnostics.
data RestError
    = HttpError Int [(String, String)] BSL.ByteString
    -- ^ Non-2xx response: status code, response headers, body
    | ParseError String BSL.ByteString
    -- ^ 2xx response that failed JSON decoding: message, raw body
    | ConnectionError String
    -- ^ No response received: connection failure message
    deriving (Show)

-- | Collapse a structured error to a human-readable string.
-- Use this in contexts that only care about failure, not the cause.
restErrorMessage :: RestError -> String
restErrorMessage (HttpError code _ body) =
    "HTTP error " <> show code <> ": " <> BSL.unpack body
restErrorMessage (ParseError msg _) =
    "Failed to parse response: " <> msg
restErrorMessage (ConnectionError msg) =
    "Connection error: " <> msg

-- | Basic REST API effect for non-streaming requests.
-- The effect returns Either RestError s — errors are results, not failures.
-- Use the convenience functions (post, get, etc.) which call fail on Left,
-- or handle the Either directly when you need structured error information
-- (e.g. for retry logic on HttpError 429).
data RestAPI p (m :: Type -> Type) a where
    RestRequest :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m (Either RestError s)

makeSem ''RestAPI

-- Convenience functions: send the request and fail on error
get :: forall p s r. (FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> Sem r s
get endpoint = restRequest "GET" endpoint (Nothing :: Maybe ()) >>= either (fail . restErrorMessage) return

post :: forall p req s r. (ToJSON req, FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> req -> Sem r s
post endpoint body = restRequest "POST" endpoint (Just body) >>= either (fail . restErrorMessage) return

put :: forall p req s r. (ToJSON req, FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> req -> Sem r s
put endpoint body = restRequest "PUT" endpoint (Just body) >>= either (fail . restErrorMessage) return

delete :: forall p s r. (FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> Sem r s
delete endpoint = restRequest "DELETE" endpoint (Nothing :: Maybe ()) >>= either (fail . restErrorMessage) return

patch :: forall p req s r. (ToJSON req, FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> req -> Sem r s
patch endpoint body = restRequest "PATCH" endpoint (Just body) >>= either (fail . restErrorMessage) return

class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]
    -- | User-Agent header value for this client
    -- Default is "runix/0.1" but applications should override to identify themselves
    useragent :: p -> String
    useragent _ = "runix/0.1"

-- | Build an HTTPRequest from RestEndpoint configuration
makeHTTPRequest :: (RestEndpoint p, ToJSON r) => p -> String -> Endpoint -> Maybe r -> HTTPRequest
makeHTTPRequest api method (Endpoint endpoint) body =
    HTTPRequest
        { method = method
        , uri = apiroot api </> endpoint
        , headers = ("Content-Type", "application/json")
                  : ("User-Agent", useragent api)
                  : authheaders api
        , body = fmap encode body
        }

-- | Basic REST API interpreter (non-streaming)
-- Never fails — HTTP and parse errors are returned as Left RestError.
restapiHTTP :: HasCallStack => (RestEndpoint p, Member HTTP r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    RestRequest method e maybeData -> do
        let httpReq = makeHTTPRequest api method e maybeData
        result <- send (HttpRequest httpReq)
        case result of
            Left err -> return $ Left $ ConnectionError err
            Right response -> return $ parseResponse response
  where
    parseResponse :: FromJSON s => HTTPResponse -> Either RestError s
    parseResponse response =
        if response.code >= 200 && response.code < 300
        then case decode response.body of
            Just result -> Right result
            Nothing -> Left $ ParseError "JSON decode failed" response.body
        else Left $ HttpError response.code response.headers response.body
