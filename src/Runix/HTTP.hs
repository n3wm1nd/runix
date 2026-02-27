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
{-# LANGUAGE TypeApplications #-}

module Runix.HTTP where
import Prelude
import Polysemy
import Polysemy.Fail
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Kind (Type)
import Data.String (fromString)
import GHC.Stack
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import Data.CaseInsensitive (original)
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS), responseTimeoutMicro, responseBody)
import Runix.Logging (Logging, info)
import Conduit
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Conduit.Combinators (iterM)

data HTTPRequest = HTTPRequest {
    method :: String,
    uri :: String,
    headers :: [(String, String)],
    body :: Maybe ByteString
} deriving (Show)

data HTTPResponse = HTTPResponse {
    code :: Int,
    headers :: [(String, String)],
    body :: ByteString
} deriving (Show)

-- | Basic HTTP effect for non-streaming requests
data HTTP (m :: Type -> Type) a where
    HttpRequest :: HTTPRequest -> HTTP m (Either String HTTPResponse)

httpRequest :: (Member HTTP r, Member Fail r) => HTTPRequest -> Sem r HTTPResponse
httpRequest req = do
    res <- send (HttpRequest req)
    case res of
        Right resp -> return resp
        Left err -> fail err

-- | HTTP streaming effect for streaming requests
-- Returns a conduit source of raw bytes that can be consumed incrementally via IO
-- Stopping consumption can trigger request cancellation via Cancellation effect.
data HTTPStreaming (m :: Type -> Type) a where
    HttpRequestStreaming :: HTTPRequest
                         -> HTTPStreaming m (Either String (ConduitT () BS.ByteString IO (), Int, [(String, String)]))

-- | Make a streaming HTTP request, returning a source of chunks and response info
httpRequestStreaming :: (Member HTTPStreaming r, Member Fail r) => HTTPRequest -> Sem r (ConduitT () BS.ByteString IO (), Int, [(String, String)])
httpRequestStreaming req = do
    res <- send (HttpRequestStreaming req)
    case res of
        Right result -> return result
        Left err -> fail err



-- | Basic HTTP interpreter (non-streaming)
httpIO :: HasCallStack => (Request -> Request) -> Member (Embed IO) r => Sem (HTTP : r) a -> Sem r a
httpIO requestTransform = interpret $ \case
    HttpRequest request -> runFail $ do
        parsed <- embed $ CMC.try (parseRequest request.uri)
        req <- case parsed of
            Right r -> return r
            Left (e :: CMC.SomeException) -> fail $
                "error parsing uri: " <> request.uri <> "\n" <> show e
        let hdrs = map (\(hn, hv) -> (fromString hn, fromString hv)) request.headers
        let hr :: Request =
                setRequestMethod (fromString request.method) .
                setRequestHeaders hdrs .
                requestTransform .
                case request.body of
                    Just b -> setRequestBody (RequestBodyLBS b)
                    Nothing -> id
                $ req
        resp <- httpLBS hr
        return $ HTTPResponse
            { code = getResponseStatusCode resp
            , headers = map (\(hn, b) -> (BS8.unpack (original hn), BS8.unpack b)) $ getResponseHeaders resp
            , body = getResponseBody resp
            }

-- | HTTP streaming interpreter - returns conduit source via TQueue bridge
-- Uses a queue to bridge between http-conduit's withResponse scope and the returned source
httpIOStreaming :: forall r a. (HasCallStack, Member (Embed IO) r) => (Request -> Request) -> Sem (HTTPStreaming : r) a -> Sem r a
httpIOStreaming requestTransform = interpret $ \case
    HttpRequestStreaming request -> runFail $ do
        -- Parse request URI
        parsed <- embed $ CMC.try (parseRequest request.uri)
        req <- case parsed of
            Right r -> return r
            Left (e :: CMC.SomeException) -> fail $
                "error parsing uri: " <> request.uri <> "\n" <> show e

        let hdrs = map (\(hn, hv) -> (fromString hn, fromString hv)) request.headers
        let hr :: Request =
                setRequestMethod (fromString request.method) .
                setRequestHeaders hdrs .
                requestTransform .
                case request.body of
                    Just b -> setRequestBody (RequestBodyLBS b)
                    Nothing -> id
                $ req

        -- Create queue for chunks and response info
        chunkQueue <- embed $ newTQueueIO
        infoVar <- embed $ newEmptyTMVarIO

        -- Fork background thread that makes the request
        _ <- embed $ forkIO $
            CMC.catch
                (withResponse hr $ \respFull -> do
                    let code = getResponseStatusCode respFull
                    let headers = map (\(hn, b) -> (BS8.unpack (original hn), BS8.unpack b)) $ getResponseHeaders respFull
                    -- Store response info
                    atomically $ putTMVar infoVar (code, headers)
                    -- Stream chunks to queue
                    runConduit $
                        responseBody respFull
                        .| iterM (\chunk -> atomically $ writeTQueue chunkQueue (Just chunk))
                        .| sinkNull
                    -- Signal end of stream
                    atomically $ writeTQueue chunkQueue Nothing)
                (\(e :: CMC.SomeException) ->
                    atomically $ putTMVar infoVar (-1, [("error", show e)]))

        -- Wait for response info
        (code, headers) <- embed $ atomically $ readTMVar infoVar

        -- Return source that reads from queue
        let source :: ConduitT () BS.ByteString IO ()
            source = do
                mChunk <- liftIO $ atomically $ readTQueue chunkQueue
                case mChunk of
                    Nothing -> return ()  -- End of stream
                    Just chunk -> do
                        yield chunk
                        source  -- Continue reading

        return (source, code, headers)

-- | Convenience function - httpIO with no request transformation
httpIO_ :: HasCallStack => Members [Fail, Logging, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO_ = httpIO id

-- | Convenience function - httpIOStreaming with no request transformation
httpIOStreaming_ :: HasCallStack => Members [Fail, Logging, Embed IO] r => Sem (HTTPStreaming : r) a -> Sem r a
httpIOStreaming_ = httpIOStreaming id

-- | Helper function to set request timeout in seconds
withRequestTimeout :: Int -> Request -> Request
withRequestTimeout seconds = setRequestResponseTimeout (responseTimeoutMicro (seconds * 1000000))

-- | Reinterpreter for HTTP with header support
-- NOTE: Currently unused, but kept for future use
withHeaders :: Members [Fail, Logging, HTTP] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withHeaders modifyRequest = intercept $ \case
    HttpRequest request -> do
        info $ fromString "intercepted request"
        send $ HttpRequest (modifyRequest request)

-- | Reinterpreter for HTTPStreaming with header support
withStreamingHeaders :: Members [Fail, Logging, HTTPStreaming] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withStreamingHeaders modifyRequest = intercept $ \case
    HttpRequestStreaming request -> do
        info $ fromString "intercepted streaming request"
        send $ HttpRequestStreaming (modifyRequest request)

-- | Simple HTTP logging interceptor - logs method and URI only
withSimpleHTTPLogging :: Members [Logging, HTTP] r => Sem r a -> Sem r a
withSimpleHTTPLogging = intercept $ \case
    HttpRequest request -> do
        info $ fromString request.method <> fromString " " <> fromString request.uri
        send $ HttpRequest request

-- | Full HTTP logging interceptor - logs method, URI, headers, and body
withFullHTTPLogging :: Members [Logging, HTTP] r => Sem r a -> Sem r a
withFullHTTPLogging = intercept $ \case
    HttpRequest request -> do
        info $ fromString "HTTP Request: " <> fromString request.method <> fromString " " <> fromString request.uri
        info $ fromString "Headers: " <> fromString (show request.headers)
        case request.body of
            Just b -> info $ fromString "Body: " <> fromString (show b)
            Nothing -> info $ fromString "Body: (none)"
        send $ HttpRequest request

-- | Simple HTTP streaming logging interceptor - logs method and URI only
withSimpleHTTPStreamingLogging :: (Members [Logging, HTTPStreaming] r, Member Fail r) => Sem r a -> Sem r a
withSimpleHTTPStreamingLogging = intercept $ \case
    HttpRequestStreaming request -> do
        info $ fromString request.method <> fromString " " <> fromString request.uri <> fromString " (streaming)"
        send $ HttpRequestStreaming request

-- | Full HTTP streaming logging interceptor - logs method, URI, headers, and body
withFullHTTPStreamingLogging :: (Members [Logging, HTTPStreaming] r, Member Fail r) => Sem r a -> Sem r a
withFullHTTPStreamingLogging = intercept $ \case
    HttpRequestStreaming request -> do
        info $ fromString "HTTP Streaming Request: " <> fromString request.method <> fromString " " <> fromString request.uri
        info $ fromString "Headers: " <> fromString (show request.headers)
        case request.body of
            Just b -> info $ fromString "Body: " <> fromString (show b)
            Nothing -> info $ fromString "Body: (none)"
        send $ HttpRequestStreaming request

-- Example usage of withHeaders for setting authentication tokens:
--
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
