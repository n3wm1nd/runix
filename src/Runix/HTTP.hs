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
import Runix.Cancellation (Cancellation, isCanceled)
import Runix.Streaming (StreamChunk, emitChunk)
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

data StreamUpdate = StreamChunk BS.ByteString | StreamResult Int [(String, String)] [BS.ByteString] | StreamError String

-- | Basic HTTP effect for non-streaming requests
data HTTP (m :: Type -> Type) a where
    HttpRequest :: HTTPRequest -> HTTP m HTTPResponse
makeSem ''HTTP

-- | HTTP streaming effect for streaming requests
data HTTPStreaming (m :: Type -> Type) a where
    HttpRequestStreaming :: HTTPRequest -> HTTPStreaming m HTTPResponse
makeSem ''HTTPStreaming

-- | Basic HTTP interpreter (non-streaming)
httpIO :: HasCallStack => (Request -> Request) -> Members [Fail, Logging, Embed IO, Cancellation] r => Sem (HTTP : r) a -> Sem r a
httpIO requestTransform = interpret $ \case
    HttpRequest request -> do
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

-- | HTTP streaming interpreter - emits StreamChunk effects
httpIOStreaming :: HasCallStack => (Request -> Request) -> Members [Fail, Logging, StreamChunk BS.ByteString, Embed IO, Cancellation] r => Sem (HTTPStreaming : r) a -> Sem r a
httpIOStreaming requestTransform = interpret $ \case
    HttpRequestStreaming request -> do
        -- Streaming request: emit chunks as they arrive
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

        let checkcancel var = do
                mchunk <- await
                case mchunk of
                    Nothing -> return ()
                    Just chunk -> do
                        yield chunk
                        canceled <- lift . atomically $ readTVar var
                        if canceled then return () else checkcancel var

        -- Two communication channels
        canceledVar <- embed $ newTVarIO False
        updateQueue <- embed $ newTQueueIO

        -- Fork background thread - makes ONE request
        _ <- embed $ forkIO $
            CMC.catch
                (withResponse hr $ \respFull -> do
                    let code = getResponseStatusCode respFull
                    let headers = getResponseHeaders respFull
                    chunkList <- runConduit $
                      responseBody respFull
                      .| iterM (\chunk -> atomically $ writeTQueue updateQueue (StreamChunk chunk))
                      .| checkcancel canceledVar
                      .| sinkList
                    -- Send the final result with headers to queue
                    atomically $ writeTQueue updateQueue (StreamResult code (map (\(hn, b) -> (BS8.unpack (original hn), BS8.unpack b)) headers) chunkList))
                (\(e :: CMC.SomeException) -> do
                    atomically $ writeTQueue updateQueue (StreamError (show e)))

        -- Main thread: read updates from queue and emit chunks
        let consumeUpdates = do
                -- Check cancellation and signal the background thread
                canceled <- isCanceled
                embed $ atomically $ writeTVar canceledVar canceled
                -- Wait for the next update from the queue
                update <- embed $ atomically $ readTQueue updateQueue
                case update of
                    StreamChunk chunk -> do
                        emitChunk chunk
                        consumeUpdates
                    StreamError errMsg -> do
                        fail $ "HTTP streaming error: " ++ errMsg
                    StreamResult code headers chunks -> do
                        return (code, headers, chunks)

        (code, headers, finalChunks) <- consumeUpdates
        let completeBody = BSL.fromChunks finalChunks

        return $ HTTPResponse
            { code = code
            , headers = headers
            , body = completeBody
            }

-- | Convenience function - httpIO with no request transformation
httpIO_ :: HasCallStack => Members [Fail, Logging, Embed IO, Cancellation] r => Sem (HTTP : r) a -> Sem r a
httpIO_ = httpIO id

-- | Convenience function - httpIOStreaming with no request transformation
httpIOStreaming_ :: HasCallStack => Members [Fail, Logging, StreamChunk BS.ByteString, Embed IO, Cancellation] r => Sem (HTTPStreaming : r) a -> Sem r a
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
        httpRequest (modifyRequest request)

-- | Reinterpreter for HTTPStreaming with header support
withStreamingHeaders :: Members [Fail, Logging, HTTPStreaming] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withStreamingHeaders modifyRequest = intercept $ \case
    HttpRequestStreaming request -> do
        info $ fromString "intercepted streaming request"
        httpRequestStreaming (modifyRequest request)

-- | Simple HTTP logging interceptor - logs method and URI only
withSimpleHTTPLogging :: Members [Logging, HTTP] r => Sem r a -> Sem r a
withSimpleHTTPLogging = intercept $ \case
    HttpRequest request -> do
        info $ fromString request.method <> fromString " " <> fromString request.uri
        httpRequest request

-- | Full HTTP logging interceptor - logs method, URI, headers, and body
withFullHTTPLogging :: Members [Logging, HTTP] r => Sem r a -> Sem r a
withFullHTTPLogging = intercept $ \case
    HttpRequest request -> do
        info $ fromString "HTTP Request: " <> fromString request.method <> fromString " " <> fromString request.uri
        info $ fromString "Headers: " <> fromString (show request.headers)
        case request.body of
            Just b -> info $ fromString "Body: " <> fromString (show b)
            Nothing -> info $ fromString "Body: (none)"
        httpRequest request

-- | Simple HTTP streaming logging interceptor - logs method and URI only
withSimpleHTTPStreamingLogging :: Members [Logging, HTTPStreaming] r => Sem r a -> Sem r a
withSimpleHTTPStreamingLogging = intercept $ \case
    HttpRequestStreaming request -> do
        info $ fromString request.method <> fromString " " <> fromString request.uri <> fromString " (streaming)"
        httpRequestStreaming request

-- | Full HTTP streaming logging interceptor - logs method, URI, headers, and body
withFullHTTPStreamingLogging :: Members [Logging, HTTPStreaming] r => Sem r a -> Sem r a
withFullHTTPStreamingLogging = intercept $ \case
    HttpRequestStreaming request -> do
        info $ fromString "HTTP Streaming Request: " <> fromString request.method <> fromString " " <> fromString request.uri
        info $ fromString "Headers: " <> fromString (show request.headers)
        case request.body of
            Just b -> info $ fromString "Body: " <> fromString (show b)
            Nothing -> info $ fromString "Body: (none)"
        httpRequestStreaming request

-- Example usage of withHeaders for setting authentication tokens:
--
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
