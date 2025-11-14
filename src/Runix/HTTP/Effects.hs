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

module Runix.HTTP.Effects where
import Prelude
import Polysemy
import Polysemy.Fail
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.String (fromString)
import GHC.Stack
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS), responseTimeoutMicro, responseBody)
import Runix.Logging.Effects (Logging, info)
import Runix.Cancellation.Effects (Cancellation, isCanceled)
import Runix.Streaming.Effects (StreamChunk, emitChunk, ignoreChunks)
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

data StreamUpdate = StreamChunk BS.ByteString | StreamResult [BS.ByteString]

data HTTP (m :: Type -> Type) a where
    HttpRequest :: HTTPRequest -> HTTP m HTTPResponse
    HttpRequestStreaming :: HTTPRequest -> HTTP m HTTPResponse
makeSem ''HTTP

-- Internal interpreter with streaming support
httpIO' :: HasCallStack => Members [Fail, Logging, StreamChunk BS.ByteString, Embed IO, Cancellation] r => (Request -> Request) -> Sem (HTTP : r) a -> Sem r a
httpIO' requestTransform = interpret $ \case
    HttpRequest request -> do
        -- Non-streaming request: use httpLBS
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
        info $ fromString request.method <> fromString " " <> fromString request.uri
        resp <- httpLBS hr
        return $ HTTPResponse
            { code = getResponseStatusCode resp
            , headers = map (\(hn, b) -> (show hn, show b)) $ getResponseHeaders resp
            , body = getResponseBody resp
            }

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
        info $ fromString request.method <> fromString " " <> fromString request.uri <> fromString " (streaming)"

        -- Stream the response body
        resp <- embed $ CMC.try (httpNoBody hr)
        case resp of
            Left (e :: CMC.SomeException) -> fail $ "HTTP streaming error: " ++ show e
            Right respHead -> do
                let code = getResponseStatusCode respHead
                let headers = getResponseHeaders respHead

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

                -- Fork background thread
                _ <- embed $ forkIO $
                    CMC.catch
                        (withResponse hr $ \respFull -> do
                            chunkList <- runConduit $
                              responseBody respFull
                              .| iterM (\chunk -> atomically $ writeTQueue updateQueue (StreamChunk chunk))
                              .| checkcancel canceledVar
                              .| sinkList
                            -- Send the final result to queue
                            atomically $ writeTQueue updateQueue (StreamResult chunkList))
                        (\(_ :: CMC.SomeException) -> do
                            atomically $ writeTQueue updateQueue (StreamResult []))

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
                            StreamResult chunks -> do
                                return chunks

                finalChunks <- consumeUpdates
                let completeBody = BSL.fromChunks finalChunks

                return $ HTTPResponse
                    { code = code
                    , headers = map (\(hn, b) -> (show hn, show b)) headers
                    , body = completeBody
                    }


-- | Backward-compatible HTTP interpreter (no streaming chunks emitted)
-- This is the original httpIO signature that existing code expects
httpIO :: HasCallStack => (Request -> Request) -> Members [Fail, Logging, Embed IO, Cancellation] r => Sem (HTTP : r) a -> Sem r a
httpIO requestTransform prog = ignoreChunks $ httpIO' requestTransform $ raiseUnder prog

-- | Convenience function - httpIO with no request transformation
httpIO_ :: HasCallStack => Members [Fail, Logging, Embed IO, Cancellation] r => Sem (HTTP : r) a -> Sem r a
httpIO_ = httpIO id

-- | HTTP interpreter with streaming support - emits StreamChunk effects
-- Use this when you want to receive streaming chunks via the StreamChunk effect
httpIOStreaming :: HasCallStack => (Request -> Request) -> Members [Fail, Logging, StreamChunk BS.ByteString, Embed IO, Cancellation] r => Sem (HTTP : r) a -> Sem r a
httpIOStreaming = httpIO'

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
    HttpRequestStreaming request -> do
        info $ fromString "intercepted streaming request"
        httpRequestStreaming (modifyRequest request)

-- Example usage of withHeaders for setting authentication tokens:
--
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
-- authenticatedRequest = withHeaders $ \req -> req { headers = ("Authorization", "Bearer token123") : headers req }