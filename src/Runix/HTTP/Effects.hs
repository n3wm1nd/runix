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
import Control.Monad (void, forever)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Kind (Type)
import Data.String (fromString)
import GHC.Stack
import Network.HTTP.Simple
import Network.HTTP.Client (Response(..))
import Network.HTTP.Types.Status (statusCode)
import qualified Control.Monad.Catch as CMC
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS), responseTimeoutMicro)
import Runix.Logging.Effects (Logging, info)
import Runix.Streaming.Effects (StreamChunk, emitChunk, ignoreChunks)
import qualified Data.Aeson as Aeson
import Conduit
import Data.Conduit (runConduit, (.|))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally)

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

data HTTP (m :: Type -> Type) a where
    HttpRequest :: HTTPRequest -> HTTP m HTTPResponse
    HttpRequestStreaming :: HTTPRequest -> HTTP m HTTPResponse
makeSem ''HTTP

-- Internal interpreter with streaming support
httpIO' :: HasCallStack => Members [Fail, Logging, StreamChunk BS.ByteString, Embed IO] r => (Request -> Request) -> Sem (HTTP : r) a -> Sem r a
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

        -- Create STM queue for streaming chunks between threads
        chunkQueue <- embed $ newTQueueIO
        doneVar <- embed $ newEmptyTMVarIO

        -- Fork thread to fetch HTTP response and push chunks to queue
        _ <- embed $ forkIO $
            CMC.catch
                (withResponse hr $ \resp -> do
                    let code = statusCode $ responseStatus resp
                    let headers = responseHeaders resp

                    -- Stream chunks through conduit, pushing to queue
                    -- Each chunk is immediately pushed to the queue as it arrives
                    chunkList <- runConduit $ responseBody resp .| awaitForever (\chunk -> do
                        -- Push chunk to queue immediately (non-blocking)
                        lift $ do
                            atomically $ writeTQueue chunkQueue (Just chunk)
                            -- Force evaluation to ensure chunk is actually sent
                            BS.length chunk `seq` return ()
                        yield chunk
                      ) .| sinkList

                    -- Signal completion and send result
                    atomically $ do
                        writeTQueue chunkQueue Nothing  -- End marker
                        putTMVar doneVar (Right (code, headers, chunkList)))
                (\(e :: CMC.SomeException) -> do
                    -- Signal error to main thread
                    atomically $ do
                        writeTQueue chunkQueue Nothing  -- End marker
                        putTMVar doneVar (Left $ show e))

        -- Main thread: consume queue and emit chunks in real-time
        let consumeChunks = do
                maybeChunk <- embed $ atomically $ readTQueue chunkQueue
                case maybeChunk of
                    Nothing -> return ()  -- Done
                    Just chunk -> do
                        emitChunk chunk
                        consumeChunks

        consumeChunks

        -- Wait for thread to finish and get result
        result <- embed $ atomically $ takeTMVar doneVar

        case result of
            Left err -> fail $ "HTTP streaming error: " ++ err
            Right (respCode, respHeaders, chunks) -> do
                let completeBody = BSL.fromChunks chunks
                return $ HTTPResponse
                    { code = respCode
                    , headers = map (\(hn, b) -> (show hn, show b)) respHeaders
                    , body = completeBody
                    }

-- | Backward-compatible HTTP interpreter (no streaming chunks emitted)
-- This is the original httpIO signature that existing code expects
httpIO :: HasCallStack => (Request -> Request) -> Members [Fail, Logging, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO requestTransform prog = ignoreChunks $ httpIO' requestTransform $ raiseUnder prog

-- | Convenience function - httpIO with no request transformation
httpIO_ :: HasCallStack => Members [Fail, Logging, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO_ = httpIO id

-- | HTTP interpreter with streaming support - emits StreamChunk effects
-- Use this when you want to receive streaming chunks via the StreamChunk effect
httpIOStreaming :: HasCallStack => (Request -> Request) -> Members [Fail, Logging, StreamChunk BS.ByteString, Embed IO] r => Sem (HTTP : r) a -> Sem r a
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

-- Example usage of withHeaders for setting authentication tokens:
--
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
-- authenticatedRequest = withHeaders $ \req -> req { headers = ("Authorization", "Bearer token123") : headers req }