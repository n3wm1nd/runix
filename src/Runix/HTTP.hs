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
import Polysemy.Internal.Tactics (runTSimple, pureT, getInspectorT, bindT)
import Polysemy.Fail
import Polysemy.State (State, get, put, modify, evalState)
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
import Runix.Streaming (Streaming(..), StreamResult(..), StreamId(..), fetchNext, startStream, interpretStreamingStateful)
import Conduit
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Data.Map
import Data.Conduit.Combinators (iterM)
import Polysemy.Embed (runEmbedded)

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


-- | HTTP streaming effect
-- Streams ByteString chunks, returns () when closed (TODO: capture status/headers)
type HTTPStreaming = Streaming BS.ByteString () HTTPRequest

-- | Public result type for HTTP streaming
type HTTPStreamedResult = StreamResult BS.ByteString

-- | Convenience accessor for fetchNext
fetchChunk :: Member HTTPStreamedResult r => Sem r (Maybe BS.ByteString)
fetchChunk = fetchNext

-- | Make a streaming HTTP request with bracket-style resource management
-- Returns the action result and () (TODO: return status code and headers)
httpRequestStreaming :: forall r a. (Members '[HTTPStreaming, Fail] r)
                     => HTTPRequest
                     -> Sem (HTTPStreamedResult : r) a
                     -> Sem r (a, ())
httpRequestStreaming = startStream
              



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


-- | HTTP streaming interpreter with connection management via STM
-- Uses interpretStreamingStateful helper to manage stream state
httpIOStreaming :: forall r a. (HasCallStack, Members '[Embed IO, Fail] r) => (Request -> Request) -> Sem (HTTPStreaming : r) a -> Sem r a
httpIOStreaming requestTransform =
    interpretStreamingStateful onStart onFetch onClose
  where
    -- Initialize: Create TQueue and fork HTTP request thread
    onStart request = do
        chunkQueue <- embed $ newTQueueIO

        -- Fork thread to make HTTP request and fill the queue
        _ <- embed $ forkIO $ do
            result <- CMC.try (parseRequest request.uri)
            case result of
                Left (_ :: CMC.SomeException) -> do
                    -- Signal error and end
                    atomically $ writeTQueue chunkQueue Nothing
                Right req -> do
                    let hdrs = map (\(hn, hv) -> (fromString hn, fromString hv)) request.headers
                    let hr :: Request =
                            setRequestMethod (fromString request.method) .
                            setRequestHeaders hdrs .
                            requestTransform .
                            case request.body of
                                Just b -> setRequestBody (RequestBodyLBS b)
                                Nothing -> id
                            $ req

                    -- Make the request and write chunks to queue as they arrive
                    withResponse hr $ \respFull -> do
                        runConduit $
                            responseBody respFull
                            .| awaitForever (\chunk -> liftIO $ atomically $ writeTQueue chunkQueue (Just chunk))
                        -- Signal end of stream
                        atomically $ writeTQueue chunkQueue Nothing

        return $ Right chunkQueue

    -- Fetch: Read from TQueue
    onFetch queue = do
        mChunk <- embed $ atomically $ readTQueue queue
        return (mChunk, queue)  -- Queue is unchanged

    -- Close: Nothing to do (queue will be garbage collected)
    onClose _queue = return ()  -- TODO: Return status code and headers

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

-- TODO: Fix these interceptors
{-
-- | Reinterpreter for HTTPStreaming with header support
withStreamingHeaders :: Members [Fail, Logging, HTTPStreaming] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withStreamingHeaders modifyRequest action = interceptH (\case
    HttpRequestStreaming request callback -> do
        raise $ info $ fromString "intercepted streaming request"
        runTSimple $ send $ HttpRequestStreaming (modifyRequest request) callback) action

-- | Simple HTTP streaming logging interceptor - logs method and URI only
withSimpleHTTPStreamingLogging :: (Members [Logging, HTTPStreaming] r, Member Fail r) => Sem r a -> Sem r a
withSimpleHTTPStreamingLogging = interceptH $ \case
    HttpRequestStreaming request callback -> do
        raise $ info $ fromString request.method <> fromString " " <> fromString request.uri <> fromString " (streaming)"
        runTSimple $ send $ HttpRequestStreaming request callback

-- | Full HTTP streaming logging interceptor - logs method, URI, headers, and body
withFullHTTPStreamingLogging :: (Members [Logging, HTTPStreaming] r, Member Fail r) => Sem r a -> Sem r a
withFullHTTPStreamingLogging = interceptH $ \case
    HttpRequestStreaming request callback -> do
        raise $ do
            info $ fromString "HTTP Streaming Request: " <> fromString request.method <> fromString " " <> fromString request.uri
            info $ fromString "Headers: " <> fromString (show request.headers)
            case request.body of
                Just b -> info $ fromString "Body: " <> fromString (show b)
                Nothing -> info $ fromString "Body: (none)"
        runTSimple $ send $ HttpRequestStreaming request callback
-}

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

-- Example usage of withHeaders for setting authentication tokens:
--
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
