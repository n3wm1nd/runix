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
import Runix.Streaming (Streaming(..), StreamResult(..), fetchNext, startStream, interpretStreamingStateful)
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


-- | Messages sent through the HTTP stream queue
data HTTPStreamMessage
    = StreamHeader Int [(String, String)]   -- ^ Status code and headers (sent first)
    | StreamChunk BS.ByteString             -- ^ Body chunk
    | StreamEnd ByteString                  -- ^ End-of-stream with accumulated body (lazy)
    | StreamError String                    -- ^ Connection/parse error
    deriving (Show)

-- | Result returned when an HTTP stream is closed
data HTTPStreamResult = HTTPStreamResult
    { streamStatusCode :: Int
    , streamHeaders :: [(String, String)]
    , streamBody :: ByteString              -- ^ Full concatenated body (lazy)
    } deriving (Show)

-- | HTTP streaming effect
type HTTPStreaming = Streaming BS.ByteString HTTPStreamResult HTTPRequest

-- | Public result type for HTTP streaming
type HTTPStreamedResult = StreamResult BS.ByteString

-- | Convenience accessor for fetchNext
fetchChunk :: Member HTTPStreamedResult r => Sem r (Maybe BS.ByteString)
fetchChunk = fetchNext

-- | Make a streaming HTTP request with bracket-style resource management
-- Returns the action result and the HTTP stream result (status, headers, body)
httpRequestStreaming :: forall r a. (Members '[HTTPStreaming, Fail] r)
                     => HTTPRequest
                     -> Sem (HTTPStreamedResult : r) a
                     -> Sem r (a, HTTPStreamResult)
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


-- | Stream state: the queue plus status/headers captured at stream start
data HTTPStreamState = HTTPStreamState
    { hssQueue   :: TQueue HTTPStreamMessage
    , hssStatus  :: Int
    , hssHeaders :: [(String, String)]
    }

-- | HTTP streaming interpreter with connection management via STM
-- Uses interpretStreamingStateful helper to manage stream state
httpIOStreaming :: forall r a. (HasCallStack, Members '[Embed IO, Fail] r) => (Request -> Request) -> Sem (HTTPStreaming : r) a -> Sem r a
httpIOStreaming requestTransform =
    interpretStreamingStateful onStart onFetch onClose
  where
    -- Initialize: Create TQueue, fork HTTP request thread, wait for header
    onStart request = do
        queue <- embed newTQueueIO

        -- Fork thread to make HTTP request and fill the queue
        -- Wrap entire thread in exception handler to catch ALL exceptions (not just parseRequest)
        _ <- embed $ forkIO $ do
            result <- CMC.try $ do
                req <- parseRequest request.uri
                let hdrs = map (\(hn, hv) -> (fromString hn, fromString hv)) request.headers
                let hr :: Request =
                        setRequestMethod (fromString request.method) .
                        setRequestHeaders hdrs .
                        requestTransform .
                        case request.body of
                            Just b -> setRequestBody (RequestBodyLBS b)
                            Nothing -> id
                        $ req

                withResponse hr $ \respFull -> do
                    let status = getResponseStatusCode respFull
                        hdrs' = map (\(hn, b) -> (BS8.unpack (original hn), BS8.unpack b))
                                    (getResponseHeaders respFull)
                    atomically $ writeTQueue queue (StreamHeader status hdrs')

                    chunks <- runConduit $
                        responseBody respFull
                        .| iterM (\chunk -> atomically $ writeTQueue queue (StreamChunk chunk))
                        .| sinkList

                    atomically $ writeTQueue queue (StreamEnd (BSL.fromChunks chunks))

            case result of
                Left (e :: CMC.SomeException) ->
                    atomically $ writeTQueue queue (StreamError (show e))
                Right () -> return ()

        -- Wait for the first message: either StreamHeader or StreamError
        firstMsg <- embed $ atomically $ readTQueue queue
        case firstMsg of
            StreamHeader status hdrs ->
                return $ Right $ HTTPStreamState queue status hdrs
            StreamError err ->
                return $ Left $ "HTTP connection error: " ++ err
            -- Should not happen, but handle gracefully
            _ -> return $ Left "Unexpected stream message before header"

    -- Fetch: Read from TQueue, yield StreamChunk, end on StreamEnd
    onFetch st = do
        msg <- embed $ atomically $ readTQueue (hssQueue st)
        case msg of
            StreamChunk chunk -> return (Just chunk, st)
            StreamEnd _ -> do
                -- Put it back so onClose can retrieve the accumulated body
                embed $ atomically $ unGetTQueue (hssQueue st) msg
                return (Nothing, st)
            StreamError _ -> do
                embed $ atomically $ unGetTQueue (hssQueue st) msg
                return (Nothing, st)
            StreamHeader _ _ -> onFetch st  -- should not happen after start

    -- Close: Build HTTPStreamResult from state + remaining queue messages
    -- Check for errors in the remaining messages and construct appropriate result
    onClose st = do
        msgs <- embed $ atomically $ flushTQueue (hssQueue st)
        case errorFrom msgs of
            Just err ->
                -- Stream ended with error - return 500 status with error in body
                return $ HTTPStreamResult 500 [("X-Stream-Error", "true")] (BSL.fromStrict (BS8.pack err))
            Nothing ->
                -- Normal completion - return actual status from header
                let body = bodyFrom msgs
                in return $ HTTPStreamResult (hssStatus st) (hssHeaders st) body

    -- Extract error message if stream ended with error
    errorFrom [] = Nothing
    errorFrom (StreamError e : _) = Just $ "HTTP stream error: " ++ e
    errorFrom (_ : rest) = errorFrom rest

    -- Extract successful body
    bodyFrom [] = BSL.empty
    bodyFrom (StreamEnd b : _) = b
    bodyFrom (_ : rest) = bodyFrom rest

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
-- Modifies requests before they are sent to the interpreter
withStreamingHeaders :: Members [Logging, HTTPStreaming] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withStreamingHeaders modifyRequest = intercept $ \case
    StartStream request -> send $ StartStream (modifyRequest request)
    FetchItem streamId -> send $ FetchItem streamId
    CloseStream streamId -> send $ CloseStream streamId

-- | Simple HTTP streaming logging interceptor - logs start and completion of streams
-- Logs method and URI when stream starts, and a completion message when it closes
withSimpleHTTPStreamingLogging :: Members [Logging, HTTPStreaming] r => Sem r a -> Sem r a
withSimpleHTTPStreamingLogging = intercept $ \case
    StartStream request -> do
        info $ fromString request.method <> fromString " " <> fromString request.uri <> fromString " (streaming)"
        send $ StartStream request
    FetchItem streamId -> send $ FetchItem streamId
    CloseStream streamId -> do
        result <- send $ CloseStream streamId
        info $ fromString "Stream completed"
        return result

-- | Full HTTP streaming logging interceptor - logs method, URI, headers, body at start, and completion
withFullHTTPStreamingLogging :: Members [Logging, HTTPStreaming] r => Sem r a -> Sem r a
withFullHTTPStreamingLogging = intercept $ \case
    StartStream request -> do
        info $ fromString "HTTP Streaming Request: " <> fromString request.method <> fromString " " <> fromString request.uri
        info $ fromString "Headers: " <> fromString (show request.headers)
        case request.body of
            Just b -> info $ fromString "Body: " <> fromString (show b)
            Nothing -> info $ fromString "Body: (none)"
        send $ StartStream request
    FetchItem streamId -> send $ FetchItem streamId
    CloseStream streamId -> do
        result <- send $ CloseStream streamId
        info $ fromString "HTTP Stream completed"
        return result

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
