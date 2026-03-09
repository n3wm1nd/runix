{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}

-- | File-based HTTP request logging for debugging and auditing
--
-- Logs HTTP requests and responses as JSON files to a specified directory.
-- Useful for local development debugging and request inspection.
--
-- Usage:
--   withHTTPFileLogging "/path/to/project" $ do
--     -- Your HTTP code here, requests are logged to .runix/logs
--
module Runix.Tracing.FileLog
  ( -- * File Logging
    LoggedRequest(..)
  , logHTTPRequests
  , logHTTPStreamingRequests
  ) where

import Prelude hiding (writeFile)
import Polysemy
import Polysemy.Fail
import Polysemy.State
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as Map

import Runix.HTTP (HTTP(..), HTTPRequest, HTTPResponse, HTTPStreaming, HTTPStreamResult(..))
import qualified Runix.HTTP as HTTP
import Runix.Time (Time, getCurrentTime)
import Runix.FileSystem (FileSystemWrite, writeFile, fileSystemLocal)
import qualified Runix.FileSystem.System
import Runix.Streaming (Streaming(..), StreamId(..))

-- ============================================================================
-- JSON Encoding for HTTP Types
-- ============================================================================

-- | JSON encoding for HTTPRequest
instance ToJSON HTTPRequest where
  toJSON (HTTP.HTTPRequest method uri headers body) = object
    [ "method" .= method
    , "uri" .= uri
    , "headers" .= headers
    , "body" .= fmap (T.decodeUtf8 . BSL.toStrict) body
    ]

-- | JSON encoding for HTTPResponse
instance ToJSON HTTPResponse where
  toJSON (HTTP.HTTPResponse code headers body) = object
    [ "status_code" .= code
    , "headers" .= headers
    , "body" .= (T.decodeUtf8 . BSL.toStrict) body
    ]

-- | JSON encoding for logged request/response pair
data LoggedRequest = LoggedRequest
  { logTimestamp :: UTCTime
  , logRequest :: HTTPRequest
  , logResponse :: Either String HTTPResponse
  } deriving stock (Show)

instance ToJSON LoggedRequest where
  toJSON logged = object
    [ "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (logTimestamp logged)
    , "request" .= logRequest logged
    , "response" .= case logResponse logged of
        Right resp -> toJSON resp
        Left err -> object ["error" .= err]
    ]

-- ============================================================================
-- HTTP Request Interceptors
-- ============================================================================

-- | Intercept HTTP requests and log them to filesystem
-- The filesystem @fs should be set up to point to the logging directory
logHTTPRequests :: forall fs r a.
                   Members [HTTP, Time, FileSystemWrite fs, Fail] r
                => Sem r a
                -> Sem r a
logHTTPRequests = intercept $ \case
    HttpRequest request -> do
      timestamp <- getCurrentTime
      response <- send (HttpRequest request)

      -- Create log entry
      let logged = LoggedRequest
            { logTimestamp = timestamp
            , logRequest = request
            , logResponse = response
            }

      -- Write to log file (relative to the fs root)
      let filename = formatTime defaultTimeLocale "http-%Y%m%d-%H%M%S%Q.json" timestamp

      -- Write log entry
      writeFile @fs filename (BSL.toStrict $ encode logged)

      -- Return original response
      return response

-- ============================================================================
-- Streaming HTTP Request Logging
-- ============================================================================

-- | State for tracking streaming requests by StreamId
type StreamRequestMap = Map.Map StreamId (UTCTime, HTTPRequest)

-- | Intercept streaming HTTP requests and log them to filesystem
-- Uses State to track request config across StartStream/CloseStream calls
logHTTPStreamingRequests :: forall fs r a.
                            Members '[HTTPStreaming, Time, FileSystemWrite fs, Fail] r
                         => Sem r a
                         -> Sem r a
logHTTPStreamingRequests action =
  evalState (Map.empty :: StreamRequestMap) $ intercept @HTTPStreaming (\case
    StartStream config -> do
      timestamp <- getCurrentTime
      result <- send @HTTPStreaming $ StartStream config

      -- Store request config for this stream
      case result of
        Right sid -> modify @StreamRequestMap $ Map.insert sid (timestamp, config)
        Left _ -> return ()

      return result

    FetchItem sid -> send @HTTPStreaming $ FetchItem sid

    CloseStream sid -> do
      result <- send @HTTPStreaming $ CloseStream sid

      -- Retrieve the request config for this stream
      requestMap <- get @StreamRequestMap
      case Map.lookup sid requestMap of
        Just (timestamp, config) -> do
          -- Write combined request/response log
          let filename = formatTime defaultTimeLocale "http-streaming-%Y%m%d-%H%M%S%Q.json" timestamp
          let logEntry = object
                [ "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" timestamp
                , "request" .= config
                , "response" .= object
                    [ "status_code" .= streamStatusCode result
                    , "headers" .= streamHeaders result
                    , "body" .= (T.decodeUtf8 . BSL.toStrict . streamBody) result
                    ]
                ]
          writeFile @fs filename (BSL.toStrict $ encode logEntry)

          -- Clean up the map
          modify @StreamRequestMap $ Map.delete sid
        Nothing -> return ()

      return result
  ) $ raise action

-- ============================================================================
-- High-Level API
-- ============================================================================

-- Note: The application should define its own filesystem phantom type
-- (e.g., RequestLogFS) and pass it here. This keeps the module reusable.
