{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | LangFuse tracing support via OpenTelemetry
--
-- Provides HTTP-level interceptors that capture LLM requests/responses
-- and send them to LangFuse's OTLP endpoint as OpenTelemetry traces.
--
-- Usage:
--   langfuse <- langFuseFromEnv
--   whenJust langfuse $ \lf -> withLangFuse lf $ do
--     -- Your LLM code here, traces are automatically captured
--
module Runix.Tracing.LangFuse
  ( -- * LangFuse OTLP Configuration
    LangFuse(..)
  , langFuseFromEnv
    -- * Interceptors
  , withLangFuse
    -- * Types
  , Span(..)
  , SpanKind(..)
  , StatusCode(..)
  , TraceId(..)
  , SpanId(..)
    -- * Utilities (for testing)
  , generateTraceId
  , generateSpanId
  , buildSpan
  , buildSpanWithSession
  , mkExportRequest
  , ExportTraceServiceRequest(..)
  ) where

import Polysemy
import Polysemy.State (State, get, put)
import Data.Aeson (Value, ToJSON(..), object, (.=), eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as B64
import Data.String (IsString, fromString)
import System.Environment (lookupEnv)

import Runix.HTTP (HTTP(..), HTTPRequest(..), HTTPResponse(..))
import Runix.RestAPI (RestAPI, RestEndpoint(..), Endpoint(..))
import qualified Runix.RestAPI as RestAPI
import Runix.Logging (Logging, info)
import Runix.Time (Time, getCurrentTime)

-- ============================================================================
-- LangFuse OTLP Service
-- ============================================================================

-- | LangFuse OTLP endpoint configuration
data LangFuse = LangFuse
  { langfusePublicKey :: String
  , langfuseSecretKey :: String
  , langfuseBaseUrl :: String
  } deriving stock (Show, Eq)

-- | Create LangFuse service from environment variables
langFuseFromEnv :: Member (Embed IO) r => Sem r (Maybe LangFuse)
langFuseFromEnv = embed $ do
  mPublicKey <- lookupEnv "LANGFUSE_PUBLIC_KEY"
  mSecretKey <- lookupEnv "LANGFUSE_SECRET_KEY"
  baseUrl <- maybe "https://cloud.langfuse.com" id <$> lookupEnv "LANGFUSE_BASE_URL"
  pure $ LangFuse <$> mPublicKey <*> mSecretKey <*> pure baseUrl

-- | Encode Basic Auth header
basicAuth :: String -> String -> String
basicAuth user pass =
  let authString = user <> ":" <> pass
      encoded = TE.decodeUtf8 $ B64.encode $ TE.encodeUtf8 $ T.pack authString
  in "Basic " <> T.unpack encoded

instance RestEndpoint LangFuse where
  apiroot lf = langfuseBaseUrl lf <> "/api/public/otel"
  authheaders lf = [("Authorization", basicAuth (langfusePublicKey lf) (langfuseSecretKey lf))]
  useragent _ = "runix-langfuse/0.1"

-- ============================================================================
-- OpenTelemetry Types
-- ============================================================================

newtype TraceId = TraceId { unTraceId :: Text }
  deriving stock (Show, Eq)
  deriving newtype (IsString, ToJSON)

newtype SpanId = SpanId { unSpanId :: Text }
  deriving stock (Show, Eq)
  deriving newtype (IsString, ToJSON)

data SpanKind
  = SpanKindUnspecified
  | SpanKindInternal
  | SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  deriving stock (Show, Eq)

instance ToJSON SpanKind where
  toJSON = Aeson.Number . \case
    SpanKindUnspecified -> 0
    SpanKindInternal -> 1
    SpanKindServer -> 2
    SpanKindClient -> 3
    SpanKindProducer -> 4
    SpanKindConsumer -> 5

data StatusCode
  = StatusCodeUnset
  | StatusCodeOk
  | StatusCodeError
  deriving stock (Show, Eq)

instance ToJSON StatusCode where
  toJSON = Aeson.Number . \case
    StatusCodeUnset -> 0
    StatusCodeOk -> 1
    StatusCodeError -> 2

-- | OpenTelemetry Span
data Span = Span
  { spanTraceId :: TraceId
  , spanSpanId :: SpanId
  , spanParentSpanId :: Maybe SpanId
  , spanName :: Text
  , spanKind :: SpanKind
  , spanStartTimeUnixNano :: Integer
  , spanEndTimeUnixNano :: Integer
  , spanAttributes :: [(Text, Value)]
  , spanStatusCode :: StatusCode
  , spanStatusMessage :: Maybe Text
  } deriving stock (Show, Eq)

instance ToJSON Span where
  toJSON span = object
    [ "traceId" .= spanTraceId span
    , "spanId" .= spanSpanId span
    , "parentSpanId" .= spanParentSpanId span
    , "name" .= spanName span
    , "kind" .= spanKind span
    , "startTimeUnixNano" .= show (spanStartTimeUnixNano span)
    , "endTimeUnixNano" .= show (spanEndTimeUnixNano span)
    , "attributes" .= fmap attributeToJson (spanAttributes span)
    , "status" .= object
        [ "code" .= spanStatusCode span
        , "message" .= spanStatusMessage span
        ]
    ]
    where
      attributeToJson (k, v) = object ["key" .= k, "value" .= valueToOtelValue v]

      valueToOtelValue :: Value -> Value
      valueToOtelValue = \case
        Aeson.String s -> object ["stringValue" .= s]
        Aeson.Number n -> object ["doubleValue" .= n]
        Aeson.Bool b -> object ["boolValue" .= b]
        v -> object ["stringValue" .= encodeToText v]

      encodeToText = TE.decodeUtf8 . BSL.toStrict . Aeson.encode

-- | OpenTelemetry Resource and Scope wrappers
data ResourceSpans = ResourceSpans
  { resource :: Value
  , scopeSpans :: [ScopeSpans]
  } deriving stock (Show, Eq)

instance ToJSON ResourceSpans where
  toJSON rs = object ["resource" .= resource rs, "scopeSpans" .= scopeSpans rs]

data ScopeSpans = ScopeSpans
  { scope :: Value
  , spans :: [Span]
  } deriving stock (Show, Eq)

instance ToJSON ScopeSpans where
  toJSON ss = object ["scope" .= scope ss, "spans" .= spans ss]

newtype ExportTraceServiceRequest = ExportTraceServiceRequest
  { resourceSpans :: [ResourceSpans]
  } deriving stock (Show, Eq)

instance ToJSON ExportTraceServiceRequest where
  toJSON req = object ["resourceSpans" .= resourceSpans req]

-- | Smart constructor for OTLP trace export request
mkExportRequest :: Span -> ExportTraceServiceRequest
mkExportRequest span = ExportTraceServiceRequest
  { resourceSpans =
      [ ResourceSpans
          { resource = object
              [ "attributes" .=
                  [ object
                      [ "key" .= ("service.name" :: Text)
                      , "value" .= object ["stringValue" .= ("runix" :: Text)]
                      ]
                  ]
              ]
          , scopeSpans =
              [ ScopeSpans
                  { scope = object ["name" .= ("runix-langfuse" :: Text), "version" .= ("0.1" :: Text)]
                  , spans = [span]
                  }
              ]
          }
      ]
  }

-- ============================================================================
-- LLM Provider Detection
-- ============================================================================

data LLMProvider = Anthropic | OpenAI | OpenRouter | AlibabaCloud | Unknown
  deriving stock (Show, Eq)

detectProvider :: Text -> LLMProvider
detectProvider url
  | "anthropic.com" `T.isInfixOf` url = Anthropic
  | "openai.com" `T.isInfixOf` url = OpenAI
  | "openrouter.ai" `T.isInfixOf` url = OpenRouter
  | "dashscope.aliyuncs.com" `T.isInfixOf` url = AlibabaCloud
  | otherwise = Unknown

isLLMProvider :: LLMProvider -> Bool
isLLMProvider Unknown = False
isLLMProvider _ = True

-- | Check if this is an LLM endpoint
isLLMRequest :: HTTPRequest -> Bool
isLLMRequest req =
  let url = T.pack $ uri req
      provider = detectProvider url
      hasEndpoint = any (`T.isInfixOf` url) ["/chat/completions", "/messages"]
  in isLLMProvider provider || hasEndpoint

-- | Map provider to gen_ai.system value
providerToSystem :: LLMProvider -> Text
providerToSystem = \case
  Anthropic -> "anthropic"
  OpenAI -> "openai"
  OpenRouter -> "openai"  -- OpenAI-compatible
  AlibabaCloud -> "alibaba"
  Unknown -> "unknown"

-- ============================================================================
-- ID Generation
-- ============================================================================

-- | Generate a TraceId from current time (16 bytes hex = 32 chars)
generateTraceId :: Member Time r => Sem r TraceId
generateTraceId = TraceId . T.pack . take 32 . formatTimeHex <$> getCurrentTime

-- | Generate a SpanId from current time (8 bytes hex = 16 chars)
generateSpanId :: Member Time r => Sem r SpanId
generateSpanId = SpanId . T.pack . take 16 . formatTimeHex <$> getCurrentTime

-- | Format time as hex-like string (not cryptographically secure, just for tracing)
formatTimeHex :: UTCTime -> String
formatTimeHex t = formatTime defaultTimeLocale "%Y%m%d%H%M%S%Q" t <> repeat '0'

-- | Convert UTCTime to Unix nanoseconds
timeToNanos :: UTCTime -> Integer
timeToNanos = round . (* 1000000000) . utcTimeToPOSIXSeconds

-- ============================================================================
-- Span Construction
-- ============================================================================

-- | Build span from HTTP request/response
buildSpan :: TraceId
          -> SpanId
          -> HTTPRequest
          -> UTCTime
          -> UTCTime
          -> Either String HTTPResponse
          -> Span
buildSpan traceId spanId req startTime endTime result =
  buildSpanWithSession traceId traceId spanId req startTime endTime result

-- | Build span from HTTP request/response with explicit session ID
buildSpanWithSession :: TraceId
                     -> TraceId
                     -> SpanId
                     -> HTTPRequest
                     -> UTCTime
                     -> UTCTime
                     -> Either String HTTPResponse
                     -> Span
buildSpanWithSession sessionId traceId spanId req startTime endTime result =
  Span
    { spanTraceId = traceId
    , spanSpanId = spanId
    , spanParentSpanId = Nothing
    , spanName = spanNameFromRequest req
    , spanKind = SpanKindClient
    , spanStartTimeUnixNano = timeToNanos startTime
    , spanEndTimeUnixNano = timeToNanos endTime
    , spanAttributes = buildAttributesWithSession sessionId req result
    , spanStatusCode = statusFromResult result
    , spanStatusMessage = statusMessageFromResult result
    }

-- | Build span name from request
spanNameFromRequest :: HTTPRequest -> Text
spanNameFromRequest req = T.pack $ method req <> " " <> extractPath (uri req)

-- | Extract URL path from full URI
extractPath :: String -> String
extractPath url =
  case dropWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') $ url of
    "" -> "/"
    path -> path

-- | Determine status code from result
statusFromResult :: Either String HTTPResponse -> StatusCode
statusFromResult = \case
  Right resp | resp.code >= 200 && resp.code < 300 -> StatusCodeOk
             | otherwise -> StatusCodeError
  Left _ -> StatusCodeError

-- | Determine status message from result
statusMessageFromResult :: Either String HTTPResponse -> Maybe Text
statusMessageFromResult = \case
  Right resp | resp.code >= 400 -> Just $ T.pack $ "HTTP " <> show resp.code
             | otherwise -> Nothing
  Left err -> Just $ T.pack err

-- | Build all span attributes
buildAttributes :: HTTPRequest -> Either String HTTPResponse -> [(Text, Value)]
buildAttributes req result =
  baseAttributes req <> resultAttributes result <> inputAttributes req <> outputAttributes result

-- | Build all span attributes with session ID
buildAttributesWithSession :: TraceId -> HTTPRequest -> Either String HTTPResponse -> [(Text, Value)]
buildAttributesWithSession sessionId req result =
  baseAttributesWithSession sessionId req <> resultAttributes result <> inputAttributes req <> outputAttributes result

-- | Base attributes for all spans
baseAttributes :: HTTPRequest -> [(Text, Value)]
baseAttributes req =
  [ ("langfuse.observation.type", Aeson.String "generation")
  , ("gen_ai.system", Aeson.String $ providerToSystem $ detectProvider $ T.pack $ uri req)
  , ("http.request.method", Aeson.String $ T.pack $ method req)
  , ("url.full", Aeson.String $ T.pack $ uri req)
  ]

-- | Base attributes with session ID
baseAttributesWithSession :: TraceId -> HTTPRequest -> [(Text, Value)]
baseAttributesWithSession sessionId req =
  baseAttributes req <>
  [ ("langfuse.session.id", Aeson.String $ unTraceId sessionId)
  ]

-- | Attributes from response
resultAttributes :: Either String HTTPResponse -> [(Text, Value)]
resultAttributes = \case
  Right resp -> [("http.response.status_code", Aeson.Number $ fromIntegral resp.code)]
  Left _ -> [("error", Aeson.Bool True)]

-- | Parse request body for GenAI attributes
inputAttributes :: HTTPRequest -> [(Text, Value)]
inputAttributes req = maybe [] parseRequestBody req.body
  where
    parseRequestBody bodyBytes =
      case eitherDecode bodyBytes of
        Right (Aeson.Object obj) ->
          let modelAttr = maybe [] (\m -> [("gen_ai.request.model", m)]) $ KM.lookup "model" obj
              inputAttr = [("langfuse.observation.input", Aeson.String $ bytesToText bodyBytes)]
          in modelAttr <> inputAttr
        _ -> []

    bytesToText = TE.decodeUtf8 . BSL.toStrict

-- | Parse response body for GenAI attributes
outputAttributes :: Either String HTTPResponse -> [(Text, Value)]
outputAttributes = \case
  Right resp -> parseResponseBody resp.body
  Left err -> [("langfuse.observation.status_message", Aeson.String $ T.pack err)]
  where
    parseResponseBody bodyBytes =
      case eitherDecode bodyBytes of
        Right (Aeson.Object obj) ->
          let outputAttr = [("langfuse.observation.output", Aeson.String $ bytesToText bodyBytes)]
              usageAttr = maybe [] (\u -> [("gen_ai.usage", u)]) $ KM.lookup "usage" obj
          in outputAttr <> usageAttr
        _ -> []

    bytesToText = TE.decodeUtf8 . BSL.toStrict

-- ============================================================================
-- Main Interceptor
-- ============================================================================

-- | Session state for LangFuse - tracks the session trace ID
newtype LangFuseSession = LangFuseSession
  { sessionTraceId :: TraceId
  } deriving stock (Show, Eq)

-- | Intercept HTTP requests to trace LLM calls with session tracking
withLangFuse :: forall r a. Members '[HTTP, RestAPI LangFuse, Time, Logging] r
             => LangFuse
             -> Sem r a
             -> Sem r a
withLangFuse _langfuse action = do
  -- Generate a session trace ID for this entire session
  sessionTrace <- generateTraceId
  let session = LangFuseSession sessionTrace

  -- Run the action with session state
  interceptWithSession session action
  where
    interceptWithSession :: LangFuseSession -> Sem r a -> Sem r a
    interceptWithSession session = intercept @HTTP $ \case
      HttpRequest req | isLLMRequest req -> traceRequest session req
                      | otherwise -> send $ HttpRequest req

-- | Trace a single LLM request within a session
traceRequest :: Members '[HTTP, RestAPI LangFuse, Time, Logging] r
             => LangFuseSession
             -> HTTPRequest
             -> Sem r (Either String HTTPResponse)
traceRequest session req = do
  -- Use session trace ID and generate a new span ID for this request
  traceId <- generateTraceId  -- Each span gets its own trace ID
  spanId <- generateSpanId
  startTime <- getCurrentTime

  -- Execute request
  result <- send $ HttpRequest req
  endTime <- getCurrentTime

  -- Build and export span with session ID attribute
  let sessionId = sessionTraceId session
      span = buildSpanWithSession sessionId traceId spanId req startTime endTime result
      exportReq = mkExportRequest span

  -- Log and export (best-effort)
  info $ fromString $ "LangFuse: Exporting span " <> T.unpack (unSpanId spanId) <> " for session " <> T.unpack (unTraceId sessionId)
  (_ :: Value) <- RestAPI.post @LangFuse (Endpoint "v1/traces") exportReq
  info $ fromString $ "LangFuse: Exported span " <> T.unpack (unSpanId spanId)

  pure result
