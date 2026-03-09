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
  , withLangFuseStreaming
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
import Polysemy.State (State, get, modify, evalState)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, ToJSON(..), object, (.=), eitherDecode, decode)
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
import Data.Foldable (toList)
import System.Environment (lookupEnv)

import Runix.HTTP (HTTP(..), HTTPRequest(..), HTTPResponse(..), HTTPStreaming, HTTPStreamResult(..))
import Runix.RestAPI (RestAPI, RestEndpoint(..), Endpoint(..))
import qualified Runix.RestAPI as RestAPI
import Runix.Logging (Logging, info)
import Runix.Time (Time, getCurrentTime)
import Runix.Streaming (StreamId(..))
import qualified Runix.Streaming as Streaming
import Runix.Streaming.SSE (parseSSEChunks, SSEParseResult(..), SSEEvent(..))
import UniversalLLM.Protocols.Anthropic (AnthropicResponse(..), AnthropicSuccessResponse(..), AnthropicUsage(..), mergeAnthropicDelta)
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), mergeOpenAIDelta, defaultOpenAIMessage, defaultOpenAISuccessResponse, defaultOpenAIChoice)
import Autodocodec (toJSONViaCodec)

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

-- | Extract just the text content from an Anthropic content array
extractAnthropicContent :: Value -> Maybe Text
extractAnthropicContent (Aeson.Array arr) =
  let textParts = [txt | Aeson.Object item <- toList arr
                       , Just (Aeson.String txt) <- [KM.lookup "text" item]]
  in if null textParts then Nothing else Just (T.concat textParts)
extractAnthropicContent _ = Nothing

-- | Extract just the text content from OpenAI choices array
extractOpenAIContent :: Value -> Maybe Text
extractOpenAIContent (Aeson.Array arr) =
  let textParts = [txt | Aeson.Object choice <- toList arr
                       , Just (Aeson.Object msg) <- [KM.lookup "message" choice]
                       , Just (Aeson.String txt) <- [KM.lookup "content" msg]]
  in if null textParts then Nothing else Just (T.concat textParts)
extractOpenAIContent _ = Nothing

-- | Parse SSE streaming body and merge all JSON deltas, extracting just the content text
-- Tries Anthropic protocol first, then OpenAI protocol, then falls back to Nothing
parseSSEToJSON :: BSL.ByteString -> Maybe (Text, Maybe Value)
parseSSEToJSON bodyBytes =
  let SSEParseResult sseEvents _remainder = parseSSEChunks (BSL.toStrict bodyBytes)
      -- Parse all SSE events as JSON values
      jsonValues = [val | event <- sseEvents, Just val <- [decode (BSL.fromStrict (sseEventData event))]]
  in case jsonValues of
       [] -> Nothing
       _ ->
         -- Try Anthropic protocol
         let anthropicInit = AnthropicSuccess $ AnthropicSuccessResponse "" "" "assistant" [] Nothing (AnthropicUsage 0 0)
             anthropicMerged = foldl mergeAnthropicDelta anthropicInit jsonValues
             anthropicJSON = toJSONViaCodec anthropicMerged
         in case anthropicJSON of
              Aeson.Object obj ->
                case (KM.lookup "content" obj >>= extractAnthropicContent, KM.lookup "usage" obj) of
                  (Just content, usage) -> Just (content, usage)
                  _ ->
                    -- Try OpenAI protocol
                    let openaiInit = OpenAISuccess $ defaultOpenAISuccessResponse
                          { choices = [defaultOpenAIChoice { message = defaultOpenAIMessage { role = "assistant" } }] }
                        openaiMerged = foldl mergeOpenAIDelta openaiInit jsonValues
                        openaiJSON = toJSONViaCodec openaiMerged
                    in case openaiJSON of
                         Aeson.Object obj2 ->
                           case (KM.lookup "choices" obj2 >>= extractOpenAIContent, KM.lookup "usage" obj2) of
                             (Just content, usage) -> Just (content, usage)
                             _ -> Nothing
                         _ -> Nothing
              _ ->
                -- Try OpenAI protocol
                let openaiInit = OpenAISuccess $ defaultOpenAISuccessResponse
                      { choices = [defaultOpenAIChoice { message = defaultOpenAIMessage { role = "assistant" } }] }
                    openaiMerged = foldl mergeOpenAIDelta openaiInit jsonValues
                    openaiJSON = toJSONViaCodec openaiMerged
                in case openaiJSON of
                     Aeson.Object obj ->
                       case (KM.lookup "choices" obj >>= extractOpenAIContent, KM.lookup "usage" obj) of
                         (Just content, usage) -> Just (content, usage)
                         _ -> Nothing
                     _ -> Nothing

-- | Parse response body for GenAI attributes
outputAttributes :: Either String HTTPResponse -> [(Text, Value)]
outputAttributes = \case
  Right resp -> parseResponseBody resp.body
  Left err -> [("langfuse.observation.status_message", Aeson.String $ T.pack err)]
  where
    parseResponseBody bodyBytes =
      case eitherDecode bodyBytes of
        Right (Aeson.Object obj) ->
          -- Direct JSON response (non-streaming)
          -- Extract content text for better LangFuse rendering
          let contentText = extractDirectContent bodyBytes obj
              outputAttr = [("langfuse.observation.output", Aeson.String contentText)]
              usageAttr = maybe [] (\u -> [("gen_ai.usage", u)]) $ KM.lookup "usage" obj
          in outputAttr <> usageAttr
        _ ->
          -- Not direct JSON - try parsing as SSE
          case parseSSEToJSON bodyBytes of
            Just (contentText, maybeUsage) ->
              -- Successfully merged SSE deltas and extracted content
              let outputAttr = [("langfuse.observation.output", Aeson.String contentText)]
                  usageAttr = maybe [] (\u -> [("gen_ai.usage", u)]) maybeUsage
              in outputAttr <> usageAttr
            Nothing ->
              -- Couldn't parse as SSE/JSON - include raw body
              [("langfuse.observation.output", Aeson.String $ bytesToText bodyBytes)]

    -- Extract content from direct (non-streaming) JSON response
    extractDirectContent bodyBytes obj =
      -- Try Anthropic format first
      case KM.lookup "content" obj >>= extractAnthropicContent of
        Just txt -> txt
        Nothing ->
          -- Try OpenAI format
          case KM.lookup "choices" obj >>= extractOpenAIContent of
            Just txt -> txt
            Nothing -> bytesToText bodyBytes  -- Fallback to raw JSON

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

-- | Export a span to LangFuse
exportSpan :: Members '[RestAPI LangFuse, Logging] r
           => TraceId  -- ^ Session ID
           -> SpanId
           -> Span
           -> Sem r ()
exportSpan sessionId spanId span = do
  let exportReq = mkExportRequest span
  info $ fromString $ "LangFuse: Exporting span " <> T.unpack (unSpanId spanId) <> " for session " <> T.unpack (unTraceId sessionId)
  (_ :: Value) <- RestAPI.post @LangFuse (Endpoint "v1/traces") exportReq
  info $ fromString $ "LangFuse: Exported span " <> T.unpack (unSpanId spanId)

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

  exportSpan sessionId spanId span
  pure result

-- ============================================================================
-- Streaming Interceptor
-- ============================================================================

-- | State for tracking streaming requests by StreamId
type StreamRequestMap = Map.Map StreamId (UTCTime, HTTPRequest)

-- | Intercept HTTPStreaming requests to trace LLM calls
-- Similar to withLangFuse but for streaming requests
withLangFuseStreaming :: forall r a. Members '[HTTPStreaming, RestAPI LangFuse, Time, Logging] r
                      => LangFuse
                      -> Sem r a
                      -> Sem r a
withLangFuseStreaming _langfuse action = do
  -- Generate a session trace ID for this entire session
  sessionTrace <- generateTraceId
  let session = LangFuseSession sessionTrace

  let handleStreaming :: forall x rInitial. HTTPStreaming (Sem rInitial) x
                      -> Sem (Polysemy.State.State StreamRequestMap : r) x
      handleStreaming = \case
        Streaming.StartStream req -> do
          timestamp <- getCurrentTime
          result <- send $ Streaming.StartStream req
          -- Store request config for this stream if it's an LLM request
          case result of
            Right sid | isLLMRequest req -> Polysemy.State.modify @StreamRequestMap $ Map.insert sid (timestamp, req)
            _ -> return ()
          return result

        Streaming.FetchItem sid -> send $ Streaming.FetchItem sid

        Streaming.CloseStream sid -> do
          result <- send $ Streaming.CloseStream sid

          -- Retrieve the request config for this stream
          requestMap <- Polysemy.State.get @StreamRequestMap
          case Map.lookup sid requestMap of
            Just (startTime, req) -> do
              endTime <- getCurrentTime

              -- Build span from the streaming result
              let sessionId = sessionTraceId session
              traceId <- generateTraceId
              spanId <- generateSpanId

              let httpResp = HTTPResponse
                    { code = streamStatusCode result
                    , headers = streamHeaders result
                    , body = streamBody result
                    }
                  span = buildSpanWithSession sessionId traceId spanId req startTime endTime (Right httpResp)

              -- Debug: log body size
              info $ fromString $ "LangFuse: Streaming response body size: " <> show (BSL.length (streamBody result)) <> " bytes"

              -- Export using the common function
              exportSpan sessionId spanId span

              -- Clean up the map
              Polysemy.State.modify @StreamRequestMap $ Map.delete sid

            Nothing -> return ()

          return result

  Polysemy.State.evalState (Map.empty :: StreamRequestMap) . intercept @HTTPStreaming handleStreaming . raise $ action
