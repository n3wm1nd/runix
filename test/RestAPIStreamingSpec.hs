{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Tests for Runix.RestAPI.Streaming
--
-- Two properties:
--   a) Routing: streaming requests go through HTTPStreaming, never HTTP.
--      A request that reaches HTTP in a streaming context is a test failure.
--   b) Transparency: from the caller's perspective, a streaming request
--      looks identical to a normal RestAPI response.
module RestAPIStreamingSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Fail (Fail, runFail)
import Data.Aeson (Value, object, (.=), encode, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)

import Runix.HTTP (HTTP(..), HTTPStreaming, HTTPStreamResult(..))
import Runix.RestAPI (RestAPI(..), RestEndpoint(..), RestError(..), Endpoint(..), restRequest)
import Runix.RestAPI.Streaming (streamingRestAPI)
import Runix.StreamChunk (StreamChunk, ignoreChunks)
import Runix.Streaming (interpretStreamingStateful)

-- ============================================================================
-- Test auth
-- ============================================================================

data TestAuth = TestAuth

instance RestEndpoint TestAuth where
  apiroot _ = "http://test.local"
  authheaders _ = []

-- ============================================================================
-- Mock interpreters
-- ============================================================================

-- | HTTP interpreter that fails the test if reached.
-- Any streaming request that leaks down to HTTP is a routing bug.
httpMustNotBeCalled :: Member Fail r => Sem (HTTP : r) a -> Sem r a
httpMustNotBeCalled = interpret $ \case
  HttpRequest _ -> fail "routing bug: streaming request reached HTTP instead of HTTPStreaming"

-- | HTTPStreaming interpreter backed by a fixture chunk list.
-- The remaining chunks are the stream state — consumed one at a time on FetchItem.
mockHTTPStreaming :: Member Fail r => [BS.ByteString] -> Sem (HTTPStreaming : r) a -> Sem r a
mockHTTPStreaming allChunks = interpretStreamingStateful id
  (\_ -> return $ Right allChunks)
  (\case []     -> return (Nothing, [])
         (c:cs) -> return (Just c, cs))
  (\_ -> return $ HTTPStreamResult 200 [] BSL.empty)

-- | RestAPI interpreter returning a fixed Value.
-- Used to verify pass-through: if this is called, the interceptor didn't fire.
mockRestAPI :: Member Fail r => Value -> Sem (RestAPI TestAuth : r) a -> Sem r a
mockRestAPI fixed = interpret $ \case
  RestRequest _ _ _ -> return $ case decode (encode fixed) of
    Just v  -> Right v
    Nothing -> Left (ParseError "mock encode/decode failed" BSL.empty)

-- ============================================================================
-- Test runner (pure — no IO needed)
-- ============================================================================

runTest :: [BS.ByteString]   -- ^ SSE chunks for mockHTTPStreaming
        -> Value             -- ^ Fixed response for mockRestAPI (pass-through path)
        -> Sem '[RestAPI TestAuth, HTTPStreaming, HTTP, StreamChunk Value, Fail] (Either RestError Value)
        -> Either String (Either RestError Value)
runTest chunks passThroughResponse action =
  run
  . runFail
  . ignoreChunks
  . httpMustNotBeCalled
  . mockHTTPStreaming chunks
  . mockRestAPI passThroughResponse
  $ action

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Encode a list of JSON values as SSE event lines.
toSSEChunks :: [Value] -> [BS.ByteString]
toSSEChunks = map $ \v -> BSL.toStrict $ "data: " <> encode v <> "\n\n"

-- | Detect streaming requests by presence of "stream": true.
isStreamingVal :: Value -> Bool
isStreamingVal (Aeson.Object obj) =
  KM.lookup "stream" obj == Just (Aeson.Bool True)
isStreamingVal _ = False

-- | Reassemble SSE chunks by concatenating their "content" fields.
concatContent :: [Value] -> Value
concatContent chunks =
  let texts = [ t | Aeson.Object obj <- chunks
                  , Just (Aeson.String t) <- [KM.lookup "content" obj] ]
  in object [ "content" .= mconcat texts ]

-- ============================================================================
-- Spec
-- ============================================================================

assertResult :: Either String (Either RestError Value) -> Value -> IO ()
assertResult result expected = case result of
  Left err          -> fail $ "Fail: " ++ err
  Right (Left err)  -> fail $ "RestError: " ++ show err
  Right (Right val) -> val `shouldBe` expected

spec :: Spec
spec = describe "streamingRestAPI" $ do

  it "passes non-streaming requests through to RestAPI" $
    let body      = object [ "model" .= ("test" :: Text) ]
        fixedResp = object [ "content" .= ("from RestAPI" :: Text) ]
        result    = runTest [] fixedResp $
          streamingRestAPI TestAuth isStreamingVal concatContent (:[]) $
            restRequest @TestAuth "POST" (Endpoint "test") (Just body)
    in assertResult result fixedResp

  it "routes streaming requests through HTTPStreaming, not HTTP" $
    -- If HTTP is reached, httpMustNotBeCalled returns Left via Fail → assertResult fails.
    let body      = object [ "stream" .= True ]
        sseChunks = toSSEChunks [ object [ "content" .= ("Hello" :: Text) ] ]
        expected  = object [ "content" .= ("Hello" :: Text) ]
        result    = runTest sseChunks (object []) $
          streamingRestAPI TestAuth isStreamingVal concatContent (:[]) $
            restRequest @TestAuth "POST" (Endpoint "test") (Just body)
    in assertResult result expected

  it "reassembles SSE chunks into a response indistinguishable from a normal RestAPI call" $
    let body      = object [ "stream" .= True ]
        sseChunks = toSSEChunks
          [ object [ "content" .= ("Hello" :: Text) ]
          , object [ "content" .= (", world" :: Text) ]
          ]
        expected  = object [ "content" .= ("Hello, world" :: Text) ]
        result    = runTest sseChunks (object []) $
          streamingRestAPI TestAuth isStreamingVal concatContent (:[]) $
            restRequest @TestAuth "POST" (Endpoint "test") (Just body)
    in assertResult result expected
