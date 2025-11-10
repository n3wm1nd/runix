{-# LANGUAGE OverloadedStrings #-}

module Runix.Streaming.SSE where

import Data.Aeson (Value, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString as BS
import Data.Text (Text)
import Control.Applicative ((<|>))
import qualified Data.Vector as V

-- | Parse Server-Sent Events (SSE) format
-- Returns a list of JSON values from "data: {...}" lines
parseSSE :: ByteString -> [Value]
parseSSE body =
    let lines = BSLC.lines body
        dataLines = [l | l <- lines, BSLC.isPrefixOf "data: " l]
        jsonStrings = [BSLC.drop 6 line | line <- dataLines]  -- Drop "data: " prefix
        -- Filter out [DONE] markers and parse JSON
        values = [v | json <- jsonStrings
                    , not (BSLC.isPrefixOf "[DONE]" json)
                    , Just v <- [decode json]]
    in values

-- | Generic SSE reassembler with provider-specific delta merger
-- The delta merger knows how to apply each SSE chunk to the accumulated response
reassembleSSE :: (response -> Value -> response)  -- ^ Delta merger (accumulator -> chunk -> accumulator)
              -> response                          -- ^ Initial response state
              -> ByteString                        -- ^ SSE response body
              -> response                          -- ^ Final accumulated response
reassembleSSE deltaMerger initial sseBody =
    let chunks = parseSSE sseBody
    in foldl deltaMerger initial chunks

-- | Extract text delta from a ByteString chunk (for streaming preview)
-- Parses SSE format and extracts text from both Anthropic and OpenAI streaming formats
extractTextFromChunk :: BS.ByteString -> Maybe Text
extractTextFromChunk chunk =
    let chunkLazy = BSL.fromStrict chunk
        values = parseSSE chunkLazy
    in case values of
        [] -> Nothing
        (val:_) -> extractTextDelta val
  where
    extractTextDelta :: Value -> Maybe Text
    extractTextDelta (Aeson.Object obj) = do
        -- Try Anthropic format first (content_block_delta)
        let anthropicResult = do
                Aeson.String eventType <- KM.lookup "type" obj
                if eventType == "content_block_delta"
                    then do
                        Aeson.Object delta <- KM.lookup "delta" obj
                        Aeson.String text <- KM.lookup "text" delta
                        return text
                    else Nothing

        -- Try OpenAI format (choices[0].delta.content)
        let openaiResult = do
                Aeson.Array choices <- KM.lookup "choices" obj
                choice <- choices V.!? 0
                case choice of
                    Aeson.Object choiceObj -> do
                        Aeson.Object delta <- KM.lookup "delta" choiceObj
                        Aeson.String content <- KM.lookup "content" delta
                        return content
                    _ -> Nothing

        anthropicResult <|> openaiResult
    extractTextDelta _ = Nothing
