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

-- | Streaming content that can be different types of chunks
-- This will be extended in the future for other chunk types (tool calls, etc.)
data StreamingContent
  = StreamingText Text       -- ^ Regular assistant response text
  | StreamingReasoning Text  -- ^ Reasoning/thinking content
  deriving (Show, Eq)

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

-- | Extract streaming content from a ByteString chunk (for streaming preview)
-- Parses SSE format and extracts text/reasoning from both Anthropic and OpenAI streaming formats
extractContentFromChunk :: BS.ByteString -> Maybe StreamingContent
extractContentFromChunk chunk =
    let chunkLazy = BSL.fromStrict chunk
        values = parseSSE chunkLazy
    in case values of
        [] -> Nothing
        (val:_) -> extractContentDelta val
  where
    extractContentDelta :: Value -> Maybe StreamingContent
    extractContentDelta (Aeson.Object obj) = do
        -- Try Anthropic format first (content_block_delta)
        let anthropicResult = do
                Aeson.String eventType <- KM.lookup "type" obj
                if eventType == "content_block_delta"
                    then do
                        Aeson.Object delta <- KM.lookup "delta" obj
                        -- Check if it's a thinking delta
                        let thinkingResult = do
                                Aeson.String deltaType <- KM.lookup "type" delta
                                if deltaType == "thinking_delta"
                                    then do
                                        Aeson.String thinking <- KM.lookup "thinking" delta
                                        return $ StreamingReasoning thinking
                                    else Nothing
                        -- Otherwise try regular text delta
                        let textResult = do
                                Aeson.String text <- KM.lookup "text" delta
                                return $ StreamingText text
                        thinkingResult <|> textResult
                    else Nothing

        -- Try OpenAI format (choices[0].delta.content or choices[0].delta.reasoning_content or choices[0].delta.reasoning or choices[0].delta.reasoning_details)
        let openaiResult = do
                Aeson.Array choices <- KM.lookup "choices" obj
                choice <- choices V.!? 0
                case choice of
                    Aeson.Object choiceObj -> do
                        Aeson.Object delta <- KM.lookup "delta" choiceObj
                        -- Try reasoning_content first (OpenAI native format)
                        let reasoningContentResult = do
                                Aeson.String reasoning <- KM.lookup "reasoning_content" delta
                                return $ StreamingReasoning reasoning
                        -- Try reasoning (OpenRouter direct field)
                        let reasoningResult = do
                                Aeson.String reasoning <- KM.lookup "reasoning" delta
                                return $ StreamingReasoning reasoning
                        -- Try reasoning_details (OpenRouter format with array of {text: "..."})
                        let reasoningDetailsResult = do
                                Aeson.Array details <- KM.lookup "reasoning_details" delta
                                -- Extract text from first detail object
                                detailObj <- details V.!? 0
                                case detailObj of
                                    Aeson.Object obj' -> do
                                        Aeson.String text <- KM.lookup "text" obj'
                                        return $ StreamingReasoning text
                                    _ -> Nothing
                        -- Otherwise try regular content
                        let contentResult = do
                                Aeson.String content <- KM.lookup "content" delta
                                return $ StreamingText content
                        reasoningContentResult <|> reasoningResult <|> reasoningDetailsResult <|> contentResult
                    _ -> Nothing

        anthropicResult <|> openaiResult
    extractContentDelta _ = Nothing

-- | Legacy function for backwards compatibility
-- Extract just text (ignoring reasoning chunks)
extractTextFromChunk :: BS.ByteString -> Maybe Text
extractTextFromChunk chunk = case extractContentFromChunk chunk of
    Just (StreamingText text) -> Just text
    _ -> Nothing
