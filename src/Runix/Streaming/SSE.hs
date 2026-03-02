{-# LANGUAGE OverloadedStrings #-}

module Runix.Streaming.SSE
  ( -- * Core SSE types
    SSEEvent(..)
  , SSEParseResult(..)
    -- * Parsing functions
  , parseSSEChunks
  , splitOnEventTerminator
  , parseEvent
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (foldl')
import Data.Word (Word8)

-- | Byte constants for line terminators
cr, lf :: Word8
cr = 13  -- \r
lf = 10  -- \n

-- | Represents a Server-Sent Event according to the SSE specification
data SSEEvent = SSEEvent
  { sseEventId :: Maybe Text       -- ^ Optional event ID
  , sseEventType :: Maybe Text     -- ^ Optional event type (defaults to "message")
  , sseEventData :: ByteString     -- ^ Event data (concatenated from all data fields)
  , sseRetry :: Maybe Int          -- ^ Optional retry interval
  } deriving (Show, Eq)

-- | Result of parsing SSE stream chunks
data SSEParseResult = SSEParseResult
  { parsedEvents :: [SSEEvent]   -- ^ Complete events found
  , remainder :: ByteString      -- ^ Unparsed remainder (incomplete event)
  } deriving (Show, Eq)

-- | Parse SSE chunks into complete events and remainder
-- According to SSE spec, events are terminated by a blank line (double newline)
parseSSEChunks :: ByteString -> SSEParseResult
parseSSEChunks input =
    let (completeSections, rest) = splitOnEventTerminator input
        nonEmptySections = filter (not . BS.null) completeSections
        events = map parseEvent nonEmptySections
    in SSEParseResult events rest

-- | Split input on event terminators (blank lines), returning complete sections and remainder
-- Per SSE spec, valid event terminators are: \n\n, \r\n\r\n, or \r\r
splitOnEventTerminator :: ByteString -> ([ByteString], ByteString)
splitOnEventTerminator input = go [] BS.empty input
  where
    go acc current bs =
        case BS.uncons bs of
            Nothing -> (reverse acc, current)
            Just (c, rest)
                | c == cr ->
                    case BS.uncons rest of
                        Just (c2, rest2)
                            | c2 == lf ->
                                case BS.uncons rest2 of
                                    Just (c3, rest3)
                                        | c3 == cr ->
                                            case BS.uncons rest3 of
                                                Just (c4, rest4) | c4 == lf ->
                                                    go (current : acc) BS.empty rest4  -- \r\n\r\n
                                                _ -> go acc (BS.snoc (BS.snoc (BS.snoc current cr) lf) cr) rest3
                                        | c3 == lf ->
                                            go (current : acc) BS.empty rest3  -- \r\n\n (mixed)
                                        | otherwise ->
                                            go acc (BS.snoc (BS.snoc current cr) lf) rest2
                            | c2 == cr ->
                                go (current : acc) BS.empty rest2  -- \r\r
                            | otherwise ->
                                go acc (BS.snoc current cr) rest
                        Nothing -> go acc (BS.snoc current cr) rest
                | c == lf ->
                    case BS.uncons rest of
                        Just (c2, rest2) | c2 == lf ->
                            go (current : acc) BS.empty rest2  -- \n\n
                        _ -> go acc (BS.snoc current lf) rest
                | otherwise ->
                    go acc (BS.snoc current c) rest

-- | Parse a single complete event section into an SSEEvent
parseEvent :: ByteString -> SSEEvent
parseEvent eventBytes =
    let lines = splitLines eventBytes
        initialEvent = SSEEvent Nothing Nothing BS.empty Nothing
    in foldl' processLine initialEvent lines
  where
    -- Split on line terminators: \r\n, \n, or \r
    splitLines :: ByteString -> [ByteString]
    splitLines bs
        | BS.null bs = []
        | otherwise =
            case BS.uncons bs of
                Nothing -> []
                Just (c, rest)
                    | c == cr ->
                        case BS.uncons rest of
                            Just (c2, rest2) | c2 == lf ->
                                BS.empty : splitLines rest2  -- \r\n
                            _ -> BS.empty : splitLines rest  -- \r
                    | c == lf ->
                        BS.empty : splitLines rest  -- \n
                    | otherwise ->
                        let (line, remainder) = spanLine bs
                        in line : splitLines remainder

    -- Span until we hit a line terminator
    spanLine :: ByteString -> (ByteString, ByteString)
    spanLine bs =
        case BS.findIndex (\c -> c == cr || c == lf) bs of
            Nothing -> (bs, BS.empty)
            Just idx ->
                let (before, after) = BS.splitAt idx bs
                in (before, after)

    processLine :: SSEEvent -> ByteString -> SSEEvent
    processLine event line
      | BS.null line = event
      | BS.head line == 58 = event  -- 58 is ':', comment line
      | otherwise =
          let (field, afterField) = BS.break (== 58) line  -- 58 is ':'
              value = case BS.stripPrefix ":" afterField of
                  Nothing -> BS.empty  -- No colon, empty value
                  Just afterColon ->
                      case BS.stripPrefix " " afterColon of
                          Just afterSpace -> afterSpace
                          Nothing -> afterColon
              fieldText = TE.decodeUtf8 field
          in case fieldText of
               "event" -> event { sseEventType = Just (TE.decodeUtf8 value) }
               "data" ->
                   let currentData = sseEventData event
                       newData = if BS.null currentData
                                 then value
                                 else currentData <> "\n" <> value
                   in event { sseEventData = newData }
               "id" -> event { sseEventId = Just (TE.decodeUtf8 value) }
               "retry" ->
                   let retryMs = case reads (T.unpack (TE.decodeUtf8 value)) of
                                   [(n, "")] -> Just n
                                   _ -> Nothing
                   in event { sseRetry = retryMs }
               _ -> event  -- Unknown field, ignore per spec
