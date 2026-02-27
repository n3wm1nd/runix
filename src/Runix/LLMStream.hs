{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | LLM streaming effect for interactive/abortable generation.
--
-- Returns raw SSE-formatted chunks from the LLM provider. Caller is responsible
-- for parsing, reassembly, and interpretation. Use this when you need:
--
-- * Interactive generation (display tokens as they arrive)
-- * Abort capability (stop mid-generation, keep partial results)
-- * Custom processing (handle chunks yourself)
--
-- For standard request/response, use the 'LLM' effect instead.
module Runix.LLMStream
  ( LLMStream(..)
  , startStream
  ) where

import Polysemy
import Data.Kind (Type)
import qualified Data.ByteString as BS
import Conduit (ConduitT)

-- Re-export universal-llm types
import UniversalLLM (Message(..), ModelConfig(..))

-- | LLM streaming effect - returns raw chunk source
--
-- Returns a conduit source of SSE-formatted bytes from the provider.
-- Stopping consumption of the source can trigger request cancellation.
data LLMStream model (m :: Type -> Type) a where
    StartStream :: [ModelConfig model]                    -- ^ Configuration (temperature, etc.)
                -> [Message model]                         -- ^ Messages to send
                -> LLMStream model m (ConduitT () BS.ByteString IO (), Int, [(String, String)])
                -- ^ (chunk source, status code, response headers)

-- | Start a streaming LLM request
--
-- Returns:
-- * A conduit source of raw bytes (SSE format)
-- * HTTP status code
-- * Response headers
--
-- The source yields chunks as they arrive from the provider. To consume:
--
-- @
-- (source, code, headers) <- startStream configs messages
-- runConduit $ source .| mapM_C processChunk
-- @
--
-- Or to parse SSE and extract content:
--
-- @
-- (source, _, _) <- startStream configs messages
-- runConduit $ source .| parseSSE .| extractContent .| sinkList
-- @
startStream :: Member (LLMStream model) r
            => [ModelConfig model]
            -> [Message model]
            -> Sem r (ConduitT () BS.ByteString IO (), Int, [(String, String)])
startStream configs messages = send (StartStream configs messages)
