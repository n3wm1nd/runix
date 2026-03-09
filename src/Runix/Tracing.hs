{-# LANGUAGE Trustworthy #-}

-- | HTTP request tracing and observability
--
-- This module provides two complementary tracing mechanisms:
--
-- * 'Runix.Tracing.LangFuse' - Production observability via LangFuse/OpenTelemetry
-- * 'Runix.Tracing.FileLog' - Local file-based request logging for debugging
--
-- Both can be used independently or together. LangFuse is recommended for
-- production monitoring and analytics, while file logging is useful for
-- local development and debugging.
--
-- Example usage:
--
-- @
-- -- LangFuse tracing (production)
-- langfuse <- langFuseFromEnv
-- whenJust langfuse $ \\lf -> withLangFuse lf $ do
--   -- LLM calls are traced to LangFuse
--
-- -- File logging (development)
-- withHTTPFileLogging "/path/to/project" $ do
--   -- HTTP requests logged to .runix/logs/
-- @
--
module Runix.Tracing
  ( -- * LangFuse/OTLP Tracing
    module Runix.Tracing.LangFuse
    -- * File-Based Logging
  , module Runix.Tracing.FileLog
  ) where

import Runix.Tracing.LangFuse
import Runix.Tracing.FileLog
