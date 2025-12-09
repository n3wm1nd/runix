{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Runix.LLM.ToolExecution
  ( executeTool
  ) where

import Polysemy
import Polysemy.Fail (Fail, runFail)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Aeson as Aeson

import UniversalLLM.Core.Tools (LLMTool, executeToolCallFromList)
import UniversalLLM.Core.Types (ToolCall(..), ToolResult(..))
import Runix.Logging.Effects (Logging, info, warning)

-- | Execute a tool call with automatic logging
-- Logs tool name, arguments, and results
-- Tools have access to the Fail effect - failures are converted to error results
executeTool
  :: Member Logging r
  => [LLMTool (Sem (Fail ': r))]
  -> ToolCall
  -> Sem r ToolResult
executeTool tools tc = do
  -- Log the tool call
  case tc of
    ToolCall _ name args -> do
      let argsText = TL.toStrict $ TLE.decodeUtf8 $ Aeson.encode args
      info $ name <> "(" <> argsText <> ")"
    InvalidToolCall _ name _rawArgs err -> do
      warning $ name <> " [INVALID: " <> err <> "]"

  -- Execute the tool with Fail available, then interpret Fail into error results
  failResult <- runFail $ executeToolCallFromList tools tc

  let result = case failResult of
        Left errMsg ->
          -- Tool called fail - convert to error result
          ToolResult tc (Left (T.pack errMsg))
        Right toolResult ->
          -- Tool succeeded normally
          toolResult

  -- Log the result
  case toolResultOutput result of
    Left err ->
      warning $ "  error: " <> err
    Right val ->
      info $ "  result: " <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode val))

  return result
