{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Runix.LLM.ToolExecution
  ( executeTool
  ) where

import Polysemy
import Data.Text ()  -- Import only instances
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Aeson as Aeson

import UniversalLLM.Core.Tools (LLMTool, executeToolCallFromList)
import UniversalLLM.Core.Types (ToolCall(..), ToolResult(..))
import Runix.Logging.Effects (Logging, info, warning)

-- | Execute a tool call with automatic logging
-- Logs tool name, arguments, and results
executeTool
  :: Member Logging r
  => [LLMTool (Sem r)]
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

  -- Execute the tool
  result <- executeToolCallFromList tools tc

  -- Log the result
  case toolResultOutput result of
    Left err ->
      warning $ "  error: " <> err
    Right val ->
      info $ "  result: " <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode val))

  return result
