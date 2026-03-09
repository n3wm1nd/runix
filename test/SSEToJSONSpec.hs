{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SSEToJSONSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Aeson (Value, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Runix.Tracing.LangFuse (parseSSEToJSON)
import Runix.HTTP (HTTPResponse(..))
import qualified Runix.Tracing.LangFuse as LangFuse

spec :: Spec
spec = describe "parseSSEToJSON" $ do

  it "parses OpenAI SSE response with content" $ do
    let sseBody = BSL.concat
          [ "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":null}}]}\n\n"
          , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"content\":\"Hello\"}}]}\n\n"
          , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"content\":\" world\"}}]}\n\n"
          , "data: {\"choices\":[{\"finish_reason\":\"stop\",\"index\":0,\"delta\":{}}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5}}\n\n"
          , "data: [DONE]\n\n"
          ]

    case parseSSEToJSON sseBody of
      Nothing -> expectationFailure "Failed to parse SSE"
      Just (mergedJSON, maybeUsage) -> do
        -- Check we got usage
        maybeUsage `shouldSatisfy` (\case Just _ -> True; Nothing -> False)

        -- Check merged JSON has content
        case mergedJSON of
          Aeson.Object obj -> do
            case KM.lookup "choices" obj of
              Just (Aeson.Array arr) -> length arr `shouldBe` 1
              _ -> expectationFailure "No choices array"
            case KM.lookup "usage" obj of
              Just _ -> return ()
              Nothing -> expectationFailure "No usage field in merged response"
          _ -> expectationFailure "Result is not an object"

        -- The merged content should be "Hello world"
        let jsonStr = show mergedJSON
        jsonStr `shouldContain` "Hello world"

  it "parses OpenAI SSE response with tool calls" $ do
    let sseBody = BSL.concat
          [ "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"role\":\"assistant\",\"tool_calls\":[{\"index\":0,\"id\":\"call_123\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\",\"arguments\":\"\"}}]}}]}\n\n"
          , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"location\\\"\"}}]}}]}\n\n"
          , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\":\\\"Paris\\\"}\"}}]}}]}\n\n"
          , "data: {\"choices\":[{\"finish_reason\":\"tool_calls\",\"index\":0,\"delta\":{}}],\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":20}}\n\n"
          , "data: [DONE]\n\n"
          ]

    case parseSSEToJSON sseBody of
      Nothing -> expectationFailure "Failed to parse SSE with tool calls"
      Just (mergedJSON, maybeUsage) -> do
        -- Check we got usage
        maybeUsage `shouldSatisfy` (\case Just _ -> True; Nothing -> False)

        let jsonStr = show mergedJSON
        -- Should contain tool call info
        jsonStr `shouldContain` "tool_calls"
        jsonStr `shouldContain` "get_weather"
        jsonStr `shouldContain` "Paris"

  describe "outputAttributes - OpenInference format" $ do
    it "generates correct llm.output_messages attributes for OpenAI tool calls" $ do
      let sseBody = BSL.concat
            [ "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"role\":\"assistant\",\"tool_calls\":[{\"index\":0,\"id\":\"call_123\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\",\"arguments\":\"\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"location\\\"\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\":\\\"Paris\\\"}\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":\"tool_calls\",\"index\":0,\"delta\":{}}],\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":20}}\n\n"
            , "data: [DONE]\n\n"
            ]

      let response = HTTPResponse 200 [] sseBody
          attrs = LangFuse.outputAttributes (Right response)
          attrMap = KM.fromList [(Key.fromText k, v) | (k, v) <- attrs]

      putStrLn $ "\n=== Generated Attributes ==="
      mapM_ (\(k, v) -> putStrLn $ T.unpack k ++ " = " ++ show v) attrs

      -- Should have OpenInference attributes for tool calls
      KM.lookup "llm.output_messages.0.message.role" attrMap `shouldSatisfy` (\case Just (Aeson.String "assistant") -> True; _ -> False)
      KM.lookup "llm.output_messages.0.message.tool_calls.0.tool_call.function.name" attrMap `shouldSatisfy` (\case Just (Aeson.String "get_weather") -> True; _ -> False)
      KM.lookup "llm.output_messages.0.message.tool_calls.0.tool_call.function.arguments" attrMap `shouldSatisfy` (\case Just (Aeson.String args) -> "{\"location\":\"Paris\"}" `T.isInfixOf` args; _ -> False)
      KM.lookup "llm.output_messages.0.message.tool_calls.0.tool_call.id" attrMap `shouldSatisfy` (\case Just (Aeson.String "call_123") -> True; _ -> False)

      -- Should have usage
      KM.lookup "gen_ai.usage" attrMap `shouldSatisfy` (\case Just _ -> True; Nothing -> False)

    it "generates attributes for real OpenAI tool call from logs" $ do
      let sseBody = BSL.concat
            [ "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":null}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"Whz9wbGRorwojmm0YzwRlsCUwvJdmDml\",\"type\":\"function\",\"function\":{\"name\":\"echo\",\"arguments\":\"\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"text\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\":\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"10\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":null,\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"}\"}}]}}]}\n\n"
            , "data: {\"choices\":[{\"finish_reason\":\"tool_calls\",\"index\":0,\"delta\":{}}]}\n\n"
            , "data: [DONE]\n\n"
            ]
          response = HTTPResponse 200 [] sseBody
          attrs = LangFuse.outputAttributes (Right response)

      putStrLn $ "\n=== Real Log Attributes (count: " ++ show (length attrs) ++ ") ==="
      mapM_ (\(k, v) -> putStrLn $ T.unpack k ++ " = " ++ show v) attrs

      -- Should have tool call attributes
      length attrs `shouldSatisfy` (> 0)

      let attrMap = KM.fromList [(Key.fromText k, v) | (k, v) <- attrs]
      KM.lookup "llm.output_messages.0.message.tool_calls.0.tool_call.function.name" attrMap `shouldSatisfy` (\case Just (Aeson.String "echo") -> True; _ -> False)
