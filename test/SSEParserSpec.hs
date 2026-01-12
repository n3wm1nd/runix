{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module SSEParserSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import Data.Default (def)
import Runix.Streaming.SSE (extractContentFromChunk, StreamingContent(..), reassembleSSE)
import UniversalLLM.Protocols.OpenAI (OpenAIResponse(..), OpenAISuccessResponse(..), OpenAIChoice(..), OpenAIMessage(..), mergeOpenAIDelta, defaultOpenAISuccessResponse)
import UniversalLLM (Message(..), Model(..), fromProviderResponse)
import qualified UniversalLLM.Providers.OpenAI as OpenAI
import UniversalLLM.Providers.OpenAI (OpenRouter(..))

spec :: Spec
spec = do
  describe "SSE Parser - OpenRouter Reasoning" $ do
    it "extracts reasoning from delta.reasoning field (OpenRouter format)" $ do
      -- This is the actual format from OpenRouter GLM 4.5 streaming response
      let chunk = BS.pack "data: {\"id\":\"gen-1765381969-tDN9trahXBXJBmjRjPF4\",\"provider\":\"AtlasCloud\",\"model\":\"z-ai/glm-4.5-air:free\",\"object\":\"chat.completion.chunk\",\"created\":1765381969,\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"\",\"reasoning\":\"1\",\"reasoning_details\":[{\"type\":\"reasoning.text\",\"text\":\"1\",\"format\":\"unknown\",\"index\":0}]},\"finish_reason\":null,\"native_finish_reason\":null,\"logprobs\":null}]}\n\n"

      let result = extractContentFromChunk chunk

      -- Should extract reasoning text
      result `shouldBe` [StreamingReasoning "1"]

    it "extracts reasoning from delta.reasoning field with multi-char text" $ do
      let chunk = BS.pack "data: {\"id\":\"gen-1765381969-tDN9trahXBXJBmjRjPF4\",\"provider\":\"AtlasCloud\",\"model\":\"z-ai/glm-4.5-air:free\",\"object\":\"chat.completion.chunk\",\"created\":1765381969,\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"\",\"reasoning\":\"alyze\",\"reasoning_details\":[{\"type\":\"reasoning.text\",\"text\":\"alyze\",\"format\":\"unknown\",\"index\":0}]},\"finish_reason\":null,\"native_finish_reason\":null,\"logprobs\":null}]}\n\n"

      let result = extractContentFromChunk chunk

      result `shouldBe` [StreamingReasoning "alyze"]

    it "extracts regular content when no reasoning present" $ do
      let chunk = BS.pack "data: {\"id\":\"gen-123\",\"object\":\"chat.completion.chunk\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"finish_reason\":null}]}\n\n"

      let result = extractContentFromChunk chunk

      result `shouldBe` [StreamingText "Hello"]

    it "reassembles OpenRouter reasoning chunks into reasoning_content field" $ do
      -- Simulate a streaming response with reasoning chunks
      let chunk1 = "data: {\"choices\":[{\"delta\":{\"role\":\"assistant\",\"reasoning\":\"Think\"}}]}\n\n"
          chunk2 = "data: {\"choices\":[{\"delta\":{\"reasoning\":\"ing...\"}}]}\n\n"
          chunk3 = "data: {\"choices\":[{\"delta\":{\"content\":\"Answer\"}}]}\n\n"
          sseBody = BSL.fromChunks [BS.pack chunk1, BS.pack chunk2, BS.pack chunk3]
          emptyResp = OpenAISuccess defaultOpenAISuccessResponse

      let reassembled = reassembleSSE mergeOpenAIDelta emptyResp sseBody

      -- Check that reasoning was accumulated in reasoning_content
      case reassembled of
        OpenAISuccess (OpenAISuccessResponse (OpenAIChoice msg:_)) -> do
          reasoning_content msg `shouldBe` Just "Thinking..."
          content msg `shouldBe` Just "Answer"
          -- Also verify reasoning_details is not set (should be Nothing after reassembly)
          reasoning_details msg `shouldBe` Nothing
        _ -> expectationFailure "Expected successful response with choices"
