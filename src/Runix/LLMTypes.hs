{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Runix.LLMTypes where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (Value, encode, eitherDecode, toJSON)
import Data.Aeson.Types (parseEither)
import GHC.Generics (Generic, Rep, M1(..), K1(..), (:*:), (:+:), from, to, D, C, S)
import Polysemy
import Polysemy.Fail
import Runix.LLM.Effects (LLM, askLLM)
import Autodocodec
import Autodocodec.Schema

-- | Text specifically formatted for LLM prompts
type PromptText = Text

-- | Types that can be converted to/from LLM text representation
class LLMText a where
  toLLMText :: a -> Text
  fromLLMText :: Text -> Either String a

  default toLLMText :: (Generic a, GLLMText (Rep a)) => a -> Text
  toLLMText = gToLLMText . from

  default fromLLMText :: (Generic a, GLLMText (Rep a)) => Text -> Either String a
  fromLLMText = fmap to . gFromLLMText

-- | Generic implementation for LLMText
class GLLMText f where
  gToLLMText :: f p -> Text
  gFromLLMText :: Text -> Either String (f p)

-- | Implementation for newtype wrappers around Text
instance GLLMText (M1 D meta (M1 C meta' (M1 S meta'' (K1 i Text)))) where
  gToLLMText (M1 (M1 (M1 (K1 text)))) = text
  gFromLLMText text = Right (M1 (M1 (M1 (K1 text))))

-- | Implementation for newtype wrappers around TL.Text
instance GLLMText (M1 D meta (M1 C meta' (M1 S meta'' (K1 i TL.Text)))) where
  gToLLMText (M1 (M1 (M1 (K1 text)))) = TL.toStrict text
  gFromLLMText text = Right (M1 (M1 (M1 (K1 (TL.fromStrict text)))))

-- | Basic instances
instance LLMText Text where
  toLLMText = id
  fromLLMText = Right

instance LLMText TL.Text where
  toLLMText = TL.toStrict
  fromLLMText = Right . TL.fromStrict

-- | Type-safe format specification using GADTs
data LLMFormat a where
  StructuredFormat :: HasCodec a =>
    { serialize :: a -> Text
    , deserialize :: Text -> Either String a
    , structuredType :: Text  -- "json", "yaml"
    } -> LLMFormat a

  TextFormat ::
    { serialize :: a -> Text
    , deserialize :: Text -> Either String a
    , textType :: Text  -- "plain text", "markdown", "html"
    } -> LLMFormat a

-- | Main typeclass for LLM-compatible types
class HasLLMCodec a where
  llmDescription :: Text      -- What this type represents
  llmGuidance :: Text        -- How to format/structure it
  llmFormat :: LLMFormat a

-- | Convenience constructors for common formats
jsonFormat :: HasCodec a => LLMFormat a
jsonFormat = StructuredFormat
  { serialize = TE.decodeUtf8 . BSL.toStrict . encode . toJSONViaCodec
  , deserialize = \t -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 t) of
      Left err -> Left err
      Right val -> parseEither parseJSONViaCodec val
  , structuredType = "json"
  }

yamlFormat :: HasCodec a => LLMFormat a
yamlFormat = StructuredFormat
  { serialize = TE.decodeUtf8 . BSL.toStrict . encode . toJSONViaCodec  -- TODO: Use YAML when available
  , deserialize = \t -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 t) of
      Left err -> Left err
      Right val -> parseEither parseJSONViaCodec val
  , structuredType = "yaml"
  }

textFormat :: LLMText a => Text -> LLMFormat a
textFormat formatName = TextFormat
  { serialize = toLLMText
  , deserialize = fromLLMText
  , textType = formatName
  }

-- | Get JSON schema for structured formats only - requires explicit type application
getSchema :: forall a. HasCodec a => LLMFormat a -> Maybe Value
getSchema (StructuredFormat{}) = Just $ toJSON $ jsonSchemaViaCodec @a
getSchema (TextFormat{}) = Nothing

-- | Type-safe schema extraction for structured formats - requires explicit type application
getJSONSchema :: forall a. HasCodec a => LLMFormat a -> Value
getJSONSchema (StructuredFormat{}) = toJSON $ jsonSchemaViaCodec @a
getJSONSchema (TextFormat{}) = error "Cannot get JSON schema for text format"

-- | Get format type for prompts
getFormatType :: LLMFormat a -> Text
getFormatType (StructuredFormat{structuredType = t}) = t
getFormatType (TextFormat{textType = t}) = t

-- | Convert a value to LLM-readable text using its format
toLLMRepresentation :: forall a. HasLLMCodec a => a -> Text
toLLMRepresentation a = serialize (llmFormat @a) a

-- | Parse LLM output text using the type's format
fromLLMRepresentation :: forall a. HasLLMCodec a => Text -> Either String a
fromLLMRepresentation = deserialize (llmFormat @a)

-- | Get description for prompts
getLLMDescription :: forall a. HasLLMCodec a => Text
getLLMDescription = llmDescription @a

-- | Get guidance for prompts
getLLMGuidance :: forall a. HasLLMCodec a => Text
getLLMGuidance = llmGuidance @a

-- Core LLM functions using the new approach
createFrom :: forall provider model a b r. (HasLLMCodec a, HasLLMCodec b, Members '[LLM provider model, Fail] r) => Text -> b -> Sem r a
createFrom userPrompt input = do
    let augmentedPrompt = userPrompt <> "\nCreate " <> getLLMDescription @a <> " in " <> getFormatType (llmFormat @a) <> " format from the provided " <> getLLMDescription @b <> ". " <> getLLMGuidance @a
    let inputData = toLLMRepresentation input
    let combinedPrompt = augmentedPrompt <> "\n\nInput data:\n" <> inputData
    result <- askLLM combinedPrompt
    case fromLLMRepresentation result of
        Right value -> return value
        Left err -> fail $ "Failed to parse LLM response: " <> err

updateWith :: forall provider model a b r. (HasLLMCodec a, HasLLMCodec b, Members '[LLM provider model, Fail] r) => Text -> a -> b -> Sem r a
updateWith userPrompt existing input = do
    let augmentedPrompt = userPrompt <> "\nUpdate " <> getLLMDescription @a <> " in " <> getFormatType (llmFormat @a) <> " format using the provided " <> getLLMDescription @b <> ". " <> getLLMGuidance @a
    let combinedInput = augmentedPrompt <> "\n\nExisting content:\n" <> toLLMRepresentation existing <> "\n\nNew information:\n" <> toLLMRepresentation input
    result <- askLLM combinedInput
    case fromLLMRepresentation result of
        Right value -> return value
        Left err -> fail $ "Failed to parse LLM response: " <> err

createPrompt :: forall provider model a r. (HasLLMCodec a, Members '[LLM provider model, Fail] r) => Text -> Sem r a
createPrompt userPrompt = do
    let augmentedPrompt = userPrompt <> "\nCreate " <> getLLMDescription @a <> " in " <> getFormatType (llmFormat @a) <> " format. " <> getLLMGuidance @a
    result <- askLLM augmentedPrompt
    case fromLLMRepresentation result of
        Right value -> return value
        Left err -> fail $ "Failed to parse LLM response: " <> err

-- | Generate structured output using LLM API schema enforcement
createStructured :: forall provider model a r. (HasLLMCodec a, HasCodec a, Members '[LLM provider model, Fail] r) => Text -> Sem r a
createStructured userPrompt = do
    let format = llmFormat @a
    case format of
        StructuredFormat{} -> do
            -- Use schema-enforced generation for structured formats
            let schema = getJSONSchema @a format
            let augmentedPrompt = userPrompt <> "\nRespond in valid " <> getFormatType format <> " format matching this structure: " <> T.take 200 (TE.decodeUtf8 $ BSL.toStrict $ encode schema)
            result <- askLLM augmentedPrompt
            case fromLLMRepresentation result of
                Right value -> return value
                Left err -> error $ "Structured generation failed: " <> err
        TextFormat{} -> do
            -- Fall back to text-based generation
            createPrompt userPrompt