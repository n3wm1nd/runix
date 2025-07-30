{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Runix.LLMTypes where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.Text.Lazy.Encoding as TLE
import Polysemy
import Polysemy.Fail
import Runix.Effects (LLM, askLLM, queryLLM, LLMInstructions(..))

-- | Types that can be created by LLM with specific prompting strategies
class LLMCreatable a where
    description :: Text
    format :: Text
    format = "markdown"
    
    -- Creation instructions (data passed separately)
    creationPrompt :: Text -> TL.Text
    creationPrompt inputDesc = TL.fromStrict $ "Create " <> description @a <> " in " <> format @a <> " format from the provided " <> inputDesc <> "."
    
    -- Update instructions (data passed separately) 
    updatePrompt :: a -> Text -> TL.Text
    updatePrompt existing inputDesc = TL.fromStrict $ "Update this " <> description @a <> " with new information from the provided " <> inputDesc <> ".\n\nExisting content:\n" <> TL.toStrict (contentText existing)
    
    -- Reference methods for context management
    distinctName :: a -> Text    -- unique identifier for referencing
    shortName :: a -> Text       -- human-readable name
    contentText :: a -> TL.Text  -- extract the actual text content
    
    -- User-prompt creation (no input, just instruction)
    promptCreation :: Text -> TL.Text
    promptCreation userPrompt = TL.fromStrict $ "Create " <> description @a <> " in " <> format @a <> " format. " <> userPrompt

-- | Types that can be converted to meaningful LLM input text
class LLMInput a where
    toLLMText :: a -> TL.Text
    inputDescription :: Text  -- What this type represents for the LLM

-- | Types that can be parsed from LLM output text
class LLMOutput a where
    fromLLMText :: TL.Text -> Maybe a  -- Parse LLM output, Nothing on failure

-- Core LLM functions using the typeclasses
createFrom :: forall a b r. (LLMCreatable a, LLMOutput a, LLMInput b, Members '[LLM, Fail] r) => b -> Sem r a
createFrom input = do
    let instructions = creationPrompt @a (inputDescription @b)
    let inputData = toLLMText input
    result <- queryLLM (LLMInstructions instructions) inputData
    case fromLLMText result of
        Just parsed -> return parsed
        Nothing -> fail $ "Failed to parse LLM output for " <> T.unpack (description @a)

updateWith :: forall a b r. (LLMCreatable a, LLMOutput a, LLMInput b, Members '[LLM, Fail] r) => a -> b -> Sem r a  
updateWith existing input = do
    let instructions = updatePrompt existing (inputDescription @b)
    let inputData = toLLMText input
    result <- queryLLM (LLMInstructions instructions) inputData
    case fromLLMText result of
        Just parsed -> return parsed
        Nothing -> fail $ "Failed to parse LLM output for " <> T.unpack (description @a)

createPrompt :: forall a r. (LLMCreatable a, LLMOutput a, Members '[LLM, Fail] r) => Text -> Sem r a
createPrompt userPrompt = do
    result <- askLLM (promptCreation @a userPrompt)
    case fromLLMText result of
        Just parsed -> return parsed
        Nothing -> fail $ "Failed to parse LLM output for " <> T.unpack (description @a)

-- Context management helpers
referenceList :: LLMCreatable a => [a] -> TL.Text
referenceList items = TL.fromStrict $ T.unlines [shortName item <> ": " <> distinctName item | item <- items]

contextualPrompt :: LLMCreatable a => [a] -> TL.Text -> TL.Text  
contextualPrompt context prompt = 
    "Context:\n" <> referenceList context <> "\n\n" <> prompt

-- Basic instances for common types

-- Text instances
instance LLMInput Text where
    toLLMText = TL.fromStrict
    inputDescription = "text content"

instance LLMInput TL.Text where  
    toLLMText = id
    inputDescription = "text content"

instance LLMOutput Text where
    fromLLMText = Just . TL.toStrict

instance LLMOutput TL.Text where
    fromLLMText = Just

-- List instances
instance LLMInput a => LLMInput [a] where
    toLLMText items = TL.unlines $ map toLLMText items
    inputDescription = "list of " <> inputDescription @a

-- Maybe instances  
instance LLMInput a => LLMInput (Maybe a) where
    toLLMText Nothing = "(none)"
    toLLMText (Just a) = toLLMText a
    inputDescription = "optional " <> inputDescription @a

-- Wrapper for JSON types to avoid overlapping instances
newtype JSONInput a = JSONInput a
newtype JSONOutput a = JSONOutput a

instance (ToJSON a) => LLMInput (JSONInput a) where
    toLLMText (JSONInput a) = TLE.decodeUtf8 . encode $ a
    inputDescription = "JSON data"

instance (FromJSON a) => LLMOutput (JSONOutput a) where
    fromLLMText = fmap JSONOutput . decode . TLE.encodeUtf8