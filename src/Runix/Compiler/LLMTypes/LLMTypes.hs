{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Runix.LLMTypes where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.Text.Encoding as TE
import Polysemy
import Runix.Effects (LLM, askLLM, queryLLM, LLMInstructions(..))

-- | Text specifically formatted for LLM prompts
type PromptText = Text

-- | Types that can be converted to meaningful LLM input text
class LLMInput a where
    toLLMText :: a -> PromptText
    inputDescription :: Text  -- What this type represents for the LLM
    inputFormat :: Text               -- Format for LLM output (markdown, json, plain text, etc.)
    inputFormat = "markdown"          -- Default format

-- | Types that can be parsed from LLM output text
class LLMOutput a where
    fromLLMText :: Text -> a  -- Parse LLM output
    description :: Text          -- What this type represents
    format :: Text               -- Format for LLM output (markdown, json, plain text, etc.)
    format = "markdown"          -- Default format

-- Core LLM functions using the typeclasses with explicit prompts
createFrom :: forall a b r. (LLMOutput a, LLMInput b, Members '[LLM] r) => Text -> b -> Sem r a
createFrom userPrompt input = do
    let augmentedPrompt = userPrompt <> "\nCreate " <> (description @a) <> " in " <> (format @a) <> " format from the provided " <> (inputDescription @b) <> "."
    let inputData = toLLMText input
    result <- queryLLM (LLMInstructions augmentedPrompt) inputData
    return $ fromLLMText result

updateWith :: forall a b r. (LLMOutput a, LLMInput a, LLMInput b, Members '[LLM] r) => Text -> a -> b -> Sem r a  
updateWith userPrompt existing input = do
    let augmentedPrompt = userPrompt <> "\nUpdate " <> (description @a) <> " in " <> (format @a) <> " format using the provided " <> (inputDescription @b) <> "."
    let combinedInput = "Existing content:\n" <> toLLMText existing <> "\n\nNew information:\n" <> toLLMText input
    result <- queryLLM (LLMInstructions augmentedPrompt) combinedInput
    return $ fromLLMText result

createPrompt :: forall a r. (LLMOutput a, Members '[LLM] r) => Text -> Sem r a
createPrompt userPrompt = do
    let augmentedPrompt = userPrompt <> "\nCreate " <> (description @a) <> " in " <> (format @a) <> " format."
    result <- askLLM augmentedPrompt
    return $ fromLLMText result


-- Basic instances for common types

-- Text instances
instance LLMInput Text where
    toLLMText = id
    inputDescription = "text content"


instance LLMOutput Text where
    fromLLMText = id
    description = "text content"
    format = "plain text"


-- List instances
instance LLMInput a => LLMInput [a] where
    toLLMText items = T.unlines $ map toLLMText items
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
    toLLMText (JSONInput a) = TE.decodeUtf8 . encode $ a
    inputDescription = "JSON data"

-- For structured parsing that might fail, use Maybe
instance (FromJSON a, ToJSON a) => LLMOutput (Maybe (JSONOutput a)) where
    fromLLMText = fmap JSONOutput . decode . TE.encodeUtf8
    description = "JSON data (might fail to parse)"
    format = "JSON"