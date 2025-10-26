{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Design prototype for LLMCreatable typeclass
-- This explores the abstraction opportunities from dndwiki task

module LLMCreatableDesign where

import Data.Text (Text)
import qualified Data.Text as T
import Polysemy

-- Current pattern analysis from dndwiki:
-- 1. Each llmQuery call has a specific prompt for creating a type
-- 2. The prompts contain type-specific descriptions and formatting instructions
-- 3. Common pattern: "Create X with these characteristics from this input"

-- Design Option 1: Simple description-based typeclass
class LLMCreatable a where
    description :: Text           -- "a journal of a dnd session..."
    format :: Text               -- "markdown", "json", "yaml", etc.
    format = "markdown"          -- default to markdown

-- Usage would be:
-- instance LLMCreatable SessionJournal where
--   description = "a journal of a dnd session, containing a cronological list of significant events as they happened"
--   format = "markdown"

-- REFINED DESIGN: Comprehensive LLM operations
class LLMCreatable a where
    description :: Text
    format :: Text
    format = "markdown"
    
    -- Creation from input
    creationPrompt :: Text -> Text
    creationPrompt input = "Create " <> description <> " in " <> format <> " format from:\n\n" <> input
    
    -- Update existing with new input  
    updatePrompt :: a -> Text -> Text
    updatePrompt existing input = "Update this " <> description <> " with new information:\n\nExisting:\n" <> contentText existing <> "\n\nNew input:\n" <> input
    
    -- Reference methods for context management
    distinctName :: a -> Text    -- unique identifier for referencing
    shortName :: a -> Text       -- human-readable name
    contentText :: a -> Text     -- extract the actual text content
    
    -- User-prompt creation (no input, just instruction)
    promptCreation :: Text -> Text
    promptCreation userPrompt = "Create " <> description <> " in " <> format <> " format. " <> userPrompt

-- Example instances for dndwiki types:

-- Current dndwiki types (from analysis):
newtype SessionJournal = SessionJournal Text
newtype SessionSummary = SessionSummary Text  
newtype ProcessedTranscript = ProcessedTranscript Text
newtype EntityContent = EntityContent Text

-- Concrete instances for dndwiki types:
instance LLMCreatable SessionJournal where
    description = "a session journal with chronological events, linked entities using [[Entity Name]] format, and markdown formatting"
    
    distinctName (SessionJournal content) = 
        -- Extract session name from content or generate from first few words
        "session-" <> (T.take 20 $ T.filter (/= ' ') $ T.take 100 content)
    
    shortName (SessionJournal content) = 
        -- Try to extract "Session X" from content, fallback to "Session"
        "Session " <> T.take 10 content
    
    contentText (SessionJournal content) = content

instance LLMCreatable SessionSummary where
    description = "a concise summary focusing on key story developments, character moments, locations, combat, and unresolved plot threads"
    
    distinctName (SessionSummary content) = 
        "summary-" <> (T.take 15 $ T.filter (/= ' ') $ T.take 50 content)
    
    shortName (SessionSummary content) = "Summary"
    contentText (SessionSummary content) = content

instance LLMCreatable EntityContent where
    description = "detailed wiki content for a D&D entity (character, location, item, organization) with appropriate tags and relationships"
    
    -- Custom prompts for entities
    creationPrompt input = 
        "Create detailed wiki content for a D&D entity based on this information. " <>
        "Include appropriate tags (#character #npc #location #item #organization), " <>
        "brief description, relationships, and any relevant details. " <>
        "Format in markdown:\n\n" <> input
    
    updatePrompt (EntityContent existing) input =
        "Update this entity wiki page with new information. Don't duplicate existing content:\n\n" <>
        "Existing page:\n" <> existing <> "\n\n" <>
        "New information to integrate:\n" <> input
    
    distinctName (EntityContent content) = 
        -- Extract entity name from first line or heading
        T.take 30 $ T.strip $ T.takeWhile (/= '\n') $ content
    
    shortName (EntityContent content) = 
        -- Get just the name without tags/descriptions
        T.take 15 $ T.strip $ T.takeWhile (/= '\n') $ content
    
    contentText (EntityContent content) = content

-- Corresponding LLM effect functions:
createFrom :: forall a r. (LLMCreatable a, Members '[LLM] r) => Text -> Sem r a
createFrom input = do
    result <- askLLM (creationPrompt @a input)
    return (parseOrFail result)

updateWith :: forall a r. (LLMCreatable a, Members '[LLM] r) => a -> Text -> Sem r a  
updateWith existing input = do
    result <- askLLM (updatePrompt existing input)
    return (parseOrFail result)

createPrompt :: forall a r. (LLMCreatable a, Members '[LLM] r) => Text -> Sem r a
createPrompt userPrompt = do
    result <- askLLM (promptCreation @a userPrompt)
    return (parseOrFail result)

-- Context management helpers:
referenceList :: LLMCreatable a => [a] -> Text
referenceList items = unlines [shortName item <> ": " <> distinctName item | item <- items]

contextualPrompt :: LLMCreatable a => [a] -> Text -> Text  
contextualPrompt context prompt = 
    "Context:\n" <> referenceList context <> "\n\n" <> prompt

-- Usage examples:
-- journal <- createFrom @SessionJournal processedTranscript
-- updatedJournal <- updateWith journal newSessionContent
-- entity <- createPrompt @EntityContent "Create content for a new NPC wizard named Eldara"

-- Context usage:
-- allEntities <- loadAllEntities
-- newEntity <- createFrom @EntityContent (contextualPrompt allEntities sessionJournal)

-- Questions to consider:
-- 1. How to handle conversion between Text types?
-- 2. Should we support different input types beyond Text?
-- 3. How to handle context (like existing wiki content)?
-- 4. Should the typeclass include parsing/validation?
-- 5. How to make this work with Read/Show for CLI integration?

-- Alternative: Context-passing approach
createFromWithContext :: forall a r. (LLMCreatable a, Members '[LLM] r) => 
    Maybe Text -> Text -> Sem r a

-- This could handle cases like:
-- entity <- createFromWithContext @EntityContent (Just existingEntityContent) sessionJournal
-- summary <- createFromWithContext @SessionSummary (Just worldContext) sessionJournal

-- HOW THIS SIMPLIFIES DNDWIKI CODE:

-- BEFORE (current dndwiki code):
{-
createAndWriteSessionJournal sessionName processedText = do
  journalContent <- llmQuery prompt processedText
  writePage (PageName ("Session " <> sessionName)) journalContent
  return $ SessionJournal journalContent
  where
    prompt = T.unlines
      [ "Convert this D&D transcript into a session journal with the following format:"
      , "- Chronological events with timestamps where available"
      , "- Link entities using [[Entity Name]] format for characters, locations, items"
      -- ... many more lines of prompt
      ]

updatePrompt entityName existingContent = T.unlines
  [ "Update this entity page with new information from the session journal."
  , "Existing content:"
  , existingContent
  -- ... more prompt lines
  ]
-}

-- AFTER (with LLMCreatable):
{-
createAndWriteSessionJournal sessionName processedText = do
  journal <- createFrom @SessionJournal processedText
  writePage (PageName ("Session " <> sessionName)) (contentText journal)
  return journal

updateEntityFromSession existingEntity sessionJournal = do
  updated <- updateWith existingEntity (contentText sessionJournal)
  return updated
-}

-- Benefits:
-- 1. Prompt logic is encapsulated in typeclass instances
-- 2. Same operations work for all LLMCreatable types
-- 3. Context management becomes systematic with referenceList
-- 4. Update operations are standardized
-- 5. Easy to add new LLMCreatable types

-- CONTEXT MANAGEMENT EXAMPLE:
-- Instead of manually building context strings, we can do:
{-
-- Get recent entities and sessions for context
recentEntities <- getRecentEntities 5
recentSessions <- getRecentSessions 3
let context = recentEntities ++ map shortName recentSessions

-- Create new entity with context
newEntity <- createFrom @EntityContent (contextualPrompt context sessionContent)
-}

-- CONTEXT MANAGEMENT DESIGN:

-- Simplified LLM effect with only context scoping:
data LLM (m :: Type -> Type) a where
    AskLLM :: Text -> LLM m Text                              -- Always uses current context
    WithLLMContext :: Text -> Sem (LLM : r) a -> LLM m a     -- Scope a context session

-- Usage patterns:
-- 1. Fresh context (one-off): 
--    result <- askLLM "What is 2+2?"
--
-- 2. Shared context session:
--    (journal, summary) <- withLLMContext worldKnowledge $ do
--        journal <- askLLM "Create session journal from: ..."
--        summary <- askLLM "Summarize this journal: ..."  -- Builds on previous
--        return (journal, summary)

-- LLMCreatable functions work seamlessly with both patterns:
createFrom :: forall a r. (LLMCreatable a, Members '[LLM] r) => Text -> Sem r a
createFrom input = do
    result <- askLLM (creationPrompt @a input)
    return (parseOrFail result)

updateWith :: forall a r. (LLMCreatable a, Members '[LLM] r) => a -> Text -> Sem r a  
updateWith existing input = do
    result <- askLLM (updatePrompt existing input)
    return (parseOrFail result)

createPrompt :: forall a r. (LLMCreatable a, Members '[LLM] r) => Text -> Sem r a
createPrompt userPrompt = do
    result <- askLLM (promptCreation @a userPrompt)
    return (parseOrFail result)

-- Example: dndwiki with context management
processTranscriptWithContext :: Members '[LLM, Wiki] r => RawTranscript -> SessionName -> Sem r ProcessResult
processTranscriptWithContext rawTranscript sessionName = do
    worldKnowledge <- loadWikiContext  -- Get existing world state
    
    withLLMContext ("You are maintaining a D&D wiki. World knowledge:\n" <> worldKnowledge) $ do
        -- All operations share the world knowledge context
        processedTranscript <- createFrom @ProcessedTranscript (contentText rawTranscript)
        journal <- createFrom @SessionJournal (contentText processedTranscript) 
        summary <- createFrom @SessionSummary (contentText journal)
        
        -- Entity updates also benefit from shared context
        entities <- extractEntitiesFromJournal journal
        updatedEntities <- mapM (\e -> updateWith e (contentText journal)) entities
        
        return $ ProcessResult (length updatedEntities) True summary

-- Alternative: No context (fresh for each operation)
processTranscriptFresh :: Members '[LLM, Wiki] r => RawTranscript -> SessionName -> Sem r ProcessResult  
processTranscriptFresh rawTranscript sessionName = do
    -- Each operation gets fresh context
    processedTranscript <- createFrom @ProcessedTranscript (contentText rawTranscript)
    journal <- createFrom @SessionJournal (contentText processedTranscript)
    summary <- createFrom @SessionSummary (contentText journal)
    return $ ProcessResult 0 False summary

-- Benefits of this simplified approach:
-- 1. No confusion about session state - askLLM always uses current context
-- 2. Context is explicit and scoped - clear boundaries  
-- 3. Same LLMCreatable functions work with/without context
-- 4. Developer chooses context granularity (per-operation vs per-workflow)
-- 5. Backward compatible - no context = fresh for each call

-- LLMINPUT/LLMOUTPUT - Clean input/output type conversion:

class LLMInput a where
    toLLMText :: a -> Text
    inputDescription :: Text  -- What this type represents for the LLM

class LLMOutput a where
    fromLLMText :: Text -> Maybe a  -- Parse LLM output, Nothing on failure

-- Basic instances:
instance LLMInput Text where
    toLLMText = id
    inputDescription = "text content"

instance LLMInput Text where  
    toLLMText = id
    inputDescription = "text content"

instance LLMOutput Text where
    fromLLMText = Just

instance LLMOutput Text where
    fromLLMText = Just

-- Rich data instances for dndwiki:
instance LLMInput [Event] where
    toLLMText events = T.unlines $ map formatEvent events
      where formatEvent (Event time desc) = time <> ": " <> desc
    inputDescription = "chronological list of events"

instance LLMInput [EntityName] where
    toLLMText entities = T.unlines $ map (\(EntityName n) -> "- " <> n) entities  
    inputDescription = "list of entity names"

instance LLMInput [WikiEntity] where
    toLLMText entities = T.unlines $ map formatEntity entities
      where formatEntity (WikiEntity name aliases content) = 
              shortName name <> " (aliases: " <> T.intercalate ", " (map shortName aliases) <> "): " 
              <> T.take 100 (contentText content) <> "..."
    inputDescription = "list of wiki entities with their current content"

-- LLMOutput instances for dndwiki newtypes:
instance LLMOutput SessionJournal where
    fromLLMText = Just . SessionJournal

instance LLMOutput SessionSummary where
    fromLLMText = Just . SessionSummary

instance LLMOutput EntityContent where
    fromLLMText = Just . EntityContent

-- Updated createFrom function:
createFrom :: forall a b r. (LLMCreatable a, LLMOutput a, LLMInput b, Members '[LLM] r) => b -> Sem r a
createFrom input = do
    let prompt = creationPrompt @a (toLLMText input) <> 
                 "\n\nInput contains: " <> inputDescription @b
    result <- askLLM prompt
    case fromLLMText result of
        Just parsed -> return parsed
        Nothing -> fail $ "Failed to parse LLM output for " <> T.unpack (description @a)

-- Beautiful usage examples:
-- summary <- createFrom @SessionSummary events           -- [Event] -> SessionSummary
-- journal <- createFrom @SessionJournal transcript       -- ProcessedTranscript -> SessionJournal  
-- entity <- createFrom @EntityContent existingEntities   -- [WikiEntity] -> EntityContent (with context)

-- This design gives us:
-- 1. Rich typed inputs with semantic meaning (LLMInput)
-- 2. Type-safe output parsing with fallback (LLMOutput) 
-- 3. Clear separation of concerns
-- 4. Composable input/output types
-- 5. Automatic error handling for unparseable output