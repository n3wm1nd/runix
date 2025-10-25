{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Dndwiki where

import Runix.LLM.Effects (LLM, askLLM)
import Runix.LLMTypes
import Polysemy
import Polysemy.Fail
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)
import Control.Monad (when)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Wiki

-- | Different text content types with newtype wrappers
-- Using strict Text for simplicity
newtype RawTranscript = RawTranscript Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype ProcessedTranscript = ProcessedTranscript Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype SessionJournal = SessionJournal Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype SessionSummary = SessionSummary Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype WorldSummary = WorldSummary Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype EntityContent = EntityContent Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype EntityName = EntityName Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)
newtype SessionName = SessionName Text deriving (Show, Generic, FromJSON, ToJSON, LLMText)


-- | HasLLMCodec instances for D&D types
instance HasLLMCodec RawTranscript where
  llmDescription = "raw D&D session transcript"
  llmGuidance = "Unfiltered session content including off-table discussions"
  llmFormat = textFormat "plain text"

instance HasLLMCodec ProcessedTranscript where
  llmDescription = "filtered D&D session transcript"
  llmGuidance = "Off-table content removed, keeping only roleplay and game mechanics"
  llmFormat = textFormat "plain text"

instance HasLLMCodec SessionJournal where
  llmDescription = "session journal with events and entity links"
  llmGuidance = "Chronological events with [[Entity Name]] format links and markdown formatting"
  llmFormat = textFormat "markdown"

instance HasLLMCodec SessionSummary where
  llmDescription = "session summary with key developments"
  llmGuidance = "Concise summary focusing on story developments, character moments, locations, combat, and unresolved plot threads"
  llmFormat = textFormat "markdown"

instance HasLLMCodec EntityContent where
  llmDescription = "entity wiki content"
  llmGuidance = "Detailed content for D&D entities (characters, locations, items, organizations) with appropriate tags and relationships"
  llmFormat = textFormat "markdown"

instance HasLLMCodec WorldSummary where
  llmDescription = "world summary with campaign state"
  llmGuidance = "Comprehensive summary covering campaign setting, locations, NPCs, factions, and ongoing plot threads"
  llmFormat = textFormat "markdown"

instance HasLLMCodec EntityName where
  llmDescription = "entity name"
  llmGuidance = "Simple entity name without formatting"
  llmFormat = textFormat "plain text"

instance HasLLMCodec SessionName where
  llmDescription = "session name"
  llmGuidance = "Session identifier like 'Session 12'"
  llmFormat = textFormat "plain text"


-- Existential types to keep functions polymorphic over provider and model
data DefaultProvider = forall provider. DefaultProvider provider
data DefaultModel = forall model. DefaultModel model

{- | Turn your D&D session recordings into an organized wiki

Takes a messy transcript from your recorded D&D session and automatically creates 
a clean, organized wiki for your campaign. Perfect for DMs who want to keep track 
of their world without spending hours writing notes after each game.

What this does for you:
• **Cleans up your transcript** - Removes all the "um, can you hear me?" and 
  off-topic chatter, keeping only the actual game content
• **Creates session notes** - Writes up what happened in each session with 
  timestamps and links to important people/places
• **Tracks your world** - Automatically creates and updates pages for every 
  character, location, and item that comes up in your game
• **Maintains continuity** - Keeps a master summary of your campaign world 
  that grows and changes with each session
• **Links everything together** - Characters mentioned in one session get 
  automatically linked to their character pages

Input: Raw audio transcript + session name
Output: Complete wiki pages ready to browse, with character sheets, location 
guides, session summaries, and an overall campaign guide

Great for DMs who record their sessions but don't want to spend time manually 
organizing notes, or players who want a searchable reference for "wait, who 
was that NPC again?"

@param rawTranscript The raw transcript content from the D&D session recording
@param sessionName Name/identifier for this session (e.g. "Session 12")
@return SessionSummary of the processed session
-}
ingestTranscript :: Members '[Wiki, LLM DefaultProvider DefaultModel, Fail] r =>
  RawTranscript -> SessionName -> Sem r SessionSummary
ingestTranscript rawTranscript sessionName = do
  -- Load world summary (used for both LLM context and session summary generation)
  worldSummary <- do
    exists <- pageExists (PageName "World Summary")
    if exists 
      then WorldSummary <$> readPage (PageName "World Summary")
      else return $ WorldSummary "No world summary found - this appears to be a new campaign."
  
  -- Get recent session summaries for additional context
  recentSessions <- do
    allPages <- listPages
    let sessionPages = filter (\(PageName name) -> T.isPrefixOf "Session " name) allPages
    let sortedSessions = take 3 $ reverse $ sort $ map (\(PageName name) -> name) sessionPages  -- Most recent first
    mapM (readPage . PageName) sortedSessions
  
  -- Initialize LLM session with world context
  let WorldSummary worldContent = worldSummary
      wikiContext = T.unlines $
        [ "=== WORLD SUMMARY ==="
        , worldContent
        , ""
        , "=== RECENT SESSIONS ==="
        ] ++ recentSessions
  _ <- askLLM @DefaultProvider @DefaultModel ("You are helping maintain a D&D campaign wiki. Here is the current world state:\n" <> wikiContext)
  
  -- Process transcript through the pipeline (each function handles its own I/O)
  processedTranscript <- filterTranscript rawTranscript
  sessionJournal <- createSessionJournal sessionName processedTranscript

  -- Generate session summary using world context (reuse worldSummary from above)
  sessionSummary <- generateSessionSummary worldSummary sessionJournal
  
  -- Write session summary to wiki
  let SessionSummary summaryContent = sessionSummary
      SessionName sName = sessionName
  writePage (PageName ("Session " <> sName <> " Summary")) summaryContent
  
  -- Update entities (reads existing entities, processes updates, writes back)
  _ <- updateEntitiesFromSession sessionJournal

  -- Update world summary (reads existing, updates, writes back)
  _ <- updateWorldSummary sessionSummary
  
  return sessionSummary

-- | Filter raw transcript to remove off-table chatter
filterTranscript :: Members '[Wiki, LLM DefaultProvider DefaultModel, Fail] r =>
  RawTranscript -> Sem r ProcessedTranscript
filterTranscript rawTranscript = createFrom @DefaultProvider @DefaultModel filterPrompt rawTranscript
  where
    filterPrompt = "Filter this D&D session transcript to remove off-table content. " <>
                   "REMOVE: technical issues, real-life conversations, ads, unrelated chatter. " <>
                   "KEEP: all roleplay content, in-character dialogue, game mechanics, DM narration. " <>
                   "Preserve timestamps and speaker identification where present."

-- | Create or update session journal (intelligently merges with existing content)
createSessionJournal :: Members '[Wiki, LLM DefaultProvider DefaultModel, Fail] r =>
  SessionName -> ProcessedTranscript -> Sem r SessionJournal
createSessionJournal (SessionName sessionName) processedTranscript = do
  let pageName = PageName ("Session " <> sessionName)
  exists <- pageExists pageName
  
  sessionJournal <- if exists
    then do
      -- Update existing journal - LLM will intelligently merge/dedupe content
      existingJournal <- SessionJournal <$> readPage pageName
      updateWith @DefaultProvider @DefaultModel updateJournalPrompt existingJournal processedTranscript
    else do
      -- Create new journal
      createFrom @DefaultProvider @DefaultModel createJournalPrompt processedTranscript
  
  -- Write updated journal back to wiki
  let SessionJournal journalContent = sessionJournal
  writePage pageName journalContent
  
  return sessionJournal
  where
    updateJournalPrompt = "Update this existing session journal with new transcript content. " <>
                         "If this is the same content, refine and improve the existing journal. " <>
                         "If this is new/additional content, merge it chronologically. " <>
                         "Remove any duplicate events and maintain chronological order. " <>
                         "Use [[Entity Name]] format for linking and maintain markdown formatting."
    
    createJournalPrompt = "Convert this D&D transcript into a session journal with chronological events with timestamps where available. " <>
                         "Link entities using [[Entity Name]] format for characters, locations, items. " <>
                         "Use markdown formatting for readability. " <>
                         "Include combat encounters, story developments, character interactions, and add brief context for important events."

-- | Generate session summary from world context and journal (pure transformation)
generateSessionSummary :: Members '[LLM DefaultProvider DefaultModel, Fail] r =>
  WorldSummary -> SessionJournal -> Sem r SessionSummary
generateSessionSummary worldSummary sessionJournal = do
  -- Combine world context and journal into a single input
  let WorldSummary worldContent = worldSummary
      SessionJournal journalContent = sessionJournal
      combinedInput = T.unlines
        [ "=== WORLD CONTEXT ==="
        , worldContent
        , ""
        , "=== SESSION JOURNAL ==="
        , journalContent
        ]

  askLLM @DefaultProvider @DefaultModel (summaryPrompt <> "\n\n" <> combinedInput) >>= return . SessionSummary
  where
    summaryPrompt =
      "Create a concise summary of this D&D session journal, considering the world context provided. " <>
      "Include: key story developments and plot progression, important character moments and decisions, " <>
      "new locations discovered or explored, combat encounters and their outcomes, items found or lost, " <>
      "unresolved plot threads and cliffhangers. " <>
      "Keep it brief but comprehensive, focusing on what matters for future sessions."


-- | Update entities (read existing, process, write updated)
updateEntitiesFromSession :: Members '[Wiki, LLM DefaultProvider DefaultModel, Fail] r =>
  SessionJournal -> Sem r ()
updateEntitiesFromSession sessionJournal = do
  -- First, extract entities from the journal using a simple prompt
  let SessionJournal journalContent = sessionJournal
  entityListText <- askLLM @DefaultProvider @DefaultModel (extractEntitiesPrompt <> "\n\n" <> journalContent)
  
  let entityNames = map (EntityName . T.strip) . filter (not . T.null) . T.lines $ entityListText
  
  -- Process each entity
  mapM_ processEntity entityNames
  where
    extractEntitiesPrompt = 
      "Extract all entities (characters, locations, items, organizations) from this session journal. " <>
      "Look for [[Entity Name]] links and other mentioned entities. " <>
      "Return one entity name per line, using their most natural/common name."
    
    updateEntityPrompt = "Update this entity wiki page with new information from the session journal. " <>
                         "Don't duplicate existing content. " <>
                         "Add relevant new information and use appropriate tags like #character #npc #location #item."
    
    createEntityPrompt = "Create detailed wiki content for a D&D entity based on this session journal. " <>
                         "Include appropriate tags (#character #npc #location #item #organization), " <>
                         "brief description, relationships, and any relevant details mentioned in the session."
    
    processEntity :: Members [Wiki, LLM DefaultProvider DefaultModel, Fail] r => EntityName -> Sem r ()
    processEntity (EntityName name)
      | T.null (T.strip name) = return ()
      | otherwise = do
          -- Check if entity page already exists
          let pageName = PageName name
          exists <- pageExists pageName
          if exists
            then do
              -- Update existing entity
              existingContent <- EntityContent <$> readPage pageName
              updatedEntity <- updateWith @DefaultProvider @DefaultModel updateEntityPrompt existingContent sessionJournal
              let EntityContent updatedContent = updatedEntity
              writePage pageName updatedContent
            else do
              -- Create new entity
              newEntity <- createFrom @DefaultProvider @DefaultModel createEntityPrompt sessionJournal
              let EntityContent newContent = newEntity
              writePage pageName newContent

-- | Update world summary (read existing, update, write back)
updateWorldSummary :: Members '[Wiki, LLM DefaultProvider DefaultModel, Fail] r =>
  SessionSummary -> Sem r ()
updateWorldSummary sessionSummary = do
  -- Check if world summary exists
  let worldSummaryPage = PageName "World Summary"
  exists <- pageExists worldSummaryPage
  
  if exists
    then do
      -- Update existing world summary
      existingSummary <- WorldSummary <$> readPage worldSummaryPage
      updatedSummary <- updateWith @DefaultProvider @DefaultModel updateWorldPrompt existingSummary sessionSummary
      
      -- Only write if content actually changed
      let WorldSummary updatedContent = updatedSummary
          WorldSummary existingContent = existingSummary
      when (T.strip updatedContent /= T.strip existingContent) $
        writePage worldSummaryPage updatedContent
    else do
      -- Create new world summary
      newSummary <- createFrom @DefaultProvider @DefaultModel createWorldPrompt sessionSummary
      let WorldSummary newContent = newSummary
      writePage worldSummaryPage newContent
  where
    updateWorldPrompt = "Update this world summary with information from the new session. " <>
                        "Integrate relevant information from this session summary. " <>
                        "Update ongoing plot threads, world state changes, and important developments. " <>
                        "Keep the summary comprehensive but concise."
    
    createWorldPrompt = "Create a world summary for this D&D campaign based on this session. " <>
                        "Include: campaign setting and world overview, key locations and their significance, " <>
                        "important NPCs and factions, major plot threads and ongoing conflicts, current state of the world."
