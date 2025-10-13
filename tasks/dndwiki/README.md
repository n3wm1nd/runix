# DND Wiki Generator

**One-line summary:** Convert D&D session transcripts into organized Logseq wiki pages with automatic entity extraction and linking.

## Overview

This Runix task processes raw transcripts from D&D sessions and automatically generates a structured wiki for campaign management. It filters out off-table chatter, creates session journals with entity links, maintains character/location pages, and keeps an updated world summary.

## What it does

1. **Transcript Processing**: Filters raw audio transcripts to remove technical issues, off-topic conversation, and non-game content
2. **Session Documentation**: Creates timestamped session journals with automatic [[entity]] linking
3. **Entity Management**: Automatically creates and updates pages for characters, locations, items, and organizations
4. **World Tracking**: Maintains a campaign world summary that evolves with each session
5. **Logseq Integration**: Generates markdown files compatible with Logseq's linking and tagging system

## Input

- Raw transcript text (from audio transcription)
- Session name
- Existing wiki directory (for context)

## Output

- Session journal with chronological events and entity links
- Session summary highlighting key developments
- Individual entity pages (characters, locations, items, etc.) with appropriate tags
- Updated world summary incorporating new information
- Logseq-compatible file structure in `pages/` directory

## Effects Used

- `Wiki` - Custom effect for page-based knowledge management
- `LLM` - For content processing, filtering, and entity extraction
- `LLMTypes` - Type-safe LLM abstractions with `createFrom`/`updateWith` operations

## Example Usage

```bash
runix dndwiki --transcript-content "$(cat session-12-transcript.txt)" --session-name "Session 12"
```

## Features

- **Context Awareness**: Reads existing wiki content to maintain consistency and handle entity aliases
- **Smart Filtering**: Removes off-table chatter while preserving all roleplay content
- **Entity Recognition**: Handles character aliases, location variations, and cross-references
- **Incremental Updates**: Updates existing entity pages rather than overwriting them
- **Campaign Continuity**: Maintains world state across multiple sessions

## Technical Details

The task uses a pipeline approach:
1. Load existing wiki context for LLM priming
2. Filter transcript to remove irrelevant content
3. Generate annotated session journal with entity links
4. Extract and update entity information
5. Update campaign world summary
6. Write all content using Logseq-safe filenames

The `Wiki` effect abstracts away filesystem details and handles Logseq's specific filename requirements (special character sanitization, pages directory structure).

### LLM Type System

This task demonstrates Runix's type-safe LLM abstractions:

- **LLMInput types**: `RawTranscript`, `ProcessedTranscript`, `SessionJournal`, etc. can be fed to LLMs
- **LLMOutput types**: Define how to parse and prompt for specific content types
- **Type-safe operations**: `createFrom @SessionJournal transcript` generates journals with appropriate prompts
- **Automatic prompting**: Each type knows how to describe itself to the LLM for optimal results

Example transformation:
```haskell
-- Instead of manual prompting:
journalContent <- llmQuery "Convert this transcript..." processedText

-- Type-safe with automatic context:
sessionJournal <- createFrom @SessionJournal processedTranscript
```