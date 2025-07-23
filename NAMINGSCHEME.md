# Runix Naming Scheme

This document defines the naming conventions for Runix packages, modules, functions, and types to ensure predictable patterns for both human developers and LLM code generation.

## Core Principles

1. **Qualified imports preferred**: Use `CSV.parse` rather than `parseCSVData`
2. **Predictable patterns**: LLMs should be able to guess function names and locations
3. **Consistent structure**: All packages follow the same organizational patterns
4. **Domain-based organization**: Group related functionality by domain, not by operation type

## Module Hierarchy

### Core Effects (runix package)
```haskell
-- Core effects follow domain-based naming
Runix.Effects.FileSystem    -- File operations
Runix.Effects.RestAPI       -- HTTP requests  
Runix.Effects.Database      -- Database operations
Runix.Effects.Email         -- Email sending
Runix.Effects.Logging       -- Structured logging
Runix.Effects.Settings      -- Configuration access
Runix.Effects.Time          -- Time operations
Runix.Effects.Random        -- Random generation
Runix.Effects.Parallel      -- Parallel execution (future)
```

### Domain Packages (runix-* packages)
```haskell
-- Domain-specific packages with predictable structure
runix-csv/CSV.hs           -- CSV.parse, CSV.write, CSV.validate
runix-json/JSON.hs         -- JSON.decode, JSON.encode, JSON.merge
runix-xml/XML.hs           -- XML.parse, XML.transform, XML.validate
runix-email/Email.hs       -- Email.send, Email.sendBulk, Email.validate
runix-github/GitHub.hs     -- GitHub.createRepo, GitHub.pushCommit
runix-slack/Slack.hs       -- Slack.postMessage, Slack.uploadFile
runix-llm/LLM.hs           -- LLM.complete, LLM.summarize, LLM.classify
runix-image/Image.hs       -- Image.resize, Image.convert, Image.optimize
runix-pdf/PDF.hs           -- PDF.extract, PDF.merge, PDF.split
```

### User Tasks
```haskell
-- Simple tasks (recommended default)
MyTask.hs                   -- Contains types, functions, everything

-- Complex tasks (when needed)
MyTask.hs                   -- Main module, re-exports types
MyTask/
  ├── Types.hs             -- Parameter and result types
  ├── Logic.hs             -- Pure business logic
  └── Utils.hs             -- Helper functions

```

## Function Naming Conventions

### Effect Functions (Dual Interface Pattern)
```haskell
-- Simple interface (default name) - fails on error
readFile :: FilePath -> Sem r ByteString
writeFile :: FilePath -> ByteString -> Sem r ()
httpGet :: URL -> Sem r Response
sendEmail :: EmailAddress -> Subject -> Body -> Sem r ()

-- Try interface (try- prefix) - returns Maybe
tryReadFile :: FilePath -> Sem r (Maybe ByteString)
tryWriteFile :: FilePath -> ByteString -> Sem r (Maybe ())
tryHttpGet :: URL -> Sem r (Maybe Response)
trySendEmail :: EmailAddress -> Subject -> Body -> Sem r (Maybe ())
```

### Domain Package Functions
```haskell
-- Pattern: Module.verb or Module.verbNoun
-- Always use qualified imports: import qualified CSV

-- Data processing
CSV.parse :: ByteString -> Either CSV.ParseError CSV.Data
CSV.write :: CSV.Data -> ByteString
CSV.validate :: CSV.Data -> CSV.ValidationResult
CSV.transform :: (CSV.Row -> CSV.Row) -> CSV.Data -> CSV.Data
CSV.filter :: (CSV.Row -> Bool) -> CSV.Data -> CSV.Data
CSV.sort :: CSV.SortKey -> CSV.Data -> CSV.Data

-- API operations
GitHub.createRepo :: GitHub.RepoName -> GitHub.RepoConfig -> Sem r GitHub.Repository
GitHub.pushCommit :: GitHub.Repository -> GitHub.Commit -> Sem r ()
GitHub.listRepos :: GitHub.Organization -> Sem r [GitHub.Repository]
GitHub.deleteRepo :: GitHub.Repository -> Sem r ()
GitHub.createIssue :: GitHub.Repository -> GitHub.IssueData -> Sem r GitHub.Issue

-- Communication
Email.send :: Email.Address -> Email.Subject -> Email.Body -> Sem r ()
Email.sendBulk :: [Email.Address] -> Email.Subject -> Email.Body -> Sem r Email.BulkResult
Email.validate :: Email.Address -> Email.ValidationResult
Email.parseTemplate :: Email.Template -> Email.Data -> Email.Body

-- AI/LLM operations
LLM.complete :: LLM.Prompt -> Sem r LLM.Completion
LLM.summarize :: LLM.Text -> LLM.SummaryLength -> Sem r LLM.Summary
LLM.classify :: LLM.Text -> [LLM.Category] -> Sem r LLM.Classification
LLM.translate :: LLM.Text -> LLM.Language -> Sem r LLM.Translation
```

### Task Functions
```haskell
-- Pattern: descriptive verb phrase (unqualified in task modules)
processData :: ProcessParams -> Sem r ProcessResult
generateReport :: ReportParams -> Sem r ReportResult
analyzeText :: AnalysisParams -> Sem r AnalysisResult
syncFiles :: SyncParams -> Sem r SyncResult
```

## Data Type Naming Conventions

### Domain Types (Always Qualified)
```haskell
-- Pattern: Module.EntityName
CSV.Data, CSV.Row, CSV.Column, CSV.Header
JSON.Value, JSON.Object, JSON.Array, JSON.Schema
Email.Message, Email.Address, Email.Attachment, Email.Template
GitHub.Repository, GitHub.Commit, GitHub.Issue, GitHub.PullRequest
Slack.Message, Slack.Channel, Slack.User, Slack.Workspace
LLM.Prompt, LLM.Completion, LLM.Model, LLM.Token
Image.Format, Image.Dimensions, Image.Quality, Image.Metadata
```

### Parameter Types
```haskell
-- Pattern: Module.ActionParams (for domain packages)
CSV.ParseParams, CSV.WriteParams, CSV.ValidateParams
GitHub.CreateRepoParams, GitHub.PushCommitParams
Email.SendParams, Email.BulkSendParams
LLM.CompleteParams, LLM.SummarizeParams

-- Pattern: TaskNameParams (for user tasks)
DataProcessorParams, ReportGeneratorParams, TextAnalyzerParams
```

### Result Types
```haskell
-- Pattern: Module.ActionResult (for domain packages)
CSV.ParseResult, CSV.ValidationResult, CSV.TransformResult
GitHub.CreateRepoResult, GitHub.PushCommitResult
Email.SendResult, Email.BulkSendResult
LLM.CompletionResult, LLM.SummaryResult

-- Pattern: TaskNameResult (for user tasks)
DataProcessorResult, ReportGeneratorResult, TextAnalyzerResult
```

### Error Types
```haskell
-- Pattern: Module.ErrorType
CSV.ParseError, CSV.ValidationError, CSV.WriteError
JSON.DecodeError, JSON.SchemaError, JSON.ValidationError
Email.SendError, Email.ValidationError, Email.AttachmentError
GitHub.APIError, GitHub.AuthError, GitHub.RateLimitError
LLM.ModelError, LLM.TokenLimitError, LLM.APIError
```

### Configuration Types
```haskell
-- Pattern: Module.Config
GitHub.Config, Slack.Config, Email.Config, LLM.Config
CSV.ParseConfig, JSON.DecodeConfig, Image.ProcessConfig
```

## Import Conventions

### Standard Import Pattern
```haskell
-- Core effects (unqualified)
import Runix.Effects

-- Domain packages (always qualified)
import qualified CSV
import qualified JSON
import qualified Email
import qualified GitHub
import qualified Slack
import qualified LLM
import qualified Image
import qualified PDF

-- Task-specific imports (unqualified)
import TaskName.Types
import TaskName.Logic
```

### Predictable Usage Patterns
```haskell
-- LLMs can reliably predict these patterns
csvData <- CSV.parse content
jsonValue <- JSON.decode response
result <- Email.send recipient subject body
repo <- GitHub.createRepo "my-repo" GitHub.defaultConfig
response <- Slack.postMessage "#general" "Hello!"
summary <- LLM.summarize longText LLM.Medium
resized <- Image.resize (800, 600) originalImage
```

## Module Structure Conventions

### Domain Package Structure
```haskell
-- Every domain package follows this pattern
DomainName.hs              -- Main module with primary functions
DomainName/
  ├── Types.hs             -- Data types and instances
  ├── Internal.hs          -- Internal helper functions
  ├── Validation.hs        -- Validation functions (if needed)
  └── Examples.hs          -- Usage examples and documentation
```

### Task Package Structure
```haskell
-- Every task package follows this pattern
TaskName.hs                -- Main module with task functions
TaskName/
  ├── Types.hs             -- Parameter and result types
  ├── Logic.hs             -- Pure business logic
  └── Utils.hs             -- Helper functions
```

## Function Signature Patterns

### Task Functions
```haskell
-- Tasks always follow this pattern
taskName :: Members '[Effect1, Effect2] r => TaskParams -> Sem r TaskResult

-- Examples
processCSV :: Members '[FileSystem] r => ProcessCSVParams -> Sem r ProcessCSVResult
sendReport :: Members '[Email, FileSystem] r => SendReportParams -> Sem r SendReportResult
analyzeData :: Members '[LLM, Database] r => AnalyzeDataParams -> Sem r AnalyzeDataResult
```

### Domain Functions
```haskell
-- Pure functions (no effects)
Module.transform :: TransformParams -> TransformResult
Module.validate :: ValidationParams -> ValidationResult
Module.parse :: ParseParams -> Either Module.ParseError Module.Data

-- Effectful functions
Module.action :: Members '[RequiredEffects] r => ActionParams -> Sem r ActionResult
Module.tryAction :: Members '[RequiredEffects] r => ActionParams -> Sem r (Maybe ActionResult)
```

## Parameter and Result Patterns

### Standard Parameter Fields
```haskell
-- Input/output parameters
data ProcessParams = ProcessParams
  { inputFile :: FilePath      -- Always "inputFile" for file inputs
  , outputFile :: FilePath     -- Always "outputFile" for file outputs
  , options :: ProcessOptions  -- Always "options" for configuration
  } deriving (Show, Read, Generic, FromJSON, ToJSON)

-- API parameters  
data APIParams = APIParams
  { endpoint :: URL            -- Always "endpoint" for URLs
  , headers :: Headers         -- Always "headers" for HTTP headers
  , payload :: Payload         -- Always "payload" for request body
  } deriving (Show, Read, Generic, FromJSON, ToJSON)

-- Processing parameters
data TransformParams = TransformParams
  { sourceData :: SourceData   -- Always "sourceData" for input data
  , rules :: [TransformRule]   -- Always "rules" for transformation rules
  , config :: TransformConfig  -- Always "config" for configuration
  } deriving (Show, Read, Generic, FromJSON, ToJSON)
```

### Standard Result Fields
```haskell
-- Results always include metadata
data TaskResult = TaskResult
  { resultData :: ResultData       -- Always "resultData" for main result
  , resultMetadata :: Metadata     -- Always "resultMetadata" for stats
  , resultWarnings :: [Warning]    -- Always "resultWarnings" for issues
  } deriving (Show, Generic, ToJSON)

-- Processing results
data ProcessResult = ProcessResult
  { processedItems :: [ProcessedItem]  -- Always "processedItems" for processed data
  , processingStats :: ProcessingStats -- Always "processingStats" for statistics
  , outputFiles :: [FilePath]         -- Always "outputFiles" for generated files
  } deriving (Show, Generic, ToJSON)
```

## Documentation Conventions

### Haddock Comments
```haskell
-- | Parse CSV data with optional configuration
--
-- This function parses CSV data according to the provided configuration,
-- handling various CSV dialects and encoding issues.
--
-- Example:
-- @
-- result <- CSV.parse $ CSV.ParseParams
--   { CSV.parseInput = csvContent
--   , CSV.parseConfig = CSV.defaultConfig { CSV.delimiter = ';' }
--   }
-- @
CSV.parse :: CSV.ParseParams -> Either CSV.ParseError CSV.Data
```

### Module Documentation
```haskell
-- | CSV processing utilities
--
-- This module provides functions for parsing, writing, validating, and
-- transforming CSV data. All functions use qualified imports.
--
-- Typical usage:
--
-- @
-- import qualified CSV
-- 
-- csvData <- CSV.parse content
-- validated <- CSV.validate csvData schema
-- CSV.write outputPath validated
-- @
module CSV where
```

## Benefits for LLM Code Generation

1. **Predictable imports**: LLMs can guess `import qualified CSV` for CSV operations
2. **Consistent function names**: `CSV.parse`, `JSON.decode`, `Email.send` follow clear patterns
3. **Standard parameter types**: Always `Module.ActionParams` and `Module.ActionResult`
4. **Dual interfaces**: Always `action` and `tryAction` for effects
5. **Module hierarchy**: Clear where to find functionality
6. **Qualified naming**: Avoids naming conflicts and makes code more readable

## Examples

### LLM-Generated Task Example
```haskell
module DataProcessor where

import Runix.Effects
import qualified CSV
import qualified JSON
import qualified Email
import DataProcessor.Types

-- LLM can predict this structure reliably
processData :: Members '[FileSystem, Email] r => DataProcessorParams -> Sem r DataProcessorResult
processData DataProcessorParams{..} = do
  -- Read and parse CSV
  csvContent <- readFile inputFile
  csvData <- case CSV.parse csvContent of
    Left err -> fail $ "CSV parse error: " <> show err
    Right data -> return data
  
  -- Transform data
  let processedData = CSV.transform transformRules csvData
  
  -- Convert to JSON
  let jsonOutput = JSON.encode processedData
  
  -- Write output
  writeFile outputFile jsonOutput
  
  -- Send notification
  Email.send notificationEmail "Processing Complete" "Data processing finished successfully"
  
  return $ DataProcessorResult
    { resultData = processedData
    , resultMetadata = ProcessingMetadata (CSV.rowCount processedData)
    , resultWarnings = []
    }
```

This naming scheme ensures that LLMs can generate reliable, consistent code by following predictable patterns rather than memorizing arbitrary function names.
