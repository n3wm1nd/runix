# Runix: Modular Task Execution

Runix is a secure, modular framework to run and combine included and community developed tasks. The tasks are written in short, type-safe, and simple Haskell code that combines the functionality of existing libraries, pure helper functions and other tasks.

The system uses builtin tasks, tasks from a curated package repositories, local and remote code to combine into a runnable application to execute those tasks with user provided inputs.

## Table of Contents
- [Quick Start](#quick-start)
- [Overview](#overview)
- [Architecture](#architecture)
  - [Trusted Repositories](#trusted-repositories)
  - [Task Structure](#task-structure)
  - [Universal Build System](#universal-build-system)
  - [Security Model](#security-model)
- [Task Development](#task-development)
- [Building Runix Systems](#building-runix-systems)
- [Package Development](#package-development)
- [Effect System](#effect-system)

## Quick Start

### Using Existing Tasks
```bash
# Build runix with community tasks
nix build github:runix/runix-core#cli
./result/bin/runix-cli --list-tasks

# Run a task
./result/bin/runix-cli echo --input "test"
```

### Creating Your Own Task

The easiest way to create a new task is using Runix's built-in template system:

```bash
# 1. Navigate to tasks directory
cd tasks

# 2. Create new task from template
nix flake new --template ../.#task my-automation

# 3. Initialize the template with proper names
nix run ../.#task-init my-automation

# 4. Start development
cd my-automation
nix develop
```

This creates a complete task structure with:
- Properly named `.cabal` file with correct dependencies
- Haskell module with basic task structure
- Development environment with all necessary tools

**Manual Creation (Alternative)**:
```bash
# 1. Create a new task manually
mkdir my-automation && cd my-automation

# 2. Create standard cabal file
cat > my-automation.cabal << EOF
name: my-automation
version: 1.0.0
synopsis: My personal automation task

library
  exposed-modules: MyAutomation
  build-depends: base, runix, polysemy
  hs-source-dirs: src
  default-language: Haskell2010
EOF

# 3. Create task implementation
mkdir src
cat > src/MyAutomation.hs << EOF
module MyAutomation where
import Runix.Effects
import Polysemy

automationTask :: Members '[FileSystem] r => FilePath -> Sem r Text
automationTask inputFile = do
  content <- readFile inputFile
  return $ "Processed " <> show (length (lines content)) <> " lines"
EOF

# 4. Set up development environment
nix develop

# 5. Lock dependencies and publish
nix flake lock
git init && git add . && git commit -m "Initial task"
```

### Building Custom Runix System
```nix
# flake.nix
{
  inputs = {
    runix-core.url = "github:runix/runix-core";
    runix-packages.url = "github:runix/runix-packages";
    my-task = { url = "github:user/my-automation"; flake = false; };
  };
  
  outputs = { runix-packages, my-task, ... }: {
    packages.x86_64-linux.default = runix-core.cli.build {
      taskSources = [ my-task ];
    };
  };
}
```

## Overview

Runix solves the problem of safely running automation tasks in a composable, reproducible way. The system addresses three key challenges:

### **Safety**
- Tasks run in restricted environments with controlled effects (Polysemy)
- User-provided tasks cannot execute arbitrary system code
- All builds go through trusted, audited build functions to enforce **Safe Haskell** extension use (no access to direct **IO**)

### **Reproducibility** 
- Nix ensures identical builds across different machines
- Dependency locking prevents "works on my machine" issues
- Published tasks include complete dependency specifications

### **Composability**
- Tasks are designed as monadic actions that can be easily combined
- Pure functionality can be exposed as helper functions
- Haskell's type system ensures interface compatibility
- Effect system provides controlled access to system resources

## Architecture

### Engine

The engine is responsible for executing tasks and providing safe, controlled access to system resources such as files and networks. It ensures that tasks run in a controlled environment with limited privileges through Polysemy effect reinterpretation.

**Security Model**: Effects are reinterpreted by the engine to provide restricted access:
- File access limited to specified directories
- Network requests restricted to whitelisted endpoints  
- Resource usage monitored and limited
- Safe Haskell prevents unsafe operations

### Effects

Effects provide a way to bring potentially non-pure actions (like file system or network operations) into a pure environment. Tasks use these effects through a standardized interface without knowing or caring about their specific implementation.

**Key Characteristics**:
- Separate the description of an effect from its interpretation
- Allow controlled access to system resources
- Enable composition and reinterpretation of effects

**Available Effects**:
- `FileSystem`: File read/write operations
- `RestAPI`: HTTP requests  
- `CompileTask`: Dynamic Haskell compilation
- *(More effects planned)*

Effects are implemented using the Polysemy library, which allows for flexible and secure effect management.

### Frontends

Frontends provide the executable interface for users and are compiled into the final executable (not pluggable at runtime). They handle:
- Task execution triggers
- Parameter provisioning through typeclass-based interfaces
- Result presentation
- Error handling and user feedback

Frontends automatically adapt to task parameter types:
- **CLI**: Uses `Show`/`Read` for command-line argument parsing
- **Web API**: Uses `FromJSON`/`ToJSON` for REST endpoints
- **GUI**: Uses `Generic` for automatic form generation

### Tasks

Tasks define the work to be performed and are packaged separately. They are written in **Safe Haskell** and expose functions of type `Members effects r => Params -> Sem r Result`.

Key characteristics:
- **Simplicity**: Tasks use a constrained subset of Haskell accessible to non-technical users
- **Type Safety**: Extensive use of types ensures data is properly typed and interfaces are clear
- **Composability**: Tasks are monads and can be naturally composed into pipelines
- **Security**: Safe Haskell and effect constraints prevent unsafe operations

#### Coding Style
Tasks code is expected to be clear, short, straightforward and easy to follow. Parameters are to be strongly typed. Types should represent what the data actually is, use of newtype wrappers, adts and records where applicable. 

They are to perform a single responsibility, calling other tasks to delegate work to them, and perform more complex data transformation in pure helper functions. The code should be focused on performing a single responsibility. 

Logging is possible, but kept at a minimum (the Effects and execution environment already provide inspection)

Errors are handled through monadic failure and explicit error handling and reporting should _not_ be part of the Task code. Error reporting and tracing is done at Effect level.

Overall: prioritize simplicity and clearity over completeness, robustness through early failure instead of error handling.

## Building Runix

### Build Process Overview

Runix uses Nix to manage dependencies and compilation. The build system:

1. **Combines** the engine, chosen frontend, and selected tasks into a unified executable
2. **Builds tasks** using Runix's secure build process (Safe Haskell enabled)
3. **Generates** task registration module (`Runix.Tasks`) with imports of all included tasks
4. **Resolves** and builds all dependencies through Nix

**Note**: Tasks are built using Runix's centralized build process, not their own build definitions, ensuring security constraints are enforced.

### Build
To build a custom version of a runner with your own set of tasks, modify the flake.nix file accordingly:

```nix
# In flake.nix
{
  taskSources = [
    "github:user/csv-processor"
    "github:user/email-automation"
    "./local-tasks/my-task"
  ];
  frontend = runix.cli
}
```

Then build: `nix build .`



### Task Registration

Tasks are automatically registered through a generated module system:

1. **Build process** scans selected task modules
2. **Generates** `Runix.Tasks` module with imports and exports:
```haskell
-- Auto-generated Runix/Tasks.hs
-- reexporting all
module Runix.Tasks (
  module CSVProcessor,
  module EmailSender
  ) where

import qualified CSVProcessor
import qualified EmailSender
```
3. **Frontends** discover tasks through module exports

## Task Development

### Task Structure
```
my-task/
├── my-task.cabal          # Standard Cabal file with metadata
├── src/
│   └── MyTask.hs         # Task implementation
└── README.md             # Usage documentation
```

### Minimal Task Example
```haskell
module MyTask where

import Runix.Effects
import Polysemy

data TaskFiles = TaskFiles
  { inputFile :: FilePath
  , outputFile :: FilePath  
  } deriving (Show, Read, Generic, FromJSON, ToJSON)

data TaskResult = TaskResult
  { linesProcessed :: Int
  , outputPath :: FilePath
  } deriving (Show, Generic, ToJSON)

-- Main task function
task :: Members '[FileSystem] r => TaskFiles -> Sem r TaskResult
task TaskFiles{..} = do
  content <- readFile inputFile
  let processed = processContent content
  writeFile outputFile processed
  return $ TaskResult (length $ lines processed) outputFile

-- Pure helper function
processContent :: String -> String
processContent = unlines . map processLine . lines
  where
    processLine line = "Processed: " ++ line
```

### Task Requirements
- **Dependencies**: Declare them in `.cabal` file using standard Cabal format. Additionally, they need to be specified in the Nix build specification.
- **Effects**: Specify required effects via `Members` constraint in type signature
- **Parameters**: Use types with appropriate typeclass instances (`Show`, `Read`, `Generic`, `FromJSON`, `ToJSON`)
- **Safe Haskell**: Required and enforced by build process
- **Tasks**: Export tasks functions and the required types to interface with them in the main module for discovery

### Development Workflow
1. **Create task** in `./tasks/my-task/` or separate repository
2. **Add to flake.nix**: Include task source in `taskSources` list
3. **Build and test**: `nix build .#cli`
4. **Run**: `./result/bin/runix-cli my-task --input-file data.txt --output-file result.txt`

## Effect System

### Effect Usage
```haskell
-- Declare required effects in type signature
myTask :: Members '[FileSystem, RestAPI] r => Params -> Sem r Result

-- Use effects naturally in do-notation
myTask params = do
  config <- readFile "config.json"
  response <- restPost endpoint (RestData config)
  writeFile "output.json" (show response)
  return $ Success "Task completed"
```

### Available Effects

| Effect | Simple Interface | Try Interface | Security |
|--------|------------------|---------------|----------|
| `FileSystem` | `readFile`, `writeFile` | `tryReadFile`, `tryWriteFile` | Limited to specified directories |
| `RestAPI` | `restPost`, `restGet` | `tryRestPost`, `tryRestGet` | Restricted to whitelisted endpoints |
| `CompileTask` | `compileTask`, `saveProject` | `tryCompileTask`, `trySaveProject` | Sandboxed compilation environment |

**Dual Interface Pattern**: Each effect provides two interfaces:
- **Simple interface**: Operations fail and terminate the task on error (e.g., `readFile`)
- **Try interface**: Operations return `Maybe` results for explicit error handling (e.g., `tryReadFile`)

### Security Model
Security is primarily handled at the compilation stage:

- **Safe Haskell** limits direct access to IO inside task code
- Polysemy allows controlled access to IO actions within tasks
- Pure functions and data types are inherently secure
- Effects can be reinterpreted to restrict or filter capabilities

Node: these limitations alone are not enough to securely execute completely untrusted or malicious code. They are mainly in place to enforce IO actions through our predefined Effects and discourage working around them. For a true secure execution runtime OS level sandboxing is required in addition to the Safe Haskell restrictions.

### Resource Management
- Tasks are stateless
- Persistent state managed through explicit file/database IO
- Resource constraints enforced at the system level

### Secrets Management
Accessible via the Secrets effect with controlled access.

## Frontend Integration

Frontends automatically adapt to task parameter types through Haskell's typeclass system:

### Required Typeclass Instances

| Frontend | Required Instances | Usage |
|----------|-------------------|-------|
| CLI | `Show`, `Read` | Command-line argument parsing |
| Web API | `FromJSON`, `ToJSON` | REST endpoint generation |
| GUI | `Generic` | Automatic form generation |

### Example Parameter Type
```haskell
data EmailParams = EmailParams
  { recipients :: [EmailAddress]
  , subject :: Text
  , templateFile :: FilePath
  } deriving (Show, Read, Generic, FromJSON, ToJSON)
```

Most instances can be automatically derived using `deriving` clauses.

### Error Handling
Tasks are designed to fail explicitly when encountering errors. Error handling should be done implicitly through task failure, keeping the code compact and readable.

Only handle errors when you can solve the problem transparently:

```haskell
-- Typical error handling: let the task fail if it cannot proceed
content <- readFile "config.json"
response <- restPost endpoint data
```

The focus is on writing clear, straightforward tasks that fail fast when unexpected conditions occur.

## Task Discovery

### Finding Tasks
- **GitHub repositories** with topic `runix-task`
- **Local development**: Create tasks in `./tasks/` directory
- **Community sharing**: Task repositories can be shared as Git URLs
- **Future**: Centralized registry/marketplace planned

### Task Metadata
All task metadata is stored in standard Cabal files:
- Task name and description
- Version information
- Dependencies
- Author and license information

### Task Composition
Tasks are naturally composable since they are monads:

```haskell
-- Sequential composition
pipeline :: Members '[FileSystem, RestAPI] r => InputParams -> Sem r FinalResult
pipeline params = do
  intermediate <- firstTask params
  secondTask intermediate
```

This enables building complex workflows from simple, reusable task components.
