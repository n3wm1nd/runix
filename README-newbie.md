# Runix for Beginners

Runix lets you run automated tasks safely on your computer. Think of it like an app store for automation - you can download and run tasks that others have created, or make simple ones yourself. Everything runs in a secure sandbox so tasks can't harm your system.

## What is Runix?

Imagine you have repetitive tasks like:
- Converting CSV files to different formats  
- Sending automated emails
- Processing images
- Analyzing data files

Instead of doing these manually or writing complex scripts, Runix lets you:
- **Use existing tasks** from the community
- **Create simple tasks** using a beginner-friendly approach
- **Combine tasks** to build more complex workflows

## Why is it Safe?

- **Sandboxed**: Tasks can only access files and websites you explicitly allow
- **Trusted Sources**: Tasks come from curated repositories that are reviewed for safety
- **No Installation Mess**: Everything is contained and won't break your system
- **Reproducible**: Works the same way on any computer

## Getting Started

### Step 1: Install Nix (One-time Setup)

Nix is a package manager that makes sure everything works reliably. Install it once:

**On Linux/Mac:**
```bash
curl -L https://nixos.org/nix/install | sh
```

**On Windows:**
Use WSL (Windows Subsystem for Linux) first, then install Nix inside WSL.

### Step 2: Try Runix with Existing Tasks

```bash
# Get runix with community tasks
nix build github:runix-core/runix-packages#cli-with-tasks

# See what tasks are available
./result/bin/runix-cli --list-tasks

# Run a task (example)
./result/bin/runix-cli csv-analyzer --input data.csv --output report.html
```

### Step 3: Your First Custom Task

```bash
# Create your task
mkdir my-first-task && cd my-first-task
```

## Using Existing Tasks

### Finding Tasks

Tasks come from trusted repositories that are reviewed for safety:

- **Official Packages**: `github:runix-core/runix-packages` - Core tasks maintained by the team
- **Community Packages**: Community-contributed tasks in curated repositories
- **GitHub Repositories**: Individual tasks with the topic `runix-task`

### Building Your Own Runix System

Create your own `flake.nix` file to include the tasks you want:

```nix
# flake.nix
{
  inputs = {
    runix-packages.url = "github:runix-core/runix-packages";
    
    # Add individual tasks (optional)
    my-favorite-task = { 
      url = "github:user/csv-processor"; 
      flake = false; # We only want the source, not their build
    };
  };
  
  outputs = { runix-packages, my-favorite-task, ... }: {
    packages.x86_64-linux.default = runix-packages.lib.buildRunixSystem {
      taskSources = [ my-favorite-task ];
      frontend = "cli";
    };
  };
}
```

Then build: `nix build .#default`

### Running Tasks

Tasks need parameters (inputs). Each task tells you what it needs:

```bash
# Example: Process a CSV file
./result/bin/runix-cli csv-processor \
  --input-file data.csv \
  --output-file processed.csv

# Example: Send an email  
./result/bin/runix-cli email-sender \
  --recipient "someone@example.com" \
  --subject "Hello" \
  --message "This is automated"
```

## Creating Simple Tasks

Don't worry - you don't need to be a programmer! Tasks use a simplified version of a programming language called Haskell.

### Your First Task

1. **Create a folder** for your task:
```bash
mkdir my-first-task
cd my-first-task
```

2. **Create a `my-first-task.cabal` file**:
```cabal
cabal-version: 2.4
name: my-first-task
version: 0.1.0.0
synopsis: My first Runix task

library
  exposed-modules: MyFirstTask
  build-depends: base, runix, polysemy
  hs-source-dirs: src
  default-language: Haskell2010
```

3. **Create `src/MyFirstTask.hs`**:
```haskell
module MyFirstTask where

import Runix.Effects
import Polysemy

-- What inputs does your task need?
data MyInputs = MyInputs
  { inputFile :: FilePath    -- A file path
  , message :: String        -- Some text
  } deriving (Show, Read)

-- What does your task output?
data MyResult = MyResult
  { outputMessage :: String
  } deriving (Show)

-- Your task
task :: Members '[FileSystem] r => MyInputs -> Sem r MyResult
task MyInputs{..} = do
  -- Read a file
  content <- readFile inputFile
  
  -- Do something with it
  let result = message ++ " - File had " ++ show (length (lines content)) ++ " lines"
  
  -- Return the result
  return $ MyResult result
```

4. **Add it to your Runix**:
   - Edit `flake.nix` and add `"./my-first-task"` to `taskSources`
   - Run `nix build .#cli`

5. **Test it**:
```bash
./result/bin/runix-cli my-first-task \
  --input-file README.md \
  --message "Hello"
```

## Common Task Patterns

### Reading and Writing Files
```haskell
-- Read a file
content <- readFile "input.txt"

-- Write a file
writeFile "output.txt" "Hello, world!"
```

### Processing Text
```haskell
-- Count lines
let lineCount = length (lines content)

-- Convert to uppercase
let uppercase = map toUpper content

-- Split into words
let words = words content
```

### Working with Lists
```haskell
-- Process each item in a list
let results = map processItem myList

-- Filter items
let filtered = filter isGood myList

-- Combine results
let combined = unlines results
```

## What Can Tasks Do?

Tasks can:
- ✅ Read and write files (in allowed directories)
- ✅ Make web requests (to allowed websites)
- ✅ Process text and data
- ✅ Perform calculations

Tasks cannot:
- ❌ Access your entire computer
- ❌ Install software
- ❌ Access arbitrary websites
- ❌ Run dangerous commands

## Getting Help

### Common Issues

**"Command not found"**
- Make sure you ran `nix build .#cli` first
- Use `./result/bin/runix-cli` (with the `./`)

**"Build failed"**
- Check your `.cabal` file syntax
- Make sure your Haskell code has correct indentation
- Look for typos in module names

**"Task not found"**
- Check that you added the task to `taskSources` in `flake.nix`
- Rebuild with `nix build .#cli`

### Learning More

- **Haskell basics**: [Learn You a Haskell](http://learnyouahaskell.com/) (focus on basic syntax)
- **Nix basics**: [Nix Pills](https://nixos.org/guides/nix-pills/) (just the first few chapters)
- **Ask for help**: Create an issue on the Runix GitHub repository

## Examples to Try

### File Counter
Count lines, words, and characters in a file:

```haskell
data CountResult = CountResult
  { lines :: Int
  , words :: Int  
  , chars :: Int
  } deriving (Show)

countTask :: Members '[FileSystem] r => FilePath -> Sem r CountResult
countTask filePath = do
  content <- readFile filePath
  return $ CountResult 
    (length $ lines content)
    (length $ words content)
    (length content)
```

### Text Replacer
Replace all occurrences of one word with another:

```haskell
data ReplaceParams = ReplaceParams
  { inputFile :: FilePath
  , outputFile :: FilePath
  , findText :: String
  , replaceText :: String
  } deriving (Show, Read)

replaceTask :: Members '[FileSystem] r => ReplaceParams -> Sem r String
replaceTask ReplaceParams{..} = do
  content <- readFile inputFile
  let newContent = replace findText replaceText content
  writeFile outputFile newContent
  return "Replacement complete"

-- Helper function
replace :: String -> String -> String -> String
replace find repl = unlines . map (replaceInLine find repl) . lines
  where
    replaceInLine f r line = -- implementation details...
```

Remember: Start simple, and gradually add more features as you get comfortable!
