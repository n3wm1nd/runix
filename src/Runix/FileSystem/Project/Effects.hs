{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Parameterized filesystem effects for multiple project support
--
-- This module provides filesystem effects parameterized by a project type,
-- allowing multiple independent filesystem views on the effect stack.
--
-- Example usage:
--
-- @
-- newtype MainRepo = MainRepo FilePath
-- newtype DepRepo = DepRepo FilePath
--
-- myFunc :: Members '[FileSystemRead MainRepo, FileSystemRead DepRepo] r
--        => Sem r ()
-- myFunc = do
--   main <- readFile @MainRepo "config.yaml"
--   dep <- readFile @DepRepo "lib/code.hs"
-- @
module Runix.FileSystem.Project.Effects where

import Data.Kind (Type)
import Polysemy
import Data.ByteString (ByteString)
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath
import Data.String (fromString)
import GHC.Stack
import qualified Runix.FileSystem.Effects as System
import Runix.Logging.Effects (Logging, info)

-- | Core project effect - provides project metadata and structure exploration
-- This allows seeing the directory tree without reading file contents
data Project project (m :: Type -> Type) a where
    -- | Get the project value/configuration
    GetProject :: Project project m project
    -- | List files in a directory
    ListFiles :: FilePath -> Project project m (Either String [FilePath])
    -- | Check if a file exists
    FileExists :: FilePath -> Project project m (Either String Bool)
    -- | Check if path is a directory
    IsDirectory :: FilePath -> Project project m (Either String Bool)
    -- | Glob for files matching a pattern
    Glob :: FilePath -> String -> Project project m (Either String [FilePath])

-- | Get the project configuration/value
getProject :: forall project r. Member (Project project) r => Sem r project
getProject = send @(Project project) GetProject

-- | Read file contents - separate from structure exploration
data FileSystemRead project (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystemRead project m (Either String ByteString)

-- | Write-only filesystem operations parameterized by project
data FileSystemWrite project (m :: Type -> Type) a where
    WriteFile :: FilePath -> ByteString -> FileSystemWrite project m (Either String ())
    CreateDirectory :: Bool -> FilePath -> FileSystemWrite project m (Either String ())
    Remove :: Bool -> FilePath -> FileSystemWrite project m (Either String ())

-- | Public API functions that convert Either to Fail

-- Project structure operations
listFiles :: forall project r. Members [Project project, Fail] r => FilePath -> Sem r [FilePath]
listFiles p = send @(Project project) (ListFiles p) >>= either fail return

fileExists :: forall project r. Members [Project project, Fail] r => FilePath -> Sem r Bool
fileExists p = send @(Project project) (FileExists p) >>= either fail return

isDirectory :: forall project r. Members [Project project, Fail] r => FilePath -> Sem r Bool
isDirectory p = send @(Project project) (IsDirectory p) >>= either fail return

glob :: forall project r. Members [Project project, Fail] r => FilePath -> String -> Sem r [FilePath]
glob base pat = send @(Project project) (Glob base pat) >>= either fail return

-- File content operations
readFile :: forall project r. Members [FileSystemRead project, Fail] r => FilePath -> Sem r ByteString
readFile p = send @(FileSystemRead project) (ReadFile p) >>= either fail return

writeFile :: forall project r. Members [FileSystemWrite project, Fail] r => FilePath -> ByteString -> Sem r ()
writeFile p d = send @(FileSystemWrite project) (WriteFile p d) >>= either fail return

createDirectory :: forall project r. Members [FileSystemWrite project, Fail] r => Bool -> FilePath -> Sem r ()
createDirectory createParents p = send @(FileSystemWrite project) (CreateDirectory createParents p) >>= either fail return

remove :: forall project r. Members [FileSystemWrite project, Fail] r => Bool -> FilePath -> Sem r ()
remove recursive p = send @(FileSystemWrite project) (Remove recursive p) >>= either fail return

--------------------------------------------------------------------------------
-- Project Path Extraction
--------------------------------------------------------------------------------

-- | Typeclass for extracting filesystem path from project types
class HasProjectPath project where
  -- | Get the root path of the project
  getProjectPath :: project -> FilePath

  -- | Convert a project-relative path to a system path (for external tools)
  -- Default implementation: just append to project root
  projectToSystemPath :: project -> FilePath -> FilePath
  projectToSystemPath proj relPath = getProjectPath proj </> relPath

-- | FilePath is its own project path
instance HasProjectPath FilePath where
  getProjectPath = id

--------------------------------------------------------------------------------
-- Local Filesystem Interpreter
--------------------------------------------------------------------------------

-- | Interpret project effects for a local directory
-- All paths are relative to the project root
projectFileSystemLocal :: forall project r a.
                         ( HasProjectPath project
                         , Members [System.FileSystemRead, System.FileSystemWrite] r
                         )
                       => project
                       -> Sem (FileSystemWrite project : FileSystemRead project : Project project : r) a
                       -> Sem r a
projectFileSystemLocal project action =
  let rootPath = getProjectPath project
  in interpretProject project
     . interpretProjectRead rootPath
     . interpretProjectWrite rootPath
     $ action
  where
    interpretProject :: Member System.FileSystemRead r'
                     => project -> Sem (Project project : r') a' -> Sem r' a'
    interpretProject proj = interpret $ \case
      GetProject -> return proj
      ListFiles p -> send (System.ListFiles (getProjectPath proj </> p))
      FileExists p -> send (System.FileExists (getProjectPath proj </> p))
      IsDirectory p -> send (System.IsDirectory (getProjectPath proj </> p))
      Glob base pat -> send (System.Glob (getProjectPath proj </> base) pat)

    interpretProjectRead :: Member System.FileSystemRead r'
                         => FilePath -> Sem (FileSystemRead project : r') a' -> Sem r' a'
    interpretProjectRead root = interpret $ \case
      ReadFile p -> send (System.ReadFile (root </> p))

    interpretProjectWrite :: Member System.FileSystemWrite r'
                          => FilePath -> Sem (FileSystemWrite project : r') a' -> Sem r' a'
    interpretProjectWrite root = interpret $ \case
      WriteFile p d -> send (System.WriteFile (root </> p) d)
      CreateDirectory createParents p -> send (System.CreateDirectory createParents (root </> p))
      Remove recursive p -> send (System.Remove recursive (root </> p))

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | A path filter predicate
data PathFilter = PathFilter
  { shouldInclude :: FilePath -> Bool
  , filterName :: String  -- for error messages
  }

instance Semigroup PathFilter where
  f1 <> f2 = PathFilter
    { shouldInclude = \p -> shouldInclude f1 p && shouldInclude f2 p
    , filterName = filterName f1 <> " + " <> filterName f2
    }

instance Monoid PathFilter where
  mempty = PathFilter (const True) "no filter"

-- | Apply a filter to project structure operations
filterProject :: forall project r a.
                 Member (Project project) r
              => PathFilter
              -> Sem r a
              -> Sem r a
filterProject filter = intercept $ \case
  GetProject -> send (GetProject @project)

  ListFiles p -> do
    result <- send (ListFiles @project p)
    return $ fmap (Prelude.filter (shouldInclude filter)) result

  FileExists p | not (shouldInclude filter p) ->
    return $ Right False
  FileExists p -> send (FileExists @project p)

  IsDirectory p | not (shouldInclude filter p) ->
    return $ Right False
  IsDirectory p -> send (IsDirectory @project p)

  Glob base pat -> do
    result <- send (Glob @project base pat)
    return $ fmap (Prelude.filter (shouldInclude filter)) result

-- | Apply a filter to filesystem read operations
filterRead :: forall project r a.
              Member (FileSystemRead project) r
           => PathFilter
           -> Sem r a
           -> Sem r a
filterRead filter = intercept $ \case
  ReadFile p | not (shouldInclude filter p) ->
    return $ Left $ "filtered out by " <> filterName filter
  ReadFile p -> send (ReadFile @project p)

-- | Apply a filter to filesystem write operations
filterWrite :: forall project r a.
               Member (FileSystemWrite project) r
            => PathFilter
            -> Sem r a
            -> Sem r a
filterWrite filter = intercept $ \case
  WriteFile p d | not (shouldInclude filter p) ->
    return $ Left $ "filtered out by " <> filterName filter
  WriteFile p d -> send (WriteFile @project p d)

  CreateDirectory createParents p | not (shouldInclude filter p) ->
    return $ Left $ "filtered out by " <> filterName filter
  CreateDirectory createParents p -> send (CreateDirectory @project createParents p)

  Remove recursive p | not (shouldInclude filter p) ->
    return $ Left $ "filtered out by " <> filterName filter
  Remove recursive p -> send (Remove @project recursive p)

--------------------------------------------------------------------------------
-- Common Filters
--------------------------------------------------------------------------------

-- | Hide dotfiles (files/directories starting with '.')
hideDotfiles :: PathFilter
hideDotfiles = PathFilter
  { shouldInclude = not . isDotfile . takeFileName
  , filterName = "hide dotfiles"
  }
  where
    isDotfile ('.':_) = True
    isDotfile _ = False

-- | Hide .git directory
hideGit :: PathFilter
hideGit = PathFilter
  { shouldInclude = \p -> ".git" `notElem` splitPath p
  , filterName = "hide .git"
  }

-- | Hide .claude directory
hideClaude :: PathFilter
hideClaude = PathFilter
  { shouldInclude = \p -> ".claude" `notElem` splitPath p
  , filterName = "hide .claude"
  }

-- | Filter to only allow paths within .claude directory or CLAUDE.md
onlyClaude :: PathFilter
onlyClaude = PathFilter
  { shouldInclude = \p ->
      let components = splitPath p
      in any (\c -> c == ".claude" || c == ".claude/") components || p == "CLAUDE.md"
  , filterName = "only .claude"
  }

--------------------------------------------------------------------------------
-- Logging Wrappers
--------------------------------------------------------------------------------

-- | Add logging to project structure operations
loggingProject :: forall project r a.
                  ( HasCallStack
                  , Member Logging r
                  , Member (Project project) r
                  )
               => String  -- ^ Log prefix
               -> Sem r a
               -> Sem r a
loggingProject prefix = intercept $ \case
  GetProject -> send (GetProject @project)
  ListFiles p -> do
    info $ fromString $ prefix <> "listing files: " <> p
    send (ListFiles @project p)
  FileExists p -> do
    info $ fromString $ prefix <> "checking file exists: " <> p
    send (FileExists @project p)
  IsDirectory p -> do
    info $ fromString $ prefix <> "checking is directory: " <> p
    send (IsDirectory @project p)
  Glob base pat -> do
    info $ fromString $ prefix <> "glob pattern: " <> pat <> " in " <> base
    send (Glob @project base pat)

-- | Add logging to filesystem read operations
loggingRead :: forall project r a.
               ( HasCallStack
               , Member Logging r
               , Member (FileSystemRead project) r
               )
            => String  -- ^ Log prefix
            -> Sem r a
            -> Sem r a
loggingRead prefix = intercept $ \case
  ReadFile p -> do
    info $ fromString $ prefix <> "reading file: " <> p
    send (ReadFile @project p)

-- | Add logging to filesystem write operations
loggingWrite :: forall project r a.
                ( HasCallStack
                , Member Logging r
                , Member (FileSystemWrite project) r
                )
             => String  -- ^ Log prefix
             -> Sem r a
             -> Sem r a
loggingWrite prefix = intercept $ \case
  WriteFile p d -> do
    info $ fromString $ prefix <> "writing file: " <> p
    send (WriteFile @project p d)
  CreateDirectory createParents p -> do
    info $ fromString $ prefix <> "creating directory: " <> p
    send (CreateDirectory @project createParents p)
  Remove recursive p -> do
    info $ fromString $ prefix <> "removing: " <> p
    send (Remove @project recursive p)
