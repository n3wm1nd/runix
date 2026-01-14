{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
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
--   main <- readFile @MainRepo \"config.yaml\"
--   dep <- readFile @DepRepo \"lib/code.hs\"
-- @
module Runix.FileSystem where

import Data.Kind (Type)
import Polysemy
import Polysemy.State (get, put, runState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath
import Data.String (fromString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Time (UTCTime)
import Control.Monad (forM)
import qualified Data.List
import GHC.Stack
import qualified Runix.FileSystem.System as System
import Runix.Logging (Logging, info)
import qualified System.Directory
import System.IO.Error (tryIOError)

-- | Core filesystem effect - provides project metadata and structure exploration
-- This allows seeing the directory tree without reading file contents
data FileSystem project (m :: Type -> Type) a where
    -- | Get the project value/configuration
    GetFileSystem :: FileSystem project m project
    -- | List files in a directory
    ListFiles :: FilePath -> FileSystem project m (Either String [FilePath])
    -- | Check if a file exists
    FileExists :: FilePath -> FileSystem project m (Either String Bool)
    -- | Check if path is a directory
    IsDirectory :: FilePath -> FileSystem project m (Either String Bool)
    -- | Glob for files matching a pattern
    Glob :: FilePath -> String -> FileSystem project m (Either String [FilePath])
    -- | Get current working directory
    GetCwd :: FileSystem project m (Either String FilePath)

-- | Get the project configuration/value
getFileSystem :: forall project r. Member (FileSystem project) r => Sem r project
getFileSystem = send @(FileSystem project) GetFileSystem

-- | Read file contents - separate from structure exploration
data FileSystemRead project (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystemRead project m (Either String ByteString)

-- | Write-only filesystem operations parameterized by project
data FileSystemWrite project (m :: Type -> Type) a where
    WriteFile :: FilePath -> ByteString -> FileSystemWrite project m (Either String ())
    CreateDirectory :: Bool -> FilePath -> FileSystemWrite project m (Either String ())
    Remove :: Bool -> FilePath -> FileSystemWrite project m (Either String ())

-- | Public API functions that convert Either to Fail

-- Filesystem structure operations
listFiles :: forall project r. Members [FileSystem project, Fail] r => FilePath -> Sem r [FilePath]
listFiles p = send @(FileSystem project) (ListFiles p) >>= either fail return

fileExists :: forall project r. Members [FileSystem project, Fail] r => FilePath -> Sem r Bool
fileExists p = send @(FileSystem project) (FileExists p) >>= either fail return

isDirectory :: forall project r. Members [FileSystem project, Fail] r => FilePath -> Sem r Bool
isDirectory p = send @(FileSystem project) (IsDirectory p) >>= either fail return

glob :: forall project r. Members [FileSystem project, Fail] r => FilePath -> String -> Sem r [FilePath]
glob base pat = send @(FileSystem project) (Glob base pat) >>= either fail return

getCwd :: forall project r. Members [FileSystem project, Fail] r => Sem r FilePath
getCwd = send @(FileSystem project) GetCwd >>= either fail return

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
-- Path Translation Utilities (for chroot and external tools)
--------------------------------------------------------------------------------

-- | Translate a chroot-relative path to a system path
-- Takes the virtual CWD (as seen inside chroot) and a user-provided path
-- Returns the actual system path that should be used for IO operations
translateToSystemPath :: HasProjectPath project
                      => project  -- ^ Project configuration
                      -> FilePath  -- ^ Virtual CWD inside chroot (e.g., "/subdir")
                      -> FilePath  -- ^ User-provided path (chroot-relative or absolute within chroot)
                      -> FilePath  -- ^ System path
translateToSystemPath proj virtualCwd userPath
  | isAbsolute userPath = root </> dropWhile (== '/') userPath  -- Absolute within chroot
  | otherwise = root </> virtualCwdRelative </> userPath  -- Relative to virtual CWD
  where
    root = getProjectPath proj
    virtualCwdRelative = dropWhile (== '/') virtualCwd

-- | Translate a system absolute path back to chroot-relative path
-- E.g., "/home/user/.local/share/runix-code/generated-tools/Foo.hs" -> "/generated-tools/Foo.hs"
translateFromSystemPath :: HasProjectPath project
                        => project  -- ^ Project configuration
                        -> FilePath  -- ^ System absolute path
                        -> FilePath  -- ^ Chroot-relative path
translateFromSystemPath proj systemPath =
  let root = getProjectPath proj
      relative = makeRelative root systemPath
  in if relative == "." then "/" else "/" </> relative

-- | Translate a system absolute path back, preserving relative/absolute format
-- If the input was relative, return relative to virtualCwd
-- If the input was absolute, return absolute within chroot
translateFromSystemPath' :: HasProjectPath project
                         => project    -- ^ Project configuration
                         -> FilePath   -- ^ Virtual CWD (chroot-relative)
                         -> Bool       -- ^ Whether input was absolute
                         -> FilePath   -- ^ System absolute path
                         -> FilePath   -- ^ Chroot path (relative or absolute)
translateFromSystemPath' proj virtualCwd inputWasAbsolute systemPath
  | inputWasAbsolute = translateFromSystemPath proj systemPath
  | otherwise =
      -- Input was relative, return relative to virtual CWD
      let chrootAbsolute = translateFromSystemPath proj systemPath
          -- Remove leading / to make it relative to root, then make relative to virtualCwd
          chrootRelativeToRoot = dropWhile (== '/') chrootAbsolute
          virtualCwdRelative = dropWhile (== '/') virtualCwd
      in makeRelative virtualCwdRelative chrootRelativeToRoot

--------------------------------------------------------------------------------
-- Local Filesystem Interpreter
--------------------------------------------------------------------------------

-- | Wrap System filesystem as a parameterized filesystem (no chroot)
-- Just passes through to System.* operations
fileSystemFromSystem :: forall project r a.
                        ( HasProjectPath project
                        , Members [System.FileSystemRead, System.FileSystemWrite] r
                        )
                     => project
                     -> Sem (FileSystemWrite project : FileSystemRead project : FileSystem project : r) a
                     -> Sem r a
fileSystemFromSystem project action =
  interpretFileSystem project
    . interpretFileSystemRead
    . interpretFileSystemWrite
    $ action
  where
    interpretFileSystem :: Member System.FileSystemRead r'
                        => project -> Sem (FileSystem project : r') a' -> Sem r' a'
    interpretFileSystem proj = interpret $ \case
      GetFileSystem -> return proj
      GetCwd -> fmap Right (send System.GetCwd)
      ListFiles p -> send (System.ListFiles p)
      FileExists p -> send (System.FileExists p)
      IsDirectory p -> send (System.IsDirectory p)
      Glob base pat -> send (System.Glob base pat)

    interpretFileSystemRead :: Member System.FileSystemRead r'
                            => Sem (FileSystemRead project : r') a' -> Sem r' a'
    interpretFileSystemRead = interpret $ \case
      ReadFile p -> send (System.ReadFile p)

    interpretFileSystemWrite :: Member System.FileSystemWrite r'
                             => Sem (FileSystemWrite project : r') a' -> Sem r' a'
    interpretFileSystemWrite = interpret $ \case
      WriteFile p d -> send (System.WriteFile p d)
      CreateDirectory createParents p -> send (System.CreateDirectory createParents p)
      Remove recursive p -> send (System.Remove recursive p)

-- | Chroot a filesystem to a subdirectory
-- Translates absolute paths and GetCwd to chroot coordinates
chrootFileSystem :: forall project r a.
                    ( HasProjectPath project
                    , Members [FileSystemRead project, FileSystemWrite project, FileSystem project, Fail] r
                    )
                 => Sem r a
                 -> Sem r a
chrootFileSystem action =
  interceptFileSystem . interceptFileSystemRead . interceptFileSystemWrite $ action
  where
    -- Helper to get virtual CWD from system CWD
    getVirtualCwd :: Members [FileSystem project, Fail] r' => Sem r' FilePath
    getVirtualCwd = do
      proj <- send (GetFileSystem @project)
      systemCwd <- getCwd @project
      let root = getProjectPath proj
          relative = makeRelative root systemCwd
      return $ if relative == "." then "/" else "/" </> relative

    interceptFileSystem :: Members [FileSystem project, Fail] r' => Sem r' a' -> Sem r' a'
    interceptFileSystem = intercept $ \case
      GetFileSystem -> send (GetFileSystem @project)
      GetCwd -> do
        virtualCwd <- getVirtualCwd
        return $ Right virtualCwd
      ListFiles p -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        let systemPath = translateToSystemPath proj virtualCwd p
            inputWasAbsolute = isAbsolute p
        result <- send (ListFiles @project systemPath)
        -- Translate results back, preserving relative/absolute format
        return $ fmap (map (translateFromSystemPath' proj virtualCwd inputWasAbsolute)) result
      FileExists p -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        send (FileExists @project (translateToSystemPath proj virtualCwd p))
      IsDirectory p -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        send (IsDirectory @project (translateToSystemPath proj virtualCwd p))
      Glob base pat -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        let systemPath = translateToSystemPath proj virtualCwd base
            inputWasAbsolute = isAbsolute base
        result <- send (Glob @project systemPath pat)
        -- Translate results back, preserving relative/absolute format
        return $ fmap (map (translateFromSystemPath' proj virtualCwd inputWasAbsolute)) result

    interceptFileSystemRead :: Members [FileSystemRead project, FileSystem project, Fail] r' => Sem r' a' -> Sem r' a'
    interceptFileSystemRead = intercept $ \case
      ReadFile p -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        send (ReadFile @project (translateToSystemPath proj virtualCwd p))

    interceptFileSystemWrite :: Members [FileSystemWrite project, FileSystem project, Fail] r' => Sem r' a' -> Sem r' a'
    interceptFileSystemWrite = intercept $ \case
      WriteFile p d -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        send (WriteFile @project (translateToSystemPath proj virtualCwd p) d)
      CreateDirectory createParents p -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        send (CreateDirectory @project createParents (translateToSystemPath proj virtualCwd p))
      Remove recursive p -> do
        proj <- send (GetFileSystem @project)
        virtualCwd <- getVirtualCwd
        send (Remove @project recursive (translateToSystemPath proj virtualCwd p))

-- | Interpret filesystem effects for a local directory (with chroot)
-- All paths are relative to the project root
fileSystemLocal :: forall project r a.
                   ( HasProjectPath project
                   , Members [System.FileSystemRead, System.FileSystemWrite, Fail] r
                   )
                 => project
                 -> Sem (FileSystemWrite project : FileSystemRead project : FileSystem project : r) a
                 -> Sem r a
fileSystemLocal project action =
  fileSystemFromSystem project (chrootFileSystem action)

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

-- | Apply a filter to filesystem structure operations
-- Resolves all paths to absolute using GetCwd before checking the filter
filterFileSystem :: forall project r a.
                    Member (FileSystem project) r
                 => PathFilter
                 -> Sem r a
                 -> Sem r a
filterFileSystem filter = intercept $ \case
  GetFileSystem -> send (GetFileSystem @project)
  GetCwd -> send (GetCwd @project)

  ListFiles p -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = resolveForCheck cwd p
        if shouldInclude filter resolved
          then do
            result <- send (ListFiles @project p)
            -- Also filter the results
            return $ fmap (Prelude.filter (\f ->
              let absF = resolveForCheck resolved f
              in shouldInclude filter absF)) result
          else return $ Left $ "Access denied: " ++ filterName filter

  FileExists p -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = resolveForCheck cwd p
        if shouldInclude filter resolved
          then send (FileExists @project p)
          else return $ Right False

  IsDirectory p -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = resolveForCheck cwd p
        if shouldInclude filter resolved
          then send (IsDirectory @project p)
          else return $ Right False

  Glob base pat -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolvedBase = resolveForCheck cwd base
        if shouldInclude filter resolvedBase
          then do
            result <- send (Glob @project base pat)
            -- Filter the results
            return $ fmap (Prelude.filter (\f ->
              let absF = resolveForCheck resolvedBase f
              in shouldInclude filter absF)) result
          else return $ Left $ "Access denied: " ++ filterName filter
  where
    resolveForCheck cwd p
      | isAbsolute p = System.basicResolvePath p
      | otherwise = System.basicResolvePath (cwd </> p)

-- | Apply a filter to filesystem read operations
-- Resolves all paths to absolute using GetCwd before checking the filter
filterRead :: forall project r a.
              ( Member (FileSystemRead project) r
              , Member (FileSystem project) r
              )
           => PathFilter
           -> Sem r a
           -> Sem r a
filterRead filter = intercept $ \case
  ReadFile p -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = if isAbsolute p
                      then System.basicResolvePath p
                      else System.basicResolvePath (cwd </> p)
        if shouldInclude filter resolved
          then send (ReadFile @project p)
          else return $ Left $ "Access denied: " ++ filterName filter

-- | Apply a filter to filesystem write operations
-- Resolves all paths to absolute using GetCwd before checking the filter
filterWrite :: forall project r a.
               ( Member (FileSystemWrite project) r
               , Member (FileSystem project) r
               )
            => PathFilter
            -> Sem r a
            -> Sem r a
filterWrite filter = intercept $ \case
  WriteFile p d -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = if isAbsolute p
                      then System.basicResolvePath p
                      else System.basicResolvePath (cwd </> p)
        if shouldInclude filter resolved
          then send (WriteFile @project p d)
          else return $ Left $ "Access denied: " ++ filterName filter

  CreateDirectory createParents p -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = if isAbsolute p
                      then System.basicResolvePath p
                      else System.basicResolvePath (cwd </> p)
        if shouldInclude filter resolved
          then send (CreateDirectory @project createParents p)
          else return $ Left $ "Access denied: " ++ filterName filter

  Remove recursive p -> do
    cwdResult <- send (GetCwd @project)
    case cwdResult of
      Left err -> return $ Left err
      Right cwd -> do
        let resolved = if isAbsolute p
                      then System.basicResolvePath p
                      else System.basicResolvePath (cwd </> p)
        if shouldInclude filter resolved
          then send (Remove @project recursive p)
          else return $ Left $ "Access denied: " ++ filterName filter

--------------------------------------------------------------------------------
-- Common Filters
--------------------------------------------------------------------------------

-- | Hide dotfiles (files/directories starting with '.')
hideDotfiles :: PathFilter
hideDotfiles = PathFilter
  { shouldInclude = not . isDotfile . takeFileName
  , filterName = "dotfiles are hidden"
  }
  where
    isDotfile ('.':_) = True
    isDotfile _ = False

-- | Hide .git directory
hideGit :: PathFilter
hideGit = PathFilter
  { shouldInclude = \p ->
      let components = splitPath p
      in not (any (\c -> c == ".git" || c == ".git/") components)
  , filterName = ".git directory is hidden"
  }

-- | Hide .claude directory
hideClaude :: PathFilter
hideClaude = PathFilter
  { shouldInclude = \p ->
      let components = splitPath p
      in not (any (\c -> c == ".claude" || c == ".claude/") components)
  , filterName = ".claude directory is hidden"
  }

-- | Filter to only allow paths within .claude directory or CLAUDE.md
onlyClaude :: PathFilter
onlyClaude = PathFilter
  { shouldInclude = \p ->
      let components = splitPath p
          fileName = takeFileName p
      in any (\c -> c == ".claude" || c == ".claude/") components || fileName == "CLAUDE.md"
  , filterName = "only .claude directory and CLAUDE.md are accessible"
  }

-- | Restrict access to a specific subpath (security filter)
-- This checks that resolved absolute paths fall within the allowed path
-- NOTE: The path passed to shouldInclude is already resolved to absolute by filterFileSystem
limitToSubpath :: FilePath  -- ^ Allowed base path (should be absolute)
               -> PathFilter
limitToSubpath allowedPath = PathFilter
  { shouldInclude = \p ->
      let normalizedAllowed = addTrailingPathSeparator $ System.basicResolvePath allowedPath
          -- Path is already resolved to absolute by filterFileSystem
          normalizedTarget = addTrailingPathSeparator $ System.basicResolvePath p
      in splitPath normalizedAllowed `isPrefixOf` splitPath normalizedTarget
  , filterName = "path is outside allowed directory " ++ allowedPath
  }
  where
    isPrefixOf = Data.List.isPrefixOf

--------------------------------------------------------------------------------
-- Logging Wrappers
--------------------------------------------------------------------------------

-- | Add logging to filesystem structure operations
loggingFileSystem :: forall project r a.
                     ( HasCallStack
                     , Member Logging r
                     , Member (FileSystem project) r
                     )
                  => String  -- ^ Log prefix
                  -> Sem r a
                  -> Sem r a
loggingFileSystem prefix = intercept $ \case
  GetFileSystem -> send (GetFileSystem @project)
  GetCwd -> send (GetCwd @project)
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

--------------------------------------------------------------------------------
-- FileWatcher Effect (Parameterized)
--------------------------------------------------------------------------------

-- | File watching effect for tracking changes to accessed files
-- Parameterized by project type, works with project-relative paths
data FileWatcher project (m :: Type -> Type) a where
    -- | Register a file path for change tracking (project-relative path)
    WatchFile :: FilePath -> FileWatcher project m ()

    -- | Get list of watched files that have changed since last check
    -- Returns [(FilePath, OldContent, NewContent)] for each changed file
    GetChangedFiles :: FileWatcher project m [(FilePath, ByteString, ByteString)]

    -- | Clear the list of watched files
    ClearWatched :: FileWatcher project m ()

    -- | Stop watching a specific file
    UnwatchFile :: FilePath -> FileWatcher project m ()

    -- | Get list of all currently watched files (for debugging)
    GetWatchedFiles :: FileWatcher project m [FilePath]

makeSem ''FileWatcher

-- | Intercept FileSystemRead operations to automatically watch accessed files
interceptFileAccessRead :: forall project r a.
                           Members '[FileSystemRead project, FileWatcher project] r
                        => Sem r a -> Sem r a
interceptFileAccessRead = intercept @(FileSystemRead project) $ \case
    ReadFile path -> do
        watchFile @project path
        send (ReadFile @project path)

-- | Intercept FileSystemWrite operations to automatically watch written files
-- Watch AFTER the write completes so we don't immediately detect our own write as a change
interceptFileAccessWrite :: forall project r a.
                            Members '[FileSystemWrite project, FileWatcher project] r
                         => Sem r a -> Sem r a
interceptFileAccessWrite = intercept @(FileSystemWrite project) $ \case
    WriteFile path content -> do
        result <- send (WriteFile @project path content)
        -- Watch after write completes to avoid triggering on our own write
        watchFile @project path
        return result
    CreateDirectory createParents path -> send (CreateDirectory @project createParents path)
    Remove recursive path -> send (Remove @project recursive path)

-- | State for the FileWatcher interpreter
-- | State for content-based file watching (no mtimes, just hashes)
data WatcherStateGeneric = WatcherStateGeneric
    { watchedFilesGeneric :: !(Map FilePath ByteString)  -- FilePath -> LastContentHash
    }

emptyWatcherStateGeneric :: WatcherStateGeneric
emptyWatcherStateGeneric = WatcherStateGeneric mempty

-- | State for mtime-based file watching
data WatcherState = WatcherState
    { watchedFiles :: !(Map FilePath (UTCTime, ByteString))  -- FilePath -> (ModTime, LastContent)
    }

emptyWatcherState :: WatcherState
emptyWatcherState = WatcherState mempty

-- | Generic interpreter for FileWatcher effect
-- Uses FileSystemRead to read files and compares content hashes
-- Works with any filesystem backend, not just System/IO
-- NOTE: This re-reads all watched files on each GetChangedFiles call
fileWatcherGeneric :: forall project r a.
                      Members '[FileSystemRead project, Fail] r
                   => Sem (FileWatcher project : r) a -> Sem r a
fileWatcherGeneric action = fmap snd $ runState emptyWatcherStateGeneric $ reinterpret (\case
    WatchFile path -> do
        -- Try to read the file content
        contentResult <- raise $ runFail $ readFile @project path
        case contentResult of
            Left _ -> return ()  -- Silently ignore files that can't be read
            Right content -> do
                WatcherStateGeneric watched <- get @WatcherStateGeneric
                put $ WatcherStateGeneric $ Map.insert path content watched

    GetChangedFiles -> do
        WatcherStateGeneric watched <- get @WatcherStateGeneric
        -- Re-read each watched file and compare content
        changedFiles <- fmap catMaybes $ forM (Map.toList watched) $ \(path, oldContent) -> do
            contentResult <- raise $ runFail $ readFile @project path
            case contentResult of
                Left _ -> return Nothing  -- File no longer exists or can't be accessed
                Right newContent
                    | newContent /= oldContent -> return $ Just (path, oldContent, newContent)
                    | otherwise -> return Nothing

        -- Update state with new content for changed files
        let updatedContent = Map.fromList [(path, newContent) | (path, _old, newContent) <- changedFiles]
        WatcherStateGeneric currentWatched <- get @WatcherStateGeneric
        let finalWatched = Map.union updatedContent currentWatched
        put $ WatcherStateGeneric finalWatched

        return changedFiles

    ClearWatched -> do
        put emptyWatcherStateGeneric

    UnwatchFile path -> do
        WatcherStateGeneric watched <- get @WatcherStateGeneric
        put $ WatcherStateGeneric $ Map.delete path watched

    GetWatchedFiles -> do
        WatcherStateGeneric watched <- get @WatcherStateGeneric
        return $ Map.keys watched
    ) action

-- | IO interpreter for FileWatcher effect
-- Tracks file modification times and content to detect changes
-- Uses internal State that persists across the entire wrapped computation
-- Converts project-relative paths to system paths for IO operations
fileWatcherIO :: forall project r a.
                 ( HasCallStack
                 , HasProjectPath project
                 , Members '[Embed IO, Logging, FileSystem project] r
                 )
              => Sem (FileWatcher project : r) a -> Sem r a
fileWatcherIO action = fmap snd $ runState emptyWatcherState $ reinterpret (\case
        WatchFile path -> do
            project <- raise $ getFileSystem @project
            let systemPath = projectToSystemPath project path
            -- Get current mtime and content
            mtimeResult <- embed $ tryIOError $ System.Directory.getModificationTime systemPath
            case mtimeResult of
                Left _ -> return ()  -- Silently ignore files that can't be watched
                Right mtime -> do
                    contentResult <- embed $ tryIOError $ BS.readFile systemPath
                    case contentResult of
                        Left _ -> return ()  -- Silently ignore files that can't be read
                        Right content -> do
                            WatcherState watched <- get @WatcherState
                            put $ WatcherState $ Map.insert path (mtime, content) watched

        GetChangedFiles -> do
            proj <- raise $ getFileSystem @project
            WatcherState watched <- get @WatcherState
            -- Check each watched file for changes
            changedFiles <- fmap catMaybes $ forM (Map.toList watched) $ \(path, (oldMtime, oldContent)) -> do
                let systemPath = projectToSystemPath proj path
                mtimeResult <- embed $ tryIOError $ System.Directory.getModificationTime systemPath
                case mtimeResult of
                    Left _ -> return Nothing  -- File no longer exists or can't be accessed
                    Right newMtime
                        | newMtime > oldMtime -> do
                            -- File was modified, re-read content
                            contentResult <- embed $ tryIOError $ BS.readFile systemPath
                            case contentResult of
                                Left _ -> return Nothing
                                Right newContent
                                    | newContent /= oldContent -> return $ Just (path, oldContent, newContent)
                                    | otherwise -> return Nothing
                        | otherwise -> return Nothing

            -- Update state with new mtimes and content for changed files
            updatedWithMtimes <- embed $ fmap Map.fromList $ forM changedFiles $ \(path, _oldContent, newContent) -> do
                let systemPath = projectToSystemPath proj path
                mtime <- System.Directory.getModificationTime systemPath
                return (path, (mtime, newContent))

            WatcherState currentWatched <- get @WatcherState
            let finalWatched = Map.union updatedWithMtimes currentWatched
            put $ WatcherState finalWatched

            return changedFiles

        ClearWatched -> do
            put emptyWatcherState
            raise $ info $ fromString "Cleared all watched files"

        UnwatchFile path -> do
            WatcherState watched <- get @WatcherState
            put $ WatcherState $ Map.delete path watched
            raise $ info $ fromString $ "Stopped watching file: " ++ path

        GetWatchedFiles -> do
            WatcherState watched <- get @WatcherState
            return $ Map.keys watched
    ) action

-- | No-op interpreter for FileWatcher (for CLI or other contexts where watching is not needed)
fileWatcherNoop :: Sem (FileWatcher project : r) a -> Sem r a
fileWatcherNoop = interpret $ \case
    WatchFile _ -> return ()
    GetChangedFiles -> return []
    ClearWatched -> return ()
    UnwatchFile _ -> return ()
    GetWatchedFiles -> return []
