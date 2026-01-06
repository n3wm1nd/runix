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

module Runix.FileSystem.Effects where
import Data.Kind (Type)
import Polysemy
import Polysemy.State (get, put, runState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Time (UTCTime)
import GHC.Stack
import Control.Monad (forM)
import qualified System.Directory
import qualified System.FilePath.Glob as Glob
import Runix.Logging.Effects (Logging, info)
import System.IO.Error (tryIOError)

-- | Read-only filesystem operations
data FileSystemRead (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystemRead m (Either String ByteString)
    ListFiles :: FilePath -> FileSystemRead m (Either String [FilePath])
    FileExists :: FilePath -> FileSystemRead m (Either String Bool)
    IsDirectory :: FilePath -> FileSystemRead m (Either String Bool)
    Glob :: FilePath -> String -> FileSystemRead m (Either String [FilePath])  -- base path and pattern
    GetCwd :: FileSystemRead m FilePath  -- Always succeeds

-- | Public API functions that convert Either to Fail
readFile :: Members [FileSystemRead, Fail] r => FilePath -> Sem r ByteString
readFile p = send (ReadFile p) >>= either fail return

listFiles :: Members [FileSystemRead, Fail] r => FilePath -> Sem r [FilePath]
listFiles p = send (ListFiles p) >>= either fail return

fileExists :: Members [FileSystemRead, Fail] r => FilePath -> Sem r Bool
fileExists p = send (FileExists p) >>= either fail return

isDirectory :: Members [FileSystemRead, Fail] r => FilePath -> Sem r Bool
isDirectory p = send (IsDirectory p) >>= either fail return

glob :: Members [FileSystemRead, Fail] r => FilePath -> String -> Sem r [FilePath]
glob base pat = send (Glob base pat) >>= either fail return

getCwd :: Member FileSystemRead r => Sem r FilePath
getCwd = send GetCwd

-- | Helper function to make a path absolute using the filesystem's CWD
makeAbsolute :: Members [FileSystemRead, Fail] r => FilePath -> Sem r FilePath
makeAbsolute path
    | isAbsolute path = return $ normalise path
    | otherwise = do
        cwd <- getCwd
        return $ normalise (cwd </> path)

-- | Write-only filesystem operations
data FileSystemWrite (m :: Type -> Type) a where
    WriteFile :: FilePath -> ByteString -> FileSystemWrite m (Either String ())

-- | Public API function that converts Either to Fail
writeFile :: Members [FileSystemWrite, Fail] r => FilePath -> ByteString -> Sem r ()
writeFile p d = send (WriteFile p d) >>= either fail return

-- | Combined filesystem effect for backwards compatibility
-- This is a type alias that combines both read and write operations
type FileSystem = '[FileSystemRead, FileSystemWrite]

data AccessPermission = AllowAccess | ForbidAccess String

-- | Basic resolution of .. and . in paths
-- This handles common cases but isn't perfect (doesn't handle symlinks, etc.)
basicResolvePath :: FilePath -> FilePath
basicResolvePath path =
  let normalized = normalise path
      parts = splitDirectories normalized
      resolved = foldl step [] parts
  in joinPath resolved
  where
    step :: [FilePath] -> FilePath -> [FilePath]
    step acc "." = acc                    -- Skip current directory
    step [] ".." = []                     -- Can't go above root
    step acc ".." = init acc              -- Go up one level (remove last component)
    step acc part = acc ++ [part]         -- Normal component

-- | Limited access control for read operations
limitedAccessRead :: (forall m x. FileSystemRead m x -> AccessPermission) -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
limitedAccessRead isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        ReadFile f -> send (ReadFile f)
        ListFiles f -> send (ListFiles f)
        FileExists f -> send (FileExists f)
        IsDirectory f -> send (IsDirectory f)
        Glob base pat -> send (Glob base pat)
        GetCwd -> send GetCwd
    ForbidAccess reason -> case action of
        ReadFile _ -> return (Left ("not allowed: " ++ reason))
        ListFiles _ -> return (Left ("not allowed: " ++ reason))
        FileExists _ -> return (Left ("not allowed: " ++ reason))
        IsDirectory _ -> return (Left ("not allowed: " ++ reason))
        Glob _ _ -> return (Left ("not allowed: " ++ reason))
        GetCwd -> error ("GetCwd should always be allowed, but got: " ++ reason)

-- | Limited access control for write operations
limitedAccessWrite :: (forall m x. FileSystemWrite m x -> AccessPermission) -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
limitedAccessWrite isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        WriteFile f c -> send (WriteFile f c)
    ForbidAccess reason -> case action of
        WriteFile _ _ -> return (Left ("not allowed: " ++ reason))

-- | Limit read operations to a subpath
-- Relative paths are resolved using the actual CWD, then checked against allowedPath
limitSubpathRead :: FilePath -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
limitSubpathRead allowedPath action = do
  -- Get the actual CWD from the underlying filesystem
  cwd <- send GetCwd

  -- Now intercept all operations and check them
  intercept (\case
    GetCwd -> send GetCwd  -- Don't modify GetCwd, pass it through
    ReadFile sp -> checkAndSend cwd sp (ReadFile sp)
    ListFiles sp -> checkAndSend cwd sp (ListFiles sp)
    FileExists sp -> checkAndSend cwd sp (FileExists sp)
    IsDirectory sp -> checkAndSend cwd sp (IsDirectory sp)
    Glob base pat -> checkAndSendGlob cwd base pat
    ) action
  where
    checkAndSend cwd targetPath act =
      case checkPath cwd targetPath of
        AllowAccess -> send act
        ForbidAccess reason -> return (Left ("not allowed: " ++ reason))

    checkAndSendGlob cwd base pat =
      case checkPath cwd base of
        AllowAccess -> do
          -- Execute the glob operation
          result <- send (Glob base pat)
          -- Resolve base to absolute path (glob results are relative to base)
          let absoluteBase = if isAbsolute base
                            then basicResolvePath base
                            else basicResolvePath (cwd </> base)
          -- Filter the results to only include files within allowedPath
          return $ fmap (filter (isGlobResultAllowed absoluteBase)) result
        ForbidAccess reason -> return (Left ("not allowed: " ++ reason))

    -- Check if a glob result (relative to base) is within the allowed path
    isGlobResultAllowed :: FilePath -> FilePath -> Bool
    isGlobResultAllowed absoluteBase relativePath =
      -- Glob returns paths relative to base, resolve to absolute
      let normalizedAllowed = addTrailingPathSeparator $ basicResolvePath allowedPath
          -- Resolve relative path against base to get absolute path
          absoluteTarget = basicResolvePath (absoluteBase </> relativePath)
          normalizedTarget = addTrailingPathSeparator absoluteTarget
      in splitPath normalizedAllowed `isPrefixOf` splitPath normalizedTarget

    checkPath :: FilePath -> FilePath -> AccessPermission
    checkPath cwd targetPath =
      let normalizedAllowed = addTrailingPathSeparator $ basicResolvePath allowedPath
          -- Resolve relative paths against actual CWD, then resolve .. and .
          absoluteTarget = if isAbsolute targetPath
                          then basicResolvePath targetPath
                          else basicResolvePath (cwd </> targetPath)
          -- Add trailing separator to target as well for consistent comparison
          normalizedTarget = addTrailingPathSeparator absoluteTarget
      in if splitPath normalizedAllowed `isPrefixOf` splitPath normalizedTarget
         then AllowAccess
         else ForbidAccess $ "not in explicitly allowed path " ++ allowedPath

-- | Limit write operations to a subpath
-- Requires FileSystemRead to be available to resolve relative paths against CWD
limitSubpathWrite :: Members '[FileSystemRead, FileSystemWrite] r => FilePath -> Sem r a -> Sem r a
limitSubpathWrite allowedPath action = do
  -- Get the actual CWD from the underlying filesystem
  cwd <- send GetCwd

  -- Now intercept all write operations and check them
  intercept @FileSystemWrite (\case
    WriteFile targetPath content ->
      case checkPath cwd targetPath of
        AllowAccess -> send (WriteFile targetPath content)
        ForbidAccess reason -> return (Left ("not allowed: " ++ reason))
    ) action
  where
    checkPath :: FilePath -> FilePath -> AccessPermission
    checkPath cwd targetPath =
      let normalizedAllowed = addTrailingPathSeparator $ basicResolvePath allowedPath
          -- Resolve relative paths against actual CWD, then resolve .. and .
          absoluteTarget = if isAbsolute targetPath
                          then basicResolvePath targetPath
                          else basicResolvePath (cwd </> targetPath)
          normalizedTarget = addTrailingPathSeparator absoluteTarget
      in if splitPath normalizedAllowed `isPrefixOf` splitPath normalizedTarget
         then AllowAccess
         else ForbidAccess $ "not in explicitly allowed path " ++ allowedPath

-- | Chroot for read operations
chrootSubpathRead :: FilePath -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
chrootSubpathRead chrootPath = intercept $ \case
    ReadFile f -> send (ReadFile (chrootPath </> f))
    ListFiles f -> send (ListFiles (chrootPath </> f)) >>= return . fmap (fmap (makeRelative chrootPath))
    FileExists f -> send (FileExists (chrootPath </> f))
    IsDirectory f -> send (IsDirectory (chrootPath </> f))
    Glob base pat -> send (Glob (chrootPath </> base) pat) >>= return . fmap (fmap (makeRelative chrootPath))
    GetCwd -> return chrootPath  -- Return chroot path as the "current" directory

-- | Chroot for write operations
chrootSubpathWrite :: FilePath -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
chrootSubpathWrite chrootPath = intercept $ \case
    WriteFile f c -> send (WriteFile (chrootPath </> f) c)

-- | Hide dotfiles for read operations
hideDotfilesRead :: Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
hideDotfilesRead = intercept $ \case
    ReadFile f | isDotfile f -> return (Left ("Access to dotfile denied: " ++ f))
                | otherwise -> send (ReadFile f)
    ListFiles f -> send (ListFiles f) >>= return . fmap (Prelude.filter (not . isDotfile . takeFileName))
    FileExists f | isDotfile f -> return (Right False)
                 | otherwise -> send (FileExists f)
    IsDirectory f | isDotfile f -> return (Right False)
                  | otherwise -> send (IsDirectory f)
    Glob base pat -> send (Glob base pat) >>= return . fmap (Prelude.filter (not . isDotfile . takeFileName))
    GetCwd -> send GetCwd
  where
    isDotfile :: FilePath -> Bool
    isDotfile path = case takeFileName path of
        ('.':_) -> True
        _ -> False

-- | Hide dotfiles for write operations
hideDotfilesWrite :: Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
hideDotfilesWrite = intercept $ \case
    WriteFile f c | isDotfile f -> return (Left ("Access to dotfile denied: " ++ f))
                  | otherwise -> send (WriteFile f c)
  where
    isDotfile :: FilePath -> Bool
    isDotfile path = case takeFileName path of
        ('.':_) -> True
        _ -> False

-- | IO interpreter for FileSystemRead effect
filesystemReadIO :: HasCallStack => Members [Embed IO, Logging] r => Sem (FileSystemRead : r) a -> Sem r a
filesystemReadIO = interpret $ \case
    ReadFile p -> do
        info $ fromString "reading file: " <> fromString p
        embed (tryIOError $ BS.readFile p) >>= return . either (Left . show) Right
    ListFiles p -> do
        info $ fromString "listing files: " <> fromString p
        embed (tryIOError $ System.Directory.listDirectory p) >>= return . either (Left . show) Right
    FileExists p -> do
        info $ fromString "checking file exists: " <> fromString p
        embed (tryIOError $ System.Directory.doesFileExist p) >>= return . either (Left . show) Right
    IsDirectory p -> do
        info $ fromString "checking is directory: " <> fromString p
        embed (tryIOError $ System.Directory.doesDirectoryExist p) >>= return . either (Left . show) Right
    Glob basePath pattern -> do
        info $ fromString "glob pattern: " <> fromString pattern <> fromString " in " <> fromString basePath
        embed (tryIOError $ globFiles basePath pattern) >>= return . either (Left . show) Right
    GetCwd -> do
        info $ fromString "getting current working directory"
        embed System.Directory.getCurrentDirectory
  where
    globFiles :: FilePath -> String -> IO [FilePath]
    globFiles base pat = do
        let compiledPattern = Glob.compile pat
        Glob.globDir1 compiledPattern base

-- | IO interpreter for FileSystemWrite effect
filesystemWriteIO :: HasCallStack => Members [Embed IO, Logging] r => Sem (FileSystemWrite : r) a -> Sem r a
filesystemWriteIO = interpret $ \case
    WriteFile p d -> do
        info $ fromString "writing file: " <> fromString p
        embed (tryIOError $ BS.writeFile p d) >>= return . either (Left . show) Right

-- | Combined IO interpreter for backwards compatibility
-- Interprets both FileSystemRead and FileSystemWrite
filesystemIO :: HasCallStack => Members [Embed IO, Logging] r => Sem (FileSystemRead : FileSystemWrite : r) a -> Sem r a
filesystemIO = filesystemWriteIO . filesystemReadIO

--------------------------------------------------------------------------------
-- FileWatcher Effect
--------------------------------------------------------------------------------

-- | File watching effect for tracking changes to accessed files
-- This effect allows tracking filesystem changes independently of read/write operations
-- Can be composed with FileSystemRead/Write via intercept to automatically watch accessed files
data FileWatcher (m :: Type -> Type) a where
    -- | Register a file path for change tracking
    WatchFile :: FilePath -> FileWatcher m ()

    -- | Get list of watched files that have changed since last check
    -- Returns [(FilePath, OldContent, NewContent)] for each changed file
    GetChangedFiles :: FileWatcher m [(FilePath, ByteString, ByteString)]

    -- | Clear the list of watched files
    ClearWatched :: FileWatcher m ()

    -- | Stop watching a specific file
    UnwatchFile :: FilePath -> FileWatcher m ()

    -- | Get list of all currently watched files (for debugging)
    GetWatchedFiles :: FileWatcher m [FilePath]

makeSem ''FileWatcher

-- | Intercept FileSystemRead operations to automatically watch accessed files
-- Usage: interceptFileAccess . runApp
-- Requires FileWatcher to be available in the effect stack
interceptFileAccessRead :: Members '[FileSystemRead, FileWatcher] r => Sem r a -> Sem r a
interceptFileAccessRead = intercept @FileSystemRead $ \case
    ReadFile path -> do
        watchFile path
        send (ReadFile path)
    ListFiles path -> send (ListFiles path)
    FileExists path -> send (FileExists path)
    IsDirectory path -> send (IsDirectory path)
    Glob base pat -> send (Glob base pat)
    GetCwd -> send GetCwd

-- | Intercept FileSystemWrite operations to automatically watch written files
-- Watch AFTER the write completes so we don't immediately detect our own write as a change
interceptFileAccessWrite :: Members '[FileSystemWrite, FileWatcher] r => Sem r a -> Sem r a
interceptFileAccessWrite = intercept @FileSystemWrite $ \case
    WriteFile path content -> do
        result <- send (WriteFile path content)
        -- Watch after write completes to avoid triggering on our own write
        watchFile path
        return result

-- | State for the FileWatcher interpreter
data WatcherState = WatcherState
    { watchedFiles :: !(Map FilePath (UTCTime, ByteString))  -- FilePath -> (ModTime, LastContent)
    }

emptyWatcherState :: WatcherState
emptyWatcherState = WatcherState mempty

-- | IO interpreter for FileWatcher effect
-- Tracks file modification times and content to detect changes
-- Uses internal State that persists across the entire wrapped computation
fileWatcherIO :: HasCallStack => Members '[Embed IO, Logging] r => Sem (FileWatcher : r) a -> Sem r a
fileWatcherIO action = fmap snd $ runState emptyWatcherState $ reinterpret (\case
        WatchFile path -> do
            -- Get current mtime and content
            -- If file is already watched, this resets the modification time (user requirement)
            mtimeResult <- embed $ tryIOError $ System.Directory.getModificationTime path
            case mtimeResult of
                Left _ -> return ()  -- Silently ignore files that can't be watched
                Right mtime -> do
                    contentResult <- embed $ tryIOError $ BS.readFile path
                    case contentResult of
                        Left _ -> return ()  -- Silently ignore files that can't be read
                        Right content -> do
                            WatcherState watched <- get @WatcherState
                            put $ WatcherState $ Map.insert path (mtime, content) watched

        GetChangedFiles -> do
            WatcherState watched <- get @WatcherState
            -- Check each watched file for changes
            changedFiles <- fmap catMaybes $ forM (Map.toList watched) $ \(path, (oldMtime, oldContent)) -> do
                mtimeResult <- embed $ tryIOError $ System.Directory.getModificationTime path
                case mtimeResult of
                    Left _ -> return Nothing  -- File no longer exists or can't be accessed
                    Right newMtime
                        | newMtime > oldMtime -> do
                            -- File was modified, re-read content
                            contentResult <- embed $ tryIOError $ BS.readFile path
                            case contentResult of
                                Left _ -> return Nothing
                                Right newContent
                                    | newContent /= oldContent -> return $ Just (path, oldContent, newContent)
                                    | otherwise -> return Nothing
                        | otherwise -> return Nothing

            -- Update state with new mtimes and content for changed files
            updatedWithMtimes <- embed $ fmap Map.fromList $ forM changedFiles $ \(path, _oldContent, newContent) -> do
                mtime <- System.Directory.getModificationTime path
                return (path, (mtime, newContent))

            WatcherState currentWatched <- get @WatcherState
            let finalWatched = Map.union updatedWithMtimes currentWatched
            put $ WatcherState finalWatched

            return changedFiles

        ClearWatched -> do
            put emptyWatcherState
            info $ fromString "Cleared all watched files"

        UnwatchFile path -> do
            WatcherState watched <- get @WatcherState
            put $ WatcherState $ Map.delete path watched
            info $ fromString $ "Stopped watching file: " ++ path

        GetWatchedFiles -> do
            WatcherState watched <- get @WatcherState
            return $ Map.keys watched
    ) action

-- | No-op interpreter for FileWatcher (for CLI or other contexts where watching is not needed)
fileWatcherNoop :: Sem (FileWatcher : r) a -> Sem r a
fileWatcherNoop = interpret $ \case
    WatchFile _ -> return ()
    GetChangedFiles -> return []
    ClearWatched -> return ()
    UnwatchFile _ -> return ()
    GetWatchedFiles -> return []
