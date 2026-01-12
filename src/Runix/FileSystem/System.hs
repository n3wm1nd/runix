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

-- | Low-level system filesystem effects
--
-- This module provides direct access to the system filesystem without
-- access control, filtering, or sandboxing. Use this when you need
-- unrestricted filesystem access. For scoped or filtered access, use
-- the parameterized effects in Runix.FileSystem.
module Runix.FileSystem.System
  ( -- * Effects
    FileSystemRead(..)
  , FileSystemWrite(..)
    -- * API functions
  , readFile
  , listFiles
  , fileExists
  , isDirectory
  , glob
  , getCwd
  , makeAbsolute
  , writeFile
  , createDirectory
  , remove
    -- * Type aliases
  , FileSystem
    -- * Interpreters
  , filesystemReadIO
  , filesystemWriteIO
  , filesystemIO
    -- * Utilities
  , basicResolvePath
  ) where

import Data.Kind (Type)
import Polysemy
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath
import Data.String (fromString)
import GHC.Stack
import qualified System.Directory
import qualified System.FilePath.Glob as Glob
import Runix.Logging (Logging, info)
import System.IO.Error (tryIOError, userError)

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
    CreateDirectory :: Bool -> FilePath -> FileSystemWrite m (Either String ())  -- Bool for createParents
    Remove :: Bool -> FilePath -> FileSystemWrite m (Either String ())  -- Bool for recursive

-- | Public API function that converts Either to Fail
writeFile :: Members [FileSystemWrite, Fail] r => FilePath -> ByteString -> Sem r ()
writeFile p d = send (WriteFile p d) >>= either fail return

-- | Create a directory, optionally creating parent directories
createDirectory :: Members [FileSystemWrite, Fail] r => Bool -> FilePath -> Sem r ()
createDirectory createParents p = send (CreateDirectory createParents p) >>= either fail return

-- | Remove a file or directory, optionally recursive for directories
remove :: Members [FileSystemWrite, Fail] r => Bool -> FilePath -> Sem r ()
remove recursive p = send (Remove recursive p) >>= either fail return

-- | Combined filesystem effect for backwards compatibility
-- This is a type alias that combines both read and write operations
type FileSystem = '[FileSystemRead, FileSystemWrite]

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
    CreateDirectory createParents p -> do
        info $ fromString "creating directory: " <> fromString p <> (if createParents then fromString " (with parents)" else fromString "")
        embed (tryIOError $ System.Directory.createDirectoryIfMissing createParents p) >>= return . either (Left . show) Right
    Remove recursive p -> do
        info $ fromString "removing: " <> fromString p <> (if recursive then fromString " (recursive)" else fromString "")
        embed (tryIOError $ removePathSafe recursive p) >>= return . either (Left . show) Right
  where
    -- Safe removal that handles both files and directories
    removePathSafe :: Bool -> FilePath -> IO ()
    removePathSafe recursive path = do
        isDir <- System.Directory.doesDirectoryExist path
        isFile <- System.Directory.doesFileExist path
        if isDir
            then if recursive
                 then System.Directory.removeDirectoryRecursive path
                 else System.Directory.removeDirectory path
            else if isFile
                 then System.Directory.removeFile path
                 else ioError $ userError $ "Path does not exist: " ++ path

-- | Combined IO interpreter for backwards compatibility
-- Interprets both FileSystemRead and FileSystemWrite
filesystemIO :: HasCallStack => Members [Embed IO, Logging] r => Sem (FileSystemRead : FileSystemWrite : r) a -> Sem r a
filesystemIO = filesystemWriteIO . filesystemReadIO
