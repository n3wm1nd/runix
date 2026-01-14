{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simple (non-parameterized) filesystem effects
--
-- This module provides a simplified interface for code that only needs
-- to work with a single filesystem. It's implemented as type aliases over
-- the parameterized effects from Runix.FileSystem using a Default marker type.
--
-- Use this when you have a single "default" filesystem and don't need
-- multiple independent filesystem views.
--
-- Example usage:
--
-- @
-- myFunc :: Members '[FileSystemRead, FileSystem, Fail] r => Sem r ()
-- myFunc = do
--   content <- readFile \"config.yaml\"
--   files <- listFiles \".\"
-- @
module Runix.FileSystem.Simple where

import Polysemy
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import qualified Runix.FileSystem as Parameterized
import qualified Runix.FileSystem.System as System
import Runix.Logging (Logging)
import Data.ByteString (ByteString)

-- | Default marker type for simple (non-parameterized) filesystem operations
-- This is just a unit type with no data - it's a marker that says "use the default filesystem"
data Default = Default
  deriving (Show, Eq)

-- | Type aliases for simple (non-parameterized) filesystem effects
-- These are just the parameterized effects with Default as the project type
type FileSystem = Parameterized.FileSystem Default
type FileSystemRead = Parameterized.FileSystemRead Default
type FileSystemWrite = Parameterized.FileSystemWrite Default
type FileWatcher = Parameterized.FileWatcher Default

-- | Convenience functions that work with the simple types

-- Filesystem structure operations
listFiles :: Members [FileSystem, Fail] r => FilePath -> Sem r [FilePath]
listFiles = Parameterized.listFiles

fileExists :: Members [FileSystem, Fail] r => FilePath -> Sem r Bool
fileExists = Parameterized.fileExists

isDirectory :: Members [FileSystem, Fail] r => FilePath -> Sem r Bool
isDirectory = Parameterized.isDirectory

glob :: Members [FileSystem, Fail] r => FilePath -> String -> Sem r [FilePath]
glob = Parameterized.glob

getCwd :: Members [FileSystem, Fail] r => Sem r FilePath
getCwd = Parameterized.getCwd

-- File content operations
readFile :: Members [FileSystemRead, Fail] r => FilePath -> Sem r ByteString
readFile = Parameterized.readFile

writeFile :: Members [FileSystemWrite, Fail] r => FilePath -> ByteString -> Sem r ()
writeFile = Parameterized.writeFile

createDirectory :: Members [FileSystemWrite, Fail] r => Bool -> FilePath -> Sem r ()
createDirectory = Parameterized.createDirectory

remove :: Members [FileSystemWrite, Fail] r => Bool -> FilePath -> Sem r ()
remove = Parameterized.remove

-- File watching operations
watchFile :: Member FileWatcher r => FilePath -> Sem r ()
watchFile = Parameterized.watchFile

getChangedFiles :: Member FileWatcher r => Sem r [(FilePath, ByteString, ByteString)]
getChangedFiles = Parameterized.getChangedFiles

clearWatched :: Member FileWatcher r => Sem r ()
clearWatched = Parameterized.clearWatched

unwatchFile :: Member FileWatcher r => FilePath -> Sem r ()
unwatchFile = Parameterized.unwatchFile

getWatchedFiles :: Member FileWatcher r => Sem r [FilePath]
getWatchedFiles = Parameterized.getWatchedFiles

-- | Interpreter: translates Simple (Default-based) effects to any parameterized filesystem
-- This allows code written against the simple interface to work with any actual filesystem
withDefaultFileSystem :: forall p r x. Member (Parameterized.FileSystem p) r => Sem (FileSystem : r) x -> Sem r x
withDefaultFileSystem = interpret $ \case
  Parameterized.GetFileSystem -> pure Default
  Parameterized.ListFiles p -> send (Parameterized.ListFiles @p p)
  Parameterized.FileExists p -> send (Parameterized.FileExists @p p)
  Parameterized.IsDirectory p -> send (Parameterized.IsDirectory @p p)
  Parameterized.Glob b pat -> send (Parameterized.Glob @p b pat)
  Parameterized.GetCwd -> send (Parameterized.GetCwd @p)

withDefaultFileSystemRead :: forall p r x. Member (Parameterized.FileSystemRead p) r => Sem (FileSystemRead : r) x -> Sem r x
withDefaultFileSystemRead = interpret $ \case
  Parameterized.ReadFile p -> send (Parameterized.ReadFile @p p)

withDefaultFileSystemWrite :: forall p r x. Member (Parameterized.FileSystemWrite p) r => Sem (FileSystemWrite : r) x -> Sem r x
withDefaultFileSystemWrite = interpret $ \case
  Parameterized.WriteFile p d -> send (Parameterized.WriteFile @p p d)
  Parameterized.CreateDirectory cp p -> send (Parameterized.CreateDirectory @p cp p)
  Parameterized.Remove rec p -> send (Parameterized.Remove @p rec p)

-- | IO-based interpreter for simple filesystem effects
-- This interprets the simple FileSystemRead and FileSystemWrite effects directly to System effects then IO
filesystemIO :: (Member (Embed IO) r,  Member Logging r, Member Fail r) =>
  Sem (FileSystemWrite : FileSystemRead : FileSystem :r)   a -> Sem r a
filesystemIO =
  System.filesystemReadIO . System.filesystemWriteIO . Parameterized.fileSystemLocal "/"
  . raise3Under . raise3Under -- knock out system filesystem effects
  . withDefaultFileSystem . withDefaultFileSystemRead . withDefaultFileSystemWrite
  . raise3Under . raise3Under . raise3Under -- knock out parametrized (non-default) effects

-- | Generic content-based interpreter for simple FileWatcher effect
-- Uses FileSystemRead to detect changes by comparing file content
-- This is the canonical way to watch files with the Default/Simple filesystem
fileWatcher :: Members '[FileSystemRead, Fail] r => Sem (FileWatcher : r) a -> Sem r a
fileWatcher = Parameterized.fileWatcherGeneric