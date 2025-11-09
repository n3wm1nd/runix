{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.FileSystem.Effects where
import Data.Kind (Type)
import Polysemy
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath
import Data.List (isPrefixOf)
import Data.String (fromString)
import GHC.Stack
import qualified System.Directory
import qualified System.FilePath.Glob as Glob
import Runix.Logging.Effects (Logging, info)

-- | Read-only filesystem operations
data FileSystemRead (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystemRead m ByteString
    ListFiles :: FilePath -> FileSystemRead m [FilePath]
    FileExists :: FilePath -> FileSystemRead m Bool
    IsDirectory :: FilePath -> FileSystemRead m Bool
    Glob :: FilePath -> String -> FileSystemRead m [FilePath]  -- base path and pattern

makeSem ''FileSystemRead

-- | Write-only filesystem operations
data FileSystemWrite (m :: Type -> Type) a where
    WriteFile :: FilePath -> ByteString -> FileSystemWrite m ()

makeSem ''FileSystemWrite

-- | Combined filesystem effect for backwards compatibility
-- This is a type alias that combines both read and write operations
type FileSystem = '[FileSystemRead, FileSystemWrite]

data AccessPermission = AllowAccess | ForbidAccess String

-- | Limited access control for read operations
limitedAccessRead :: Members [FileSystemRead, Fail] r => (forall m x. FileSystemRead m x -> AccessPermission) -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
limitedAccessRead isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        ReadFile f -> readFile f
        ListFiles f -> listFiles f
        FileExists f -> fileExists f
        IsDirectory f -> isDirectory f
        Glob base pat -> glob base pat
    ForbidAccess reason -> fail $ "not allowed: " ++ reason

-- | Limited access control for write operations
limitedAccessWrite :: Members [FileSystemWrite, Fail] r => (forall m x. FileSystemWrite m x -> AccessPermission) -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
limitedAccessWrite isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        WriteFile f c -> writeFile f c
    ForbidAccess reason -> fail $ "not allowed: " ++ reason

-- | Limit read operations to a subpath
limitSubpathRead :: (Member Fail r, Member FileSystemRead r) => FilePath -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
limitSubpathRead p = limitedAccessRead (\case
    ReadFile sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    ListFiles sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    FileExists sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    IsDirectory sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    Glob base _pat | splitPath p `isPrefixOf` splitPath base -> AllowAccess
    _ -> ForbidAccess $ "not in explicitly allowed path " ++ p
    )

-- | Limit write operations to a subpath
limitSubpathWrite :: (Member Fail r, Member FileSystemWrite r) => FilePath -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
limitSubpathWrite p = limitedAccessWrite (\case
    WriteFile sp _c | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    _ -> ForbidAccess $ "not in explicitly allowed path " ++ p
    )

-- | Chroot for read operations
chrootSubpathRead :: Member FileSystemRead r => FilePath -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
chrootSubpathRead chrootPath = intercept $ \case
    ReadFile f -> readFile (chrootPath </> f)
    ListFiles f -> do
        files <- listFiles (chrootPath </> f)
        return $ fmap (makeRelative chrootPath) files
    FileExists f -> fileExists (chrootPath </> f)
    IsDirectory f -> isDirectory (chrootPath </> f)
    Glob base pat -> do
        files <- glob (chrootPath </> base) pat
        return $ fmap (makeRelative chrootPath) files

-- | Chroot for write operations
chrootSubpathWrite :: Member FileSystemWrite r => FilePath -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
chrootSubpathWrite chrootPath = intercept $ \case
    WriteFile f c -> writeFile (chrootPath </> f) c

-- | Hide dotfiles for read operations
hideDotfilesRead :: Members [FileSystemRead, Fail] r => Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
hideDotfilesRead = intercept $ \case
    ReadFile f | isDotfile f -> fail $ "Access to dotfile denied: " ++ f
                | otherwise -> readFile f
    ListFiles f -> do
        files <- listFiles f
        return $ Prelude.filter (not . isDotfile . takeFileName) files
    FileExists f | isDotfile f -> return False
                 | otherwise -> fileExists f
    IsDirectory f | isDotfile f -> return False
                  | otherwise -> isDirectory f
    Glob base pat -> do
        files <- glob base pat
        return $ Prelude.filter (not . isDotfile . takeFileName) files
  where
    isDotfile :: FilePath -> Bool
    isDotfile path = case takeFileName path of
        ('.':_) -> True
        _ -> False

-- | Hide dotfiles for write operations
hideDotfilesWrite :: Members [FileSystemWrite, Fail] r => Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
hideDotfilesWrite = intercept $ \case
    WriteFile f c | isDotfile f -> fail $ "Access to dotfile denied: " ++ f
                  | otherwise -> writeFile f c
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
        embed $ BL.readFile p
    ListFiles p -> do
        info $ fromString "listing files: " <> fromString p
        embed $ System.Directory.listDirectory p
    FileExists p -> do
        info $ fromString "checking file exists: " <> fromString p
        embed $ System.Directory.doesFileExist p
    IsDirectory p -> do
        info $ fromString "checking is directory: " <> fromString p
        embed $ System.Directory.doesDirectoryExist p
    Glob basePath pattern -> do
        info $ fromString "glob pattern: " <> fromString pattern <> fromString " in " <> fromString basePath
        embed $ globFiles basePath pattern
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
        embed $ BL.writeFile p d

-- | Combined IO interpreter for backwards compatibility
-- Interprets both FileSystemRead and FileSystemWrite
filesystemIO :: HasCallStack => Members [Embed IO, Logging] r => Sem (FileSystemRead : FileSystemWrite : r) a -> Sem r a
filesystemIO = filesystemWriteIO . filesystemReadIO
