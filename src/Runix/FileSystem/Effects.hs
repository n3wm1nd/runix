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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath
import Data.List (isPrefixOf)
import Data.String (fromString)
import GHC.Stack
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

-- | Limited access control for read operations
limitedAccessRead :: Member FileSystemRead r => (forall m x. FileSystemRead m x -> AccessPermission) -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
limitedAccessRead isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        ReadFile f -> send (ReadFile f)
        ListFiles f -> send (ListFiles f)
        FileExists f -> send (FileExists f)
        IsDirectory f -> send (IsDirectory f)
        Glob base pat -> send (Glob base pat)
    ForbidAccess reason -> case action of
        ReadFile _ -> return (Left ("not allowed: " ++ reason))
        ListFiles _ -> return (Left ("not allowed: " ++ reason))
        FileExists _ -> return (Left ("not allowed: " ++ reason))
        IsDirectory _ -> return (Left ("not allowed: " ++ reason))
        Glob _ _ -> return (Left ("not allowed: " ++ reason))

-- | Limited access control for write operations
limitedAccessWrite :: Member FileSystemWrite r => (forall m x. FileSystemWrite m x -> AccessPermission) -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
limitedAccessWrite isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        WriteFile f c -> send (WriteFile f c)
    ForbidAccess reason -> case action of
        WriteFile _ _ -> return (Left ("not allowed: " ++ reason))

-- | Limit read operations to a subpath
limitSubpathRead :: Member FileSystemRead r => FilePath -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
limitSubpathRead p = limitedAccessRead (\case
    ReadFile sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    ListFiles sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    FileExists sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    IsDirectory sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    Glob base _pat | splitPath p `isPrefixOf` splitPath base -> AllowAccess
    _ -> ForbidAccess $ "not in explicitly allowed path " ++ p
    )

-- | Limit write operations to a subpath
limitSubpathWrite :: Member FileSystemWrite r => FilePath -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
limitSubpathWrite p = limitedAccessWrite (\case
    WriteFile sp _c | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    _ -> ForbidAccess $ "not in explicitly allowed path " ++ p
    )

-- | Chroot for read operations
chrootSubpathRead :: Member FileSystemRead r => FilePath -> Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
chrootSubpathRead chrootPath = intercept $ \case
    ReadFile f -> send (ReadFile (chrootPath </> f))
    ListFiles f -> send (ListFiles (chrootPath </> f)) >>= return . fmap (fmap (makeRelative chrootPath))
    FileExists f -> send (FileExists (chrootPath </> f))
    IsDirectory f -> send (IsDirectory (chrootPath </> f))
    Glob base pat -> send (Glob (chrootPath </> base) pat) >>= return . fmap (fmap (makeRelative chrootPath))

-- | Chroot for write operations
chrootSubpathWrite :: Member FileSystemWrite r => FilePath -> Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
chrootSubpathWrite chrootPath = intercept $ \case
    WriteFile f c -> send (WriteFile (chrootPath </> f) c)

-- | Hide dotfiles for read operations
hideDotfilesRead :: Member FileSystemRead r => Sem (FileSystemRead : r) a -> Sem (FileSystemRead : r) a
hideDotfilesRead = intercept $ \case
    ReadFile f | isDotfile f -> return (Left ("Access to dotfile denied: " ++ f))
                | otherwise -> send (ReadFile f)
    ListFiles f -> send (ListFiles f) >>= return . fmap (Prelude.filter (not . isDotfile . takeFileName))
    FileExists f | isDotfile f -> return (Right False)
                 | otherwise -> send (FileExists f)
    IsDirectory f | isDotfile f -> return (Right False)
                  | otherwise -> send (IsDirectory f)
    Glob base pat -> send (Glob base pat) >>= return . fmap (Prelude.filter (not . isDotfile . takeFileName))
  where
    isDotfile :: FilePath -> Bool
    isDotfile path = case takeFileName path of
        ('.':_) -> True
        _ -> False

-- | Hide dotfiles for write operations
hideDotfilesWrite :: Member FileSystemWrite r => Sem (FileSystemWrite : r) a -> Sem (FileSystemWrite : r) a
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
