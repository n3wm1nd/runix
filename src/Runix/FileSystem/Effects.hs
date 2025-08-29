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
import Prelude hiding (readFile, writeFile)
import Polysemy.Fail
import System.FilePath 
import Data.List (isPrefixOf)

data FileSystem (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystem m ByteString
    WriteFile :: FilePath -> ByteString -> FileSystem m ()
    ListFiles :: FilePath -> FileSystem m [FilePath]
    FileExists :: FilePath -> FileSystem m Bool
    IsDirectory :: FilePath -> FileSystem m Bool

makeSem ''FileSystem

data AccessPermission = AllowAccess | ForbidAccess String

limitedAccess :: Members [FileSystem, Fail] r => (forall m x. FileSystem m x -> AccessPermission) -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
limitedAccess isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        ReadFile f -> readFile f
        WriteFile f c -> writeFile f c
        ListFiles f -> listFiles f
        FileExists f -> fileExists f
        IsDirectory f -> isDirectory f

    ForbidAccess reason -> fail $ "not allowed: " ++ reason

limitSubpath :: (Member Fail r, Member FileSystem r) => FilePath -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
limitSubpath p = limitedAccess (\case
    ReadFile sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    WriteFile sp _c | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    ListFiles sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    FileExists sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    IsDirectory sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    _ -> ForbidAccess $ p ++ " is not in explicitly allowed path " ++ p
    )

-- Chroot into a subdirectory by rewriting paths relative to the chroot path
chrootSubpath :: Member FileSystem r => FilePath -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
chrootSubpath chrootPath = intercept $ \case
    ReadFile f -> readFile (chrootPath </> f)
    WriteFile f c -> writeFile (chrootPath </> f) c
    ListFiles f -> do
        files <- listFiles (chrootPath </> f)
        -- Remove the chroot prefix from returned paths
        return $ fmap (makeRelative chrootPath) files
    FileExists f -> fileExists (chrootPath </> f)
    IsDirectory f -> isDirectory (chrootPath </> f)

-- Hide dotfiles (files starting with '.')
hideDotfiles :: Members [FileSystem, Fail] r => Sem (FileSystem : r) a -> Sem (FileSystem : r) a
hideDotfiles = intercept $ \case 
    ReadFile f | isDotfile f -> fail $ "Access to dotfile denied: " ++ f
                | otherwise -> readFile f
    WriteFile f c | isDotfile f -> fail $ "Access to dotfile denied: " ++ f
                  | otherwise -> writeFile f c
    ListFiles f -> do
        files <- listFiles f
        -- Filter out dotfiles from the listing
        return $ Prelude.filter (not . isDotfile . takeFileName) files
    FileExists f | isDotfile f -> return False  -- Hide existence of dotfiles
                 | otherwise -> fileExists f
    IsDirectory f | isDotfile f -> return False  -- Hide existence of dot directories
                  | otherwise -> isDirectory f
  where
    isDotfile :: FilePath -> Bool
    isDotfile path = case takeFileName path of
        ('.':_) -> True
        _ -> False