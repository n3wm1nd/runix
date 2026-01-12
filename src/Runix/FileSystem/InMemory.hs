{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | In-memory filesystem implementation for testing
--
-- This module provides an in-memory filesystem interpreter that uses
-- a Map for storage. Useful for testing without touching the real filesystem.
module Runix.FileSystem.InMemory
  ( InMemoryFS
  , filesystemReadInMemory
  , filesystemWriteInMemory
  , filesystemInMemory
  ) where

import Polysemy
import Polysemy.Error
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath
import Data.List (sort, isPrefixOf)
import qualified System.FilePath.Glob as Glob
import Runix.FileSystem.System
import qualified Runix.FileSystem as Param

-- | In-memory filesystem represented as a map from absolute paths to file contents
type InMemoryFS = Map FilePath ByteString

-- | In-memory interpreter for FileSystemRead
-- Takes a current working directory and an in-memory filesystem map
filesystemReadInMemory :: Member (Error String) r
                       => FilePath     -- ^ Current working directory
                       -> InMemoryFS   -- ^ In-memory filesystem
                       -> Sem (FileSystemRead : r) a
                       -> Sem r a
filesystemReadInMemory cwd fs = interpret $ \case
    ReadFile p -> return $ case Map.lookup (resolveAbsolutePath cwd p) fs of
        Just content -> Right content
        Nothing -> Left $ "File not found: " ++ p

    ListFiles p -> do
        let dirPath = addTrailingPathSeparator $ resolveAbsolutePath cwd p
            allPaths = Map.keys fs
            -- Get immediate children (files and directories)
            children = [ firstComponent
                       | path <- allPaths
                       , dirPath `isPrefixOf` path
                       , let remainder = drop (length dirPath) path
                       , not (null remainder)
                       , let firstComponent = takeWhile (/= '/') remainder
                       , not (null firstComponent)
                       ]
            uniqueChildren = sort $ nub' children
        return $ Right uniqueChildren
      where
        nub' :: Ord a => [a] -> [a]
        nub' = Map.keys . Map.fromList . map (\x -> (x, ()))

    FileExists p -> return $ Right $ Map.member (resolveAbsolutePath cwd p) fs

    IsDirectory _p -> return $ Right False  -- Simplified: no directories in our in-memory FS

    Glob base pat -> do
        let basePath = resolveAbsolutePath cwd base

            -- Determine the search directory
            (searchDir, searchPattern) =
              if isAbsolute pat
              then
                -- Absolute pattern: extract directory and pattern parts
                let dir = takeDirectory pat
                    patName = takeFileName pat
                in (dir, patName)
              else
                -- Relative pattern: navigate from base according to pattern
                let patDir = takeDirectory pat
                    patName = takeFileName pat
                    resolvedDir = basicResolvePath (basePath </> patDir)
                in (resolvedDir, patName)

            -- Get all files in filesystem
            allFiles = Map.keys fs

            -- Compile the final search pattern
            finalPattern = Glob.compile searchPattern

            -- Find files under searchDir that match the pattern name
            matchedAbsolute = filter (\f ->
              let dir = takeDirectory f
                  name = takeFileName f
              in dir == searchDir && Glob.match finalPattern name) allFiles

            -- Convert to paths relative to base
            matchedRelative = map (makeRelativeTo basePath) matchedAbsolute

        return $ Right $ sort matchedRelative
      where
        makeRelativeTo :: FilePath -> FilePath -> FilePath
        makeRelativeTo base target =
          let baseParts = splitDirectories (normalise base)
              targetParts = splitDirectories (normalise target)
              common = length $ takeWhile id $ zipWith (==) baseParts targetParts
              ups = replicate (length baseParts - common) ".."
              downs = drop common targetParts
              rel = ups ++ downs
          in if null rel then "." else joinPath rel

    GetCwd -> return cwd
  where
    resolveAbsolutePath basePath path
      | isAbsolute path = basicResolvePath path
      | otherwise = basicResolvePath (basePath </> path)

-- | In-memory interpreter for FileSystemWrite (no-op for now)
-- Could be extended to return updated InMemoryFS
filesystemWriteInMemory :: Member (Error String) r
                        => InMemoryFS
                        -> Sem (FileSystemWrite : r) a
                        -> Sem r a
filesystemWriteInMemory _fs = interpret $ \case
    WriteFile _p _d -> return $ Right ()
    CreateDirectory _createParents _p -> return $ Right ()
    Remove _recursive _p -> return $ Right ()

-- | In-memory interpreter for parameterized filesystem effects
-- Interprets FileSystem project, FileSystemRead project, and FileSystemWrite project
filesystemInMemory :: forall project r a.
                      ( Param.HasProjectPath project
                      , Member (Error String) r
                      )
                   => FilePath     -- ^ Current working directory
                   -> InMemoryFS   -- ^ In-memory filesystem
                   -> project      -- ^ Project configuration
                   -> Sem (Param.FileSystemWrite project : Param.FileSystemRead project : Param.FileSystem project : r) a
                   -> Sem r a
filesystemInMemory cwd fs proj =
  interpretFileSystem . interpretFileSystemRead . interpretFileSystemWrite
  where
    -- Resolve a path to an absolute path for filesystem lookups
    resolvePath :: FilePath -> FilePath
    resolvePath p
      | isAbsolute p = basicResolvePath p
      | otherwise = basicResolvePath (cwd </> p)

    interpretFileSystem :: Sem (Param.FileSystem project : r') x -> Sem r' x
    interpretFileSystem = interpret $ \case
      Param.GetFileSystem -> return proj
      Param.GetCwd -> return $ Right cwd
      Param.ListFiles p -> do
        let dirPath = addTrailingPathSeparator $ resolvePath p
            allPaths = Map.keys fs
            -- Get immediate children (files and directories)
            children = [ firstComponent
                       | path <- allPaths
                       , dirPath `isPrefixOf` path
                       , let remainder = drop (length dirPath) path
                       , not (null remainder)
                       , let firstComponent = takeWhile (/= '/') remainder
                       , not (null firstComponent)
                       ]
            -- Remove duplicates using Map
            uniqueChildren = sort $ Map.keys $ Map.fromList $ map (\x -> (x, ())) children
        return $ Right uniqueChildren
      Param.FileExists p -> return $ Right $ Map.member (resolvePath p) fs
      Param.IsDirectory _p -> return $ Right False
      Param.Glob base pat -> do
        let basePath = resolvePath base
            (searchDir, searchPattern) =
              if isAbsolute pat
              then (takeDirectory pat, takeFileName pat)
              else
                let patDir = takeDirectory pat
                    patName = takeFileName pat
                    resolvedDir = basicResolvePath (basePath </> patDir)
                in (resolvedDir, patName)
            allFiles = Map.keys fs
            finalPattern = Glob.compile searchPattern
            matchedAbsolute = filter (\f ->
              let dir = takeDirectory f
                  name = takeFileName f
              in dir == searchDir && Glob.match finalPattern name) allFiles
            matchedRelative = map (makeRelativeTo basePath) matchedAbsolute
        return $ Right $ sort matchedRelative
      where
        makeRelativeTo :: FilePath -> FilePath -> FilePath
        makeRelativeTo base target =
          let baseParts = splitDirectories (normalise base)
              targetParts = splitDirectories (normalise target)
              common = length $ takeWhile id $ zipWith (==) baseParts targetParts
              ups = replicate (length baseParts - common) ".."
              downs = drop common targetParts
              rel = ups ++ downs
          in if null rel then "." else joinPath rel

    interpretFileSystemRead :: Sem (Param.FileSystemRead project : r') x -> Sem r' x
    interpretFileSystemRead = interpret $ \case
      Param.ReadFile p -> return $ case Map.lookup (resolvePath p) fs of
        Just content -> Right content
        Nothing -> Left $ "File not found: " ++ show (resolvePath p)

    interpretFileSystemWrite :: Sem (Param.FileSystemWrite project : r') x -> Sem r' x
    interpretFileSystemWrite = interpret $ \case
      Param.WriteFile _p _d -> return $ Right ()
      Param.CreateDirectory _createParents _p -> return $ Right ()
      Param.Remove _recursive _p -> return $ Right ()
