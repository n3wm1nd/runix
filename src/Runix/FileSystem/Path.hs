{-# LANGUAGE ScopedTypeVariables #-}

-- | Path resolution utilities for filesystem security
--
-- This module provides proper path resolution that:
-- - Handles .. and . components correctly
-- - Prevents escaping root directory
-- - Walks paths step-by-step for security checks
-- - Normalizes paths consistently
module Runix.FileSystem.Path
  ( -- * Path Resolution
    resolvePath
  , resolveRelative
  , resolveFromRoot
    -- * Security Checks
  , isWithinDirectory
  , isPathAllowed
    -- * Utilities
  , normalizePathForCheck
  ) where

import System.FilePath
  ( isAbsolute
  , takeDirectory
  , dropTrailingPathSeparator
  , splitDirectories
  , joinPath
  , (</>)
  )
import Data.List (isPrefixOf)

--------------------------------------------------------------------------------
-- Path Resolution
--------------------------------------------------------------------------------

-- | Resolve a path by walking through components
-- This handles .. and . properly, and prevents escaping root
-- Returns Nothing if the path tries to escape root
--
-- Examples:
-- >>> resolvePath "/foo/bar/../baz"
-- Just "/foo/baz"
-- >>> resolvePath "/.."
-- Just "/"
-- >>> resolvePath "/foo//bar"
-- Just "/foo/bar"
resolvePath :: FilePath -> Maybe FilePath
resolvePath path = resolveFromRoot "/" path

-- | Resolve a path from a given base directory
-- Both base and path should be absolute (or path can be relative)
-- Returns Nothing if resolution would escape root
--
-- Examples:
-- >>> resolveFromRoot "/project" "/file.txt"
-- Just "/file.txt"
-- >>> resolveFromRoot "/project" "subdir/file.txt"
-- Just "/project/subdir/file.txt"
resolveFromRoot :: FilePath -> FilePath -> Maybe FilePath
resolveFromRoot base path
  | not (isAbsolute base) = error "resolveFromRoot: base must be absolute"
  | isAbsolute path = walkPath "/" (splitDirectories $ dropWhile (== '/') path)
  | otherwise = walkPath base (splitDirectories path)
  where
    -- Walk through path components, maintaining current position
    walkPath :: FilePath -> [String] -> Maybe FilePath
    walkPath current [] = Just $ normalizeResult current
    walkPath current (comp:rest)
      | comp == "." || comp == "" = walkPath current rest
      | comp == ".." =
          let parent = takeDirectory (dropTrailingPathSeparator current)
          in if parent == "/" && current == "/"
             then walkPath "/" rest  -- Parent of root is root
             else walkPath parent rest
      | otherwise = walkPath (current </> comp) rest

    -- Normalize result: ensure single leading /, no trailing /
    normalizeResult :: FilePath -> FilePath
    normalizeResult p
      | p == "/" = "/"
      | otherwise = "/" ++ dropWhile (== '/') (dropTrailingPathSeparator p)

-- | Resolve a relative path from a working directory
-- Returns Nothing if resolution would escape root
--
-- Examples:
-- >>> resolveRelative "/project" "subdir/file.txt"
-- Just "/project/subdir/file.txt"
-- >>> resolveRelative "/project" "../etc/passwd"
-- Just "/etc/passwd"
-- >>> resolveRelative "/project" "../../.."
-- Just "/"
resolveRelative :: FilePath -> FilePath -> Maybe FilePath
resolveRelative cwd relPath
  | not (isAbsolute cwd) = error "resolveRelative: cwd must be absolute"
  | isAbsolute relPath = resolvePath relPath
  | otherwise = resolveFromRoot cwd relPath

--------------------------------------------------------------------------------
-- Security Checks
--------------------------------------------------------------------------------

-- | Check if a resolved path is within an allowed directory
-- Both paths must be absolute and already resolved (no .. or .)
--
-- Examples:
-- >>> isWithinDirectory "/project" "/project/file.txt"
-- True
-- >>> isWithinDirectory "/project" "/etc/passwd"
-- False
-- >>> isWithinDirectory "/" "/anything"
-- True
isWithinDirectory :: FilePath -> FilePath -> Bool
isWithinDirectory allowedDir targetPath
  | not (isAbsolute allowedDir && isAbsolute targetPath) =
      error "isWithinDirectory: both paths must be absolute"
  | otherwise =
      let allowedParts = splitDirectories $ dropWhile (== '/') allowedDir
          targetParts = splitDirectories $ dropWhile (== '/') targetPath
          -- Empty allowed dir means root ("/"), which contains everything
          result = if null allowedParts || allowedDir == "/"
                   then True
                   else allowedParts `isPrefixOf` targetParts
      in result

-- | Main security check: resolve path from cwd and check if within allowed dir
-- This is the primary function for security filters
--
-- Examples:
-- >>> isPathAllowed "/project" "/project" "file.txt"
-- True
-- >>> isPathAllowed "/project" "/project" "../etc/passwd"
-- False
-- >>> isPathAllowed "/project" "/project/subdir" "../file.txt"
-- True
isPathAllowed :: FilePath   -- ^ Allowed directory (absolute)
              -> FilePath   -- ^ Current working directory (absolute)
              -> FilePath   -- ^ Target path (absolute or relative)
              -> Bool
isPathAllowed allowedDir cwd targetPath =
  case resolvedPath of
    Nothing -> False  -- Path tried to escape root or is invalid
    Just resolved -> isWithinDirectory allowedDir resolved
  where
    resolvedPath = if isAbsolute targetPath
                   then resolvePath targetPath
                   else resolveRelative cwd targetPath

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Normalize a path for security checking
-- This is a compatibility function that uses the new resolution logic
-- but maintains the same interface as the old code
--
-- This function:
-- 1. Resolves the path (handles .., ., multiple /)
-- 2. Ensures it's absolute
-- 3. Returns it in normalized form
normalizePathForCheck :: FilePath -> FilePath -> FilePath
normalizePathForCheck cwd path =
  case if isAbsolute path
       then resolvePath path
       else resolveRelative cwd path of
    Nothing -> "/"  -- Fallback to root if resolution fails
    Just resolved -> resolved
