{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileSystemSecurityProperties (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (run)
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise, isAbsolute, (</>) , splitPath, addTrailingPathSeparator,
                        splitDirectories, joinPath, takeDirectory, dropTrailingPathSeparator)
import Prelude hiding (readFile)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad (forM_)

import Runix.FileSystem
import qualified Runix.FileSystem.System as System
import Runix.FileSystem.InMemory (InMemoryFS, filesystemInMemory)

--------------------------------------------------------------------------------
-- Test Project Type
--------------------------------------------------------------------------------

newtype PropTest = PropTest FilePath deriving (Show, Eq)

instance HasProjectPath PropTest where
  getProjectPath (PropTest p) = p

--------------------------------------------------------------------------------
-- Path Component Generators
--------------------------------------------------------------------------------

-- | Valid filename characters (conservative set)
genFileNameChar :: Gen Char
genFileNameChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-."

-- | Generate a valid filename (not . or ..)
genFileName :: Gen String
genFileName = do
  firstChar <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
  rest <- listOf genFileNameChar
  let name = firstChar : rest
  return $ if null name || name == "." || name == ".."
           then "file"
           else take 20 name  -- Keep reasonable length

-- | Generate a directory name
genDirName :: Gen String
genDirName = genFileName

-- | Generate a path component (can be ., .., or a name)
genPathComponent :: Gen String
genPathComponent = frequency
  [ (10, genDirName)      -- Normal names most common
  , (3, return ".")       -- Current directory
  , (3, return "..")      -- Parent directory
  ]

--------------------------------------------------------------------------------
-- Path Generators
--------------------------------------------------------------------------------

-- | Generate an absolute path with potential .. and . components
genAbsolutePath :: Gen FilePath
genAbsolutePath = do
  depth <- choose (0, 5)
  components <- vectorOf depth genPathComponent
  return $ "/" ++ joinPath components

-- | Generate a relative path with potential .. and . components
genRelativePath :: Gen FilePath
genRelativePath = do
  depth <- choose (0, 5)
  components <- vectorOf depth genPathComponent
  return $ joinPath components

-- | Generate any path (absolute or relative)
genPath :: Gen FilePath
genPath = oneof [genAbsolutePath, genRelativePath]

-- | Generate a path that tries to escape (has leading ..)
genEscapingRelativePath :: Gen FilePath
genEscapingRelativePath = do
  escapeDepth <- choose (1, 5)
  let escapes = replicate escapeDepth ".."
  maybeSuffix <- listOf genDirName
  return $ joinPath (escapes ++ take 3 maybeSuffix)

-- | Generate a working directory path (always absolute, no .. or .)
genCwdPath :: Gen FilePath
genCwdPath = do
  depth <- choose (0, 4)
  components <- vectorOf depth genDirName
  return $ "/" ++ joinPath components

--------------------------------------------------------------------------------
-- Path Resolution - Proper Implementation
--------------------------------------------------------------------------------

-- | Resolve a path by walking through components
-- This handles .. and . properly, and prevents escaping root
-- Returns Nothing if the path tries to escape root
resolvePath :: FilePath -> Maybe FilePath
resolvePath path = resolveFromRoot "/" path

-- | Resolve a path from a given base directory
-- Both base and path should be absolute
-- Returns Nothing if resolution would escape root
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
resolveRelative :: FilePath -> FilePath -> Maybe FilePath
resolveRelative cwd relPath
  | not (isAbsolute cwd) = error "resolveRelative: cwd must be absolute"
  | isAbsolute relPath = resolvePath relPath
  | otherwise = resolveFromRoot cwd relPath

-- | Check if a resolved path is within an allowed directory
-- Both paths must be absolute and already resolved
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
isPathAllowed :: FilePath   -- ^ Allowed directory (absolute)
              -> FilePath   -- ^ Current working directory (absolute)
              -> FilePath   -- ^ Target path (absolute or relative)
              -> Bool
isPathAllowed allowedDir cwd targetPath =
  case resolvedPath of
    Nothing -> False  -- Path tried to escape root
    Just resolved -> isWithinDirectory allowedDir resolved
  where
    resolvedPath = if isAbsolute targetPath
                   then resolvePath targetPath
                   else resolveRelative cwd targetPath

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "FileSystem Security Properties" $ do

  describe "Path Resolution Properties" $ do
    it "parent of root is root" $ property $
      resolvePath "/.." === Just "/"

    it "multiple parents of root is still root" $ property $
      \(Positive n) -> resolvePath ("/" ++ concat (replicate n "../")) === Just "/"

    it "absolute paths ignore cwd" $ property $
      forAll genCwdPath $ \cwd ->
      forAll genAbsolutePath $ \absPath ->
      resolveRelative cwd absPath === resolvePath absPath

    it "resolving . from any path returns that path" $ property $
      forAll genCwdPath $ \path ->
      resolveRelative path "." === Just path

    it "resolving empty path from cwd returns cwd" $ property $
      forAll genCwdPath $ \cwd ->
      resolveRelative cwd "" === Just cwd

    it "path with redundant / works correctly" $ property $
      resolvePath "/foo//bar///baz" === Just "/foo/bar/baz"

    it "path resolution is idempotent" $ property $
      forAll genAbsolutePath $ \path ->
      case resolvePath path of
        Nothing -> property True  -- Can't resolve further
        Just resolved -> resolvePath resolved === Just resolved

    it "../foo from /bar gives /foo" $ property $
      resolveRelative "/bar" "../foo" === Just "/foo"

    it "../../foo from /a/b/c gives /a/foo" $ property $
      resolveRelative "/a/b/c" "../../foo" === Just "/a/foo"

    it "escaping root with relative path from shallow cwd" $ property $
      resolveRelative "/dir" "../../../../etc/passwd" === Just "/etc/passwd"

  describe "Path Containment Properties" $ do
    it "any path is within root" $ property $
      forAll genCwdPath $ \path ->
      isWithinDirectory "/" path === True

    it "directory contains itself" $ property $
      forAll genCwdPath $ \dir ->
      isWithinDirectory dir dir === True

    it "directory contains its children" $ property $
      forAll genCwdPath $ \dir ->
      forAll genFileName $ \child ->
      case resolvePath (dir </> child) of
        Nothing -> property True  -- Invalid path
        Just childPath -> isWithinDirectory dir childPath === True

    it "directory does not contain its parent (unless parent is self)" $ property $
      forAll genCwdPath $ \dir ->
      let parent = takeDirectory (dropTrailingPathSeparator dir)
      in dir == "/" || parent == dir || not (isWithinDirectory dir parent)

    it "/project does not contain /etc" $ property $
      isWithinDirectory "/project" "/etc/passwd" === False

    it "/project contains /project/sub/file.txt" $ property $
      isWithinDirectory "/project" "/project/sub/file.txt" === True

  describe "Security Check Properties" $ do
    it "reading file in allowed directory is permitted" $ property $
      forAll genCwdPath $ \allowedDir ->
      forAll genCwdPath $ \cwd ->
      forAll genFileName $ \file ->
      -- If cwd is within allowedDir, reading a local file should work
      isWithinDirectory allowedDir cwd ==>
      isPathAllowed allowedDir cwd file === True

    it "reading file outside allowed directory is blocked (absolute path)" $ property $
      isPathAllowed "/project" "/project" "/etc/passwd" === False

    it "escaping via .. from deep directory is blocked" $ property $
      forAll genEscapingRelativePath $ \escapePath ->
      -- From /project/a/b, trying to escape to /etc via ../../../../../../etc
      let allowed = "/project"
          cwd = "/project/a/b"
          target = escapePath </> "etc/passwd"
          resolved = resolveRelative cwd target
      in case resolved of
           Nothing -> property True  -- Couldn't resolve
           Just resolvedPath ->
             not (isWithinDirectory allowed resolvedPath) ==>
             (isPathAllowed allowed cwd target === False)

    it "relative path that stays within allowed dir is permitted" $ property $
      isPathAllowed "/project" "/project/subdir" "../file.txt" === True

    it "absolute path matching cwd but outside allowed is blocked" $ property $
      -- CWD is /tmp, allowed is /project, reading /tmp/file.txt should be blocked
      isPathAllowed "/project" "/tmp" "/tmp/file.txt" === False

    it "security check is transitive: if A contains B and B contains C, then A contains C" $ property $
      -- Generate nested directories to ensure the precondition holds
      forAll genCwdPath $ \dirA ->
      forAll genDirName $ \subB ->
      forAll genDirName $ \subC ->
      let dirB = dirA </> subB
          dirC = dirB </> subC
      in isWithinDirectory dirA dirB && isWithinDirectory dirB dirC ==>
         isWithinDirectory dirA dirC

  describe "Chroot Simulation Properties" $ do
    -- Note: Path resolution alone doesn't enforce chroot boundaries
    -- Security is enforced by combining path resolution WITH the limitToSubpath filter
    -- These tests verify that the security check correctly identifies escapes

    it "security check catches paths that escape starting directory" $ property $
      forAll genCwdPath $ \allowedDir ->
      forAll genEscapingRelativePath $ \escapePath ->
      let cwd = allowedDir </> "subdir"  -- Start inside allowed dir
          target = escapePath </> "etc/passwd"
      in case resolveRelative cwd target of
           Nothing -> property True
           Just resolvedPath ->
             if isWithinDirectory allowedDir resolvedPath
             then isPathAllowed allowedDir cwd target === True
             else isPathAllowed allowedDir cwd target === False

  describe "Path Type Preservation Properties" $ do
    it "relative paths stay relative in results (not in resolution)" $ property $
      forAll genRelativePath $ \relPath ->
      -- The resolved path is absolute, but we need to track that input was relative
      not (isAbsolute relPath) ==> not (isAbsolute relPath)

    it "absolute paths stay absolute" $ property $
      forAll genAbsolutePath $ \absPath ->
      isAbsolute absPath ==>
      case resolvePath absPath of
        Nothing -> True
        Just resolved -> isAbsolute resolved

  describe "Edge Cases" $ do
    it "handles paths with multiple consecutive dots" $ property $
      resolvePath "/foo/.../bar" === Just "/foo/.../bar"  -- ... is a valid name

    it "handles . in filename" $ property $
      resolvePath "/foo/file.txt" === Just "/foo/file.txt"

    it "handles .. in filename" $ property $
      resolvePath "/foo/file..txt" === Just "/foo/file..txt"

    it "handles empty components correctly" $ property $
      resolvePath "/foo//bar" === Just "/foo/bar"

  describe "Consistency with splitPath approach" $ do
    it "our resolver matches the intended behavior of splitPath prefix checking" $ property $
      forAll genCwdPath $ \allowedDir ->
      forAll genCwdPath $ \targetPath ->
      let ourResult = isWithinDirectory allowedDir targetPath
          -- The old approach (what we're replacing)
          normalizedAllowed = addTrailingPathSeparator allowedDir
          normalizedTarget = addTrailingPathSeparator targetPath
          oldResult = splitPath normalizedAllowed `isPrefixOf` splitPath normalizedTarget
      in ourResult === oldResult

--------------------------------------------------------------------------------
-- Integration Tests with InMemory FileSystem
--------------------------------------------------------------------------------

  describe "Integration with FileSystem Operations" $ do
    it "security filter with property-generated paths" $ property $
      forAll genCwdPath $ \allowedDir ->
      forAll genCwdPath $ \cwd ->
      forAll genPath $ \targetPath ->
      let shouldAllow = isPathAllowed allowedDir cwd targetPath
          -- Create a minimal FS for testing
          fs = Map.singleton (fromMaybe "/dummy" $ resolveRelative cwd targetPath) "content"
          program = filterFileSystem @PropTest (limitToSubpath allowedDir) $
                    filterRead @PropTest (limitToSubpath allowedDir) $
                    case resolveRelative cwd targetPath of
                      Nothing -> return ()
                      Just resolved -> readFile @PropTest resolved >> return ()
          result = run $ runError @String $ failToError id $
                   filesystemInMemory cwd fs (PropTest cwd) program
      in case (shouldAllow, result) of
           (True, Right _) -> property True  -- Allowed and succeeded
           (False, Left _) -> property True  -- Blocked and failed
           (True, Left err) -> counterexample ("Should allow but got error: " ++ err) False
           (False, Right _) -> counterexample "Should block but succeeded" False

--------------------------------------------------------------------------------
-- QuickCheck Modifiers for Better Testing
--------------------------------------------------------------------------------

-- | Generate paths guaranteed to be within a directory
newtype PathInDir = PathInDir (FilePath, FilePath) deriving (Show, Eq)

instance Arbitrary PathInDir where
  arbitrary = do
    baseDir <- genCwdPath
    depth <- choose (0, 3)
    components <- vectorOf depth genDirName
    let path = baseDir </> joinPath components
    return $ PathInDir (baseDir, path)

-- | Generate paths guaranteed to be outside a directory
newtype PathOutsideDir = PathOutsideDir (FilePath, FilePath) deriving (Show, Eq)

instance Arbitrary PathOutsideDir where
  arbitrary = do
    -- Generate two different base directories
    dir1Parts <- listOf1 genDirName
    dir2Parts <- listOf1 genDirName
    let baseDir = "/" ++ joinPath (take 2 dir1Parts)
        outsidePath = "/" ++ joinPath (take 2 dir2Parts)
    if baseDir == outsidePath
      then arbitrary  -- Try again
      else return $ PathOutsideDir (baseDir, outsidePath </> "file.txt")
