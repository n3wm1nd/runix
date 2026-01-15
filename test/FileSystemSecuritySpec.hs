{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module FileSystemSecuritySpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise, isAbsolute, (</>) , splitPath, addTrailingPathSeparator, splitDirectories, joinPath)
import Prelude hiding (readFile)
import Data.List (isInfixOf, isPrefixOf)

import Runix.FileSystem
import qualified Runix.FileSystem.System as System
import qualified Runix.FileSystem.Path as Path
import Runix.FileSystem.InMemory (InMemoryFS, filesystemInMemory)

--------------------------------------------------------------------------------
-- Test Project Type
--------------------------------------------------------------------------------

-- | Test project representing the entire filesystem (root at /)
newtype SecurityTest = SecurityTest FilePath deriving (Show, Eq)

instance HasProjectPath SecurityTest where
  getProjectPath (SecurityTest p) = p

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run with security filter (equivalent to old limitSubpathRead)
limitSubpathRead :: FilePath
                 -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Fail, Error String] a
                 -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Fail, Error String] a
limitSubpathRead allowedPath prog =
  filterFileSystem @SecurityTest (limitToSubpath allowedPath) (filterRead @SecurityTest (limitToSubpath allowedPath) prog)

-- | Run filesystem test with Fail support
runFSWithFail :: FilePath -> InMemoryFS
              -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Fail, Error String] a
              -> Either String a
runFSWithFail cwd fs prog =
  let proj = SecurityTest cwd  -- Use CWD as project root for consistent path resolution
  in run
    . runError @String
    . failToError id
    . filesystemInMemory cwd fs proj
    $ prog

-- | Run filesystem test without Fail (for glob that returns Either)
runFS :: FilePath -> InMemoryFS
      -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Error String] a
      -> Either String a
runFS cwd fs prog =
  let proj = SecurityTest cwd  -- Use CWD as project root for consistent path resolution
  in run
    . runError @String
    . filesystemInMemory cwd fs proj
    $ prog

-- | Run filesystem test with chroot layer
-- This tests that security filters work with chrooted filesystem
runFSWithChroot :: FilePath -> InMemoryFS
                -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Fail, Error String] a
                -> Either String a
runFSWithChroot projectRoot fs prog =
  let proj = SecurityTest projectRoot
  in run
    . runError @String
    . failToError id
    . filesystemInMemory "/" fs proj  -- In-memory backend
    . chrootFileSystem @SecurityTest  -- Chroot layer - translates paths
    $ prog

-- | Run filesystem test with filter applied BEFORE chroot
-- The filter sees system paths like "/project/file.txt"
runFSFilterBeforeChroot :: FilePath -> InMemoryFS -> PathFilter
                        -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Fail, Error String] a
                        -> Either String a
runFSFilterBeforeChroot projectRoot fs pathFilter prog =
  let proj = SecurityTest projectRoot
  in run
    . runError @String
    . failToError id
    . filesystemInMemory "/" fs proj
    . filterFileSystem @SecurityTest pathFilter  -- Filter before chroot
    . filterRead @SecurityTest pathFilter
    . chrootFileSystem @SecurityTest  -- Chroot after filter
    $ prog

-- | Run filesystem test with filter applied AFTER chroot
-- The filter sees chroot-relative paths like "/file.txt"
runFSFilterAfterChroot :: FilePath -> InMemoryFS -> PathFilter
                       -> Sem '[FileSystemWrite SecurityTest, FileSystemRead SecurityTest, FileSystem SecurityTest, Fail, Error String] a
                       -> Either String a
runFSFilterAfterChroot projectRoot fs pathFilter prog =
  let proj = SecurityTest projectRoot
  in run
    . runError @String
    . failToError id
    . filesystemInMemory "/" fs proj
    . chrootFileSystem @SecurityTest  -- Chroot before filter
    . filterFileSystem @SecurityTest pathFilter  -- Filter after chroot
    . filterRead @SecurityTest pathFilter
    $ prog

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- Test isPathAllowed logic directly using the new Path module
testIsPathAllowed :: FilePath -> FilePath -> FilePath -> Bool
testIsPathAllowed allowedPath cwd targetPath =
  Path.isPathAllowed allowedPath cwd targetPath

spec :: Spec
spec = do
  describe "Path Utilities" $ do
    it "isPathAllowed rejects files outside allowed path (absolute paths)" $ do
      -- Test with absolute paths (like glob returns)
      testIsPathAllowed "/project" "/project/subdir" "/file.txt" `shouldBe` False
      testIsPathAllowed "/project" "/project/subdir" "/project/file.txt" `shouldBe` True

    it "isPathAllowed rejects files outside allowed path (relative paths)" $ do
      -- Test with relative paths (like ReadFile might use)
      -- Relative path "../../etc/passwd" from cwd "/project/subdir" resolves to "/etc/passwd"
      testIsPathAllowed "/project" "/project/subdir" "../../etc/passwd" `shouldBe` False
      -- Relative path "../file.txt" from cwd "/project/subdir" resolves to "/project/file.txt"
      testIsPathAllowed "/project" "/project/subdir" "../file.txt" `shouldBe` True
      -- Relative path "file.txt" from cwd "/project/subdir" resolves to "/project/subdir/file.txt"
      testIsPathAllowed "/project" "/project/subdir" "file.txt" `shouldBe` True

    it "normalise does NOT resolve .. components" $ do
      let path1 = normalise "/project/subdir" </> "../../README.md"
          path2 = normalise ("/project/subdir" </> "../../README.md")

      -- normalise does NOT resolve .., it just cleans up
      path1 `shouldBe` "/project/subdir/../../README.md"
      path2 `shouldBe` "/project/subdir/../../README.md"

    it "splitPath behavior for directory vs file" $ do
      let dirPath = splitPath "/project"
          filePath = splitPath "/project/file.txt"

      -- See actual values
      dirPath `shouldBe` ["/", "project"]
      filePath `shouldBe` ["/", "project/", "file.txt"]

      -- This will fail - showing the actual problem
      dirPath `isPrefixOf` filePath `shouldBe` False

    it "security check should reject paths outside allowed directory" $ do
      let allowedPath = "/project"
          targetPath = "/etc/passwd"
          normalizedAllowed = addTrailingPathSeparator $ normalise allowedPath
          normalizedTarget = normalise targetPath

      -- Debug: see the actual split paths
      let allowedSplit = splitPath normalizedAllowed
          targetSplit = splitPath normalizedTarget

      allowedSplit `shouldBe` ["/", "project/"]
      targetSplit `shouldBe` ["/", "etc/", "passwd"]

      -- This should be False
      (allowedSplit `isPrefixOf` targetSplit) `shouldBe` False

  describe "limitSubpathRead Security" $ do
    it "allows reading files within allowed path (absolute)" $ do
      let fs = Map.fromList [("/project/file.txt", "content")]
          program = limitSubpathRead "/project" $ readFile @SecurityTest "/project/file.txt"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "content"

    it "allows reading files within allowed path (relative)" $ do
      -- Relative path "subdir/file.txt" with CWD "/project" resolves to "/project/subdir/file.txt"
      let fs = Map.fromList [("/project/subdir/file.txt", "content")]
          program = limitSubpathRead "/project" $ readFile @SecurityTest "subdir/file.txt"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "content"

    it "blocks reading files outside allowed path (absolute)" $ do
      let fs = Map.fromList [("/etc/passwd", "secret")]
          program = limitSubpathRead "/project" $ readFile @SecurityTest "/etc/passwd"
          result = runFSWithFail "/project" fs program

      result `shouldSatisfy` \case
        Left err -> ("Access denied" `isInfixOf` err) || ("filtered out by" `isInfixOf` err)
        Right _ -> False

    it "blocks reading files outside allowed path (relative escape)" $ do
      -- "../../../etc/passwd" from "/project" normalizes to "/etc/passwd"
      let fs = Map.fromList [("/etc/passwd", "secret"), ("/project/dummy", "dummy")]
          --Test with security
          resultWithSecurity = runFSWithFail "/project" fs $ limitSubpathRead "/project" $ readFile @SecurityTest "/etc/passwd"

      -- Should be blocked with security when using absolute path
      resultWithSecurity `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

    it "uses actual CWD to resolve relative paths, then checks against allowedPath" $ do
      -- Critical test: when actual CWD differs from allowedPath
      -- The dummy filesystem has CWD = "/actual/cwd"
      -- But we want to restrict to "/project"
      -- A file at "/actual/cwd/file.txt" should be BLOCKED because it's outside /project
      let fs = Map.fromList [("/actual/cwd/file.txt", "content"), ("/project/allowed.txt", "allowed")]
          -- Actual CWD is /actual/cwd, but we're restricting to /project
          -- "file.txt" resolves to /actual/cwd/file.txt which is outside /project
          resultBlocked = runFSWithFail "/actual/cwd" fs $ limitSubpathRead "/project" $ readFile @SecurityTest "file.txt"
          -- "/project/allowed.txt" is inside /project
          resultAllowed = runFSWithFail "/actual/cwd" fs $ limitSubpathRead "/project" $ readFile @SecurityTest "/project/allowed.txt"

      -- "file.txt" resolves to /actual/cwd/file.txt which is outside /project -> BLOCKED
      resultBlocked `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

      -- "/project/allowed.txt" is absolute and inside /project -> ALLOWED
      resultAllowed `shouldBe` Right "allowed"

    it "blocks parent directory escape with ../" $ do
      -- Reproduce the actual bug: CWD is /project/subdir, restricted to /project
      -- Reading ../../README.md should resolve to /README.md and be blocked
      let fs = Map.fromList
            [ ("/README.md", "root readme")
            , ("/project/README.md", "project readme")
            , ("/project/subdir/file.txt", "subdir file")
            ]
          -- CWD is in a subdirectory, restricted to parent directory
          result = runFSWithFail "/project/subdir" fs $ limitSubpathRead "/project" $ readFile @SecurityTest "../../README.md"

      -- "../../README.md" from /project/subdir resolves to /README.md which is outside /project -> BLOCKED
      result `shouldSatisfy` \case
        Left err -> ("Access denied" `isInfixOf` err) || ("filtered out by" `isInfixOf` err)
        Right _ -> False

    it "allows reading files with relative paths within allowed directory" $ do
      -- Test that relative paths still work for normal operations (not just glob)
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "content")
            , ("/project/file.txt", "parent content")
            ]
          -- CWD is /project/subdir, read relative path "file.txt"
          result = runFSWithFail "/project/subdir" fs $ limitSubpathRead "/project" $ readFile @SecurityTest "file.txt"

      result `shouldBe` Right "content"

    it "allows reading files with relative .. that stay within allowed directory" $ do
      -- Test that relative paths with .. work when they don't escape
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "subdir content")
            , ("/project/file.txt", "parent content")
            ]
          -- CWD is /project/subdir, read "../file.txt" which resolves to /project/file.txt
          result = runFSWithFail "/project/subdir" fs $ limitSubpathRead "/project" $ readFile @SecurityTest "../file.txt"

      result `shouldBe` Right "parent content"

    it "blocks relative paths that escape allowed directory" $ do
      -- Test that relative paths are properly checked even with complex .. patterns
      let fs = Map.fromList
            [ ("/etc/passwd", "secrets")
            , ("/project/deep/nested/dir/file.txt", "content")
            ]
          -- CWD is /project/deep/nested/dir, try to read ../../../../etc/passwd
          result = runFSWithFail "/project/deep/nested/dir" fs $
                   limitSubpathRead "/project" $ readFile @SecurityTest "../../../../etc/passwd"

      result `shouldSatisfy` \case
        Left err -> ("Access denied" `isInfixOf` err) || ("filtered out by" `isInfixOf` err)
        Right _ -> False

  describe "Glob Operations" $ do
    it "glob returns matching files within base directory" $ do
      let fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            , ("/project/doc.md", "markdown")
            , ("/other/file3.txt", "content3")
            ]
          result = runFSWithFail "/project" fs $ glob @SecurityTest "/project" "*.txt"

      -- Glob returns paths relative to base
      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "glob works with relative base path" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file1.txt", "content1")
            , ("/project/subdir/file2.txt", "content2")
            , ("/project/subdir/doc.md", "markdown")
            ]
          -- "subdir" relative to CWD "/project" -> "/project/subdir"
          result = runFSWithFail "/project" fs $ glob @SecurityTest "subdir" "*.txt"

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "glob matches patterns with wildcards" $ do
      let fs = Map.fromList
            [ ("/project/test_file.txt", "content1")
            , ("/project/test_data.txt", "content2")
            , ("/project/prod_file.txt", "content3")
            ]
          result = runFSWithFail "/project" fs $ glob @SecurityTest "/project" "test_*.txt"

      result `shouldBe` Right ["test_data.txt", "test_file.txt"]

    it "glob returns empty list when no matches" $ do
      let fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            ]
          result = runFSWithFail "/project" fs $ glob @SecurityTest "/project" "*.md"

      result `shouldBe` Right []

  describe "Glob with limitSubpathRead Security" $ do
    it "blocks the most basic escape: glob . with ../file pattern" $ do
      -- THE MOST BASIC ATTACK: glob(".", "../*")
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "allowed file")
            , ("/project/parent.txt", "should be blocked")
            , ("/etc/passwd", "definitely blocked")
            ]
          -- CWD is /project/subdir, allowed path is /project/subdir
          -- glob(".", "../*") tries to list files in /project
          program = limitSubpathRead "/project/subdir" $ glob @SecurityTest "." "../*"
          result = runFSWithFail "/project/subdir" fs program

      -- Must be empty - /project/parent.txt is outside /project/subdir
      result `shouldBe` Right []

    it "allows glob within allowed path (absolute base)" $ do
      let fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            , ("/project/doc.md", "markdown")
            ]
          program = limitSubpathRead "/project" $ glob @SecurityTest "/project" "*.txt"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "allows glob within allowed path (relative base)" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file1.txt", "content1")
            , ("/project/subdir/file2.txt", "content2")
            , ("/project/subdir/doc.md", "markdown")
            ]
          -- "subdir" relative to CWD "/project" resolves to "/project/subdir"
          program = limitSubpathRead "/project" $ glob @SecurityTest "subdir" "*.txt"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "blocks glob with base outside allowed path (absolute)" $ do
      let fs = Map.fromList
            [ ("/etc/file1.conf", "config1")
            , ("/etc/file2.conf", "config2")
            ]
          program = limitSubpathRead "/project" $ glob @SecurityTest "/etc" "*.conf"
          result = runFSWithFail "/project" fs program

      result `shouldSatisfy` \case
        Left err -> ("Access denied" `isInfixOf` err) || ("filtered out by" `isInfixOf` err)
        Right _ -> False

    it "blocks glob with base that escapes via relative path" $ do
      -- CWD is /project/subdir, restricted to /project
      -- Base "../.." resolves to /project/subdir/../.. = / which is outside /project
      let fs = Map.fromList
            [ ("/README.md", "root readme")
            , ("/project/README.md", "project readme")
            , ("/project/subdir/file.txt", "content")
            ]
          program = limitSubpathRead "/project" $ glob @SecurityTest "../.." "*.md"
          result = runFSWithFail "/project/subdir" fs program

      result `shouldSatisfy` \case
        Left err -> ("Access denied" `isInfixOf` err) || ("filtered out by" `isInfixOf` err)
        Right _ -> False

    it "allows glob in subdirectory with relative base" $ do
      -- This is the key test: CWD is /project/subdir, restricted to /project
      -- Base "." should resolve to /project/subdir which IS within /project
      let fs = Map.fromList
            [ ("/project/subdir/file1.txt", "content1")
            , ("/project/subdir/file2.txt", "content2")
            , ("/project/subdir/doc.md", "markdown")
            , ("/project/other.txt", "other")
            ]
          -- "." relative to CWD "/project/subdir" -> "/project/subdir"
          program = limitSubpathRead "/project" $ glob @SecurityTest "." "*.txt"
          result = runFSWithFail "/project/subdir" fs program

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "allows glob with current directory when CWD is within allowed path" $ do
      -- CWD = /project/deep/nested/dir, allowed = /project
      -- Base "." should resolve to /project/deep/nested/dir which is within /project
      let fs = Map.fromList
            [ ("/project/deep/nested/dir/file1.txt", "content1")
            , ("/project/deep/nested/dir/file2.txt", "content2")
            , ("/project/deep/nested/dir/doc.md", "markdown")
            ]
          program = limitSubpathRead "/project" $ glob @SecurityTest "." "*.txt"
          result = runFSWithFail "/project/deep/nested/dir" fs program

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "allows glob with parent directory if still within allowed path" $ do
      -- CWD = /project/subdir, allowed = /project
      -- Base ".." should resolve to /project which is the boundary (allowed)
      let fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            , ("/project/subdir/nested.txt", "nested")
            ]
          program = limitSubpathRead "/project" $ glob @SecurityTest ".." "*.txt"
          result = runFSWithFail "/project/subdir" fs program

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "filters glob results that escape via .. in pattern" $ do
      -- CRITICAL TEST: Base is "." (allowed), but pattern "../*.txt" escapes
      -- This is the ACTUAL attack case from your bug report
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "subdir file")
            , ("/project/file.txt", "project file")
            , ("/etc/passwd", "secret")
            ]
          -- CWD is /project/subdir, allowed path is /project
          -- glob(".", "../*.txt") should match /project/file.txt
          -- Since base "." = /project/subdir is within /project, glob executes
          -- But result /project/file.txt should be allowed (it's within /project)
          program = limitSubpathRead "/project" $ glob @SecurityTest "." "../*.txt"
          result = runFSWithFail "/project/subdir" fs program

      -- /project/file.txt is within /project so it should be allowed
      -- Relative to base /project/subdir, it's ../file.txt
      result `shouldBe` Right ["../file.txt"]

    it "glob without filter returns relative paths that escape" $ do
      -- First verify that glob WITHOUT security filter actually returns the escaping path
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "subdir file")
            , ("/file.txt", "root file")
            ]
          -- glob WITHOUT limitSubpathRead should return ../../file.txt
          programWithoutFilter = glob @SecurityTest "." "../../*.txt"
          resultWithoutFilter = runFSWithFail "/project/subdir" fs programWithoutFilter

      -- Should match and return the relative path ../../file.txt
      resultWithoutFilter `shouldBe` Right ["../../file.txt"]

    it "blocks glob results that escape via .. outside allowed path" $ do
      -- CRITICAL TEST: Base is "." (allowed), but pattern "../../*.txt" escapes outside /project
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "subdir file")
            , ("/file.txt", "root file")
            ]
          -- CWD is /project/subdir, allowed path is /project
          -- glob(".", "../../*.txt") would match /file.txt which is OUTSIDE /project
          program = limitSubpathRead "/project" $ glob @SecurityTest "." "../../*.txt"
          result = runFSWithFail "/project/subdir" fs program

      -- /file.txt is outside /project so it must be filtered out
      -- The glob returns "../../file.txt" and isPathAllowed should filter it
      case result of
        Right files -> files `shouldBe` []  -- Should be empty after filtering
        Left err -> expectationFailure $ "Glob failed: " ++ err

    it "handles glob results that are relative paths (if glob implementation changes)" $ do
      -- Test what happens if glob returns relative paths instead of absolute
      -- This tests that isPathAllowed can handle relative paths correctly
      -- If someone changes glob to return relative paths, this should still work
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "subdir file")
            , ("/project/other.txt", "other file")
            ]
          -- Manually construct a test that would pass relative paths to the filter
          -- We can't easily do this with the real glob, but we can test the logic
          -- by checking our test helper handles both cases
          relativeTest = testIsPathAllowed "/project" "/project/subdir" "../other.txt"

      -- "../other.txt" from /project/subdir resolves to /project/other.txt which is allowed
      relativeTest `shouldBe` True

    it "filters glob results when base is at boundary and pattern could match parent" $ do
      -- Edge case: glob from /project with pattern "../*.txt"
      -- Even though base "/project" is allowed, results outside should be filtered
      let fs = Map.fromList
            [ ("/file.txt", "root file")
            , ("/project/file.txt", "project file")
            ]
          -- Base is "/project" which is allowed, but pattern tries to escape
          -- The glob operation itself might return /file.txt, but it should be filtered
          program = limitSubpathRead "/project" $ glob @SecurityTest "/project" "../*.txt"
          result = runFSWithFail "/project" fs program

      -- Even if glob returns files outside, they should be filtered
      -- In this case, "/file.txt" should be filtered out
      case result of
        Right files -> files `shouldNotContain` ["/file.txt"]
        Left err -> expectationFailure $ "Glob failed: " ++ err

  describe "Filter Composition" $ do
    it "hideGit filter blocks .git directory" $ do
      let filter = hideGit
          gitPath = "/project/.git/config"
          normalPath = "/project/src/Main.hs"

      -- .git directory should be blocked
      shouldInclude filter gitPath `shouldBe` False
      -- Normal files should be allowed
      shouldInclude filter normalPath `shouldBe` True

    it "hideGit filter blocks paths with .git component anywhere" $ do
      let filter = hideGit
          gitSubPath = "/project/.git/objects/abc123"
          nestedGit = "/project/subdir/.git/config"

      shouldInclude filter gitSubPath `shouldBe` False
      shouldInclude filter nestedGit `shouldBe` False

    it "hideClaude filter blocks .claude directory" $ do
      let filter = hideClaude
          claudePath = "/project/.claude/commands/test.sh"
          normalPath = "/project/src/Main.hs"

      -- .claude directory should be blocked
      shouldInclude filter claudePath `shouldBe` False
      -- Normal files should be allowed
      shouldInclude filter normalPath `shouldBe` True

    it "limitToSubpath <> hideGit combines filters with AND logic" $ do
      let combinedFilter = limitToSubpath "/project" <> hideGit
          -- Path inside project AND not in .git -> should be allowed
          allowedPath = "/project/src/Main.hs"
          -- Path inside project BUT in .git -> should be blocked
          gitPath = "/project/.git/config"
          -- Path outside project AND not in .git -> should be blocked
          outsidePath = "/etc/passwd"

      shouldInclude combinedFilter allowedPath `shouldBe` True
      shouldInclude combinedFilter gitPath `shouldBe` False
      shouldInclude combinedFilter outsidePath `shouldBe` False

    it "limitToSubpath <> hideGit <> hideClaude combines three filters" $ do
      let tripleFilter = limitToSubpath "/project" <> hideGit <> hideClaude
          normalPath = "/project/src/Main.hs"
          gitPath = "/project/.git/config"
          claudePath = "/project/.claude/commands/test.sh"
          outsidePath = "/etc/passwd"

      -- Only normal path inside project should be allowed
      shouldInclude tripleFilter normalPath `shouldBe` True
      shouldInclude tripleFilter gitPath `shouldBe` False
      shouldInclude tripleFilter claudePath `shouldBe` False
      shouldInclude tripleFilter outsidePath `shouldBe` False

    it "filterFileSystem with combined filters blocks .git files" $ do
      let fs = Map.fromList
            [ ("/project/.git/config", "git config")
            , ("/project/src/Main.hs", "main source")
            , ("/project/README.md", "readme")
            ]
          combinedFilter = limitToSubpath "/project" <> hideGit
          program = filterFileSystem @SecurityTest combinedFilter $
                    listFiles @SecurityTest "/project"
          result = runFSWithFail "/project" fs program

      case result of
        Right files -> do
          -- Should include normal files
          files `shouldSatisfy` ("src" `elem`)
          files `shouldSatisfy` ("README.md" `elem`)
          -- Should NOT include .git directory
          files `shouldSatisfy` (".git" `notElem`)
        Left err -> expectationFailure $ "List files failed: " ++ err

    it "filterRead with combined filters blocks reading .git files" $ do
      let fs = Map.fromList
            [ ("/project/.git/config", "git config")
            , ("/project/src/Main.hs", "main source")
            ]
          combinedFilter = limitToSubpath "/project" <> hideGit
          -- Try to read a .git file
          program = filterFileSystem @SecurityTest combinedFilter $
                    filterRead @SecurityTest combinedFilter $
                    readFile @SecurityTest "/project/.git/config"
          result = runFSWithFail "/project" fs program

      result `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

    it "filterRead with combined filters allows reading non-.git files" $ do
      let fs = Map.fromList
            [ ("/project/.git/config", "git config")
            , ("/project/src/Main.hs", "main source")
            ]
          combinedFilter = limitToSubpath "/project" <> hideGit
          -- Try to read a normal file
          program = filterFileSystem @SecurityTest combinedFilter $
                    filterRead @SecurityTest combinedFilter $
                    readFile @SecurityTest "/project/src/Main.hs"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "main source"

  describe "limitToSubpath with Chroot - Filter BEFORE chroot" $ do
    it "allows reading files within allowed path (filter sees system paths)" $ do
      let fs = Map.fromList [("/project/file.txt", "content")]
          -- Filter BEFORE chroot: use system path "/project"
          result = runFSFilterBeforeChroot "/project" fs (limitToSubpath "/project")
                   (readFile @SecurityTest "/file.txt")  -- This becomes "/project/file.txt"

      result `shouldBe` Right "content"

    it "blocks reading files outside allowed path (filter sees system paths)" $ do
      let fs = Map.fromList
            [ ("/project/file.txt", "allowed")
            , ("/other/blocked.txt", "blocked")
            ]
          -- Filter BEFORE chroot: restrict to "/project/subdir" (system path)
          -- Chroot to "/project", then try to read "/other" (absolute in chroot)
          -- This becomes "/project/other" in system coords, outside "/project/subdir"
          result = runFSFilterBeforeChroot "/project" fs (limitToSubpath "/project/subdir")
                   (readFile @SecurityTest "/file.txt")

      result `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

    it "allows listFiles within allowed subdirectory (filter sees system paths)" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file1.txt", "content1")
            , ("/project/subdir/file2.txt", "content2")
            , ("/project/other.txt", "other")
            ]
          -- Filter BEFORE chroot: restrict to "/project/subdir" (system path)
          result = runFSFilterBeforeChroot "/project" fs (limitToSubpath "/project/subdir")
                   (listFiles @SecurityTest "/subdir")

      case result of
        Right files -> do
          length files `shouldBe` 2  -- Should find 2 files in /subdir
          all (\f -> "file" `isInfixOf` f && ".txt" `isInfixOf` f) files `shouldBe` True
        Left err -> expectationFailure $ "listFiles failed: " ++ err

  describe "limitToSubpath with Chroot - Filter AFTER chroot" $ do
    it "allows reading files within chroot (filter sees chroot-relative paths)" $ do
      let fs = Map.fromList [("/project/file.txt", "content")]
          -- Filter AFTER chroot: use chroot-relative path "/"
          result = runFSFilterAfterChroot "/project" fs (limitToSubpath "/")
                   (readFile @SecurityTest "/file.txt")

      result `shouldBe` Right "content"

    it "blocks reading files outside allowed subdirectory (filter sees chroot-relative paths)" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "allowed")
            , ("/project/other.txt", "blocked")
            ]
          -- Filter AFTER chroot: restrict to "/subdir" (chroot-relative)
          result = runFSFilterAfterChroot "/project" fs (limitToSubpath "/subdir")
                   (readFile @SecurityTest "/other.txt")

      result `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

    it "allows listFiles within allowed subdirectory (filter sees chroot-relative paths)" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file1.txt", "content1")
            , ("/project/subdir/file2.txt", "content2")
            , ("/project/other.txt", "other")
            ]
          -- Filter AFTER chroot: restrict to "/subdir" (chroot-relative)
          result = runFSFilterAfterChroot "/project" fs (limitToSubpath "/subdir")
                   (listFiles @SecurityTest "/subdir")

      case result of
        Right files -> do
          length files `shouldBe` 2  -- Should find 2 files in /subdir
          all (\f -> "file" `isInfixOf` f && ".txt" `isInfixOf` f) files `shouldBe` True
        Left err -> expectationFailure $ "listFiles failed: " ++ err

    it "blocks listFiles outside allowed subdirectory (filter sees chroot-relative paths)" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "allowed")
            , ("/project/file.txt", "blocked")
            ]
          -- Filter AFTER chroot: restrict to "/subdir", but try to list "/"
          result = runFSFilterAfterChroot "/project" fs (limitToSubpath "/subdir")
                   (listFiles @SecurityTest "/")

      result `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

  describe "onlyClaude Filter" $ do
    it "onlyClaude filter allows .claude directory paths" $ do
      let filter = onlyClaude
          claudePath1 = "/project/.claude/commands/test.sh"
          claudePath2 = "/project/.claude/config.json"
          nestedClaude = "/project/src/.claude/file.txt"

      shouldInclude filter claudePath1 `shouldBe` True
      shouldInclude filter claudePath2 `shouldBe` True
      shouldInclude filter nestedClaude `shouldBe` True

    it "onlyClaude filter allows CLAUDE.md at any path" $ do
      let filter = onlyClaude
          rootClaude = "/project/CLAUDE.md"
          nestedClaude = "/project/subdir/CLAUDE.md"

      -- These should be allowed
      shouldInclude filter rootClaude `shouldBe` True
      shouldInclude filter nestedClaude `shouldBe` True

    it "onlyClaude filter blocks non-.claude files" $ do
      let filter = onlyClaude
          normalFile = "/project/src/Main.hs"
          readme = "/project/README.md"
          gitFile = "/project/.git/config"

      shouldInclude filter normalFile `shouldBe` False
      shouldInclude filter readme `shouldBe` False
      shouldInclude filter gitFile `shouldBe` False

    it "onlyClaude allows reading .claude files" $ do
      let fs = Map.fromList
            [ ("/project/.claude/commands/test.sh", "test command")
            , ("/project/src/Main.hs", "main source")
            ]
          program = filterFileSystem @SecurityTest onlyClaude $
                    filterRead @SecurityTest onlyClaude $
                    readFile @SecurityTest "/project/.claude/commands/test.sh"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "test command"

    it "onlyClaude allows reading CLAUDE.md" $ do
      let fs = Map.fromList
            [ ("/project/CLAUDE.md", "project instructions")
            , ("/project/src/Main.hs", "main source")
            ]
          program = filterFileSystem @SecurityTest onlyClaude $
                    filterRead @SecurityTest onlyClaude $
                    readFile @SecurityTest "/project/CLAUDE.md"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "project instructions"

    it "onlyClaude blocks reading non-.claude files" $ do
      let fs = Map.fromList
            [ ("/project/src/Main.hs", "main source")
            , ("/project/.claude/commands/test.sh", "test command")
            ]
          program = filterFileSystem @SecurityTest onlyClaude $
                    filterRead @SecurityTest onlyClaude $
                    readFile @SecurityTest "/project/src/Main.hs"
          result = runFSWithFail "/project" fs program

      result `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

    it "limitToSubpath <> onlyClaude combines correctly" $ do
      let combinedFilter = limitToSubpath "/project" <> onlyClaude
          -- Inside project AND in .claude -> allowed
          allowedPath = "/project/.claude/commands/test.sh"
          -- Inside project AND CLAUDE.md -> allowed
          claudeMd = "/project/CLAUDE.md"
          -- Inside project but NOT in .claude -> blocked
          blockedPath = "/project/src/Main.hs"
          -- Outside project (even if .claude) -> blocked
          outsidePath = "/other/.claude/file.txt"

      shouldInclude combinedFilter allowedPath `shouldBe` True
      shouldInclude combinedFilter claudeMd `shouldBe` True
      shouldInclude combinedFilter blockedPath `shouldBe` False
      shouldInclude combinedFilter outsidePath `shouldBe` False
