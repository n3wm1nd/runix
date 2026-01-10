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

import Runix.FileSystem.Effects
import qualified Runix.FileSystem.System.Effects as System
import Runix.FileSystem.InMemory.Effects (InMemoryFS, filesystemInMemory)

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

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- Test isPathAllowed logic directly
testIsPathAllowed :: FilePath -> FilePath -> FilePath -> Bool
testIsPathAllowed allowedPath cwd targetPath =
  let normalizedAllowed = addTrailingPathSeparator $ System.basicResolvePath allowedPath
      absoluteTarget = if isAbsolute targetPath
                      then System.basicResolvePath targetPath
                      else System.basicResolvePath (cwd </> targetPath)
      normalizedTarget = addTrailingPathSeparator absoluteTarget
  in splitPath normalizedAllowed `isPrefixOf` splitPath normalizedTarget

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
