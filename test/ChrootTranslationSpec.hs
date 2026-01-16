{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChrootTranslationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.FilePath (isAbsolute, (</>), makeRelative, splitPath, takeDirectory)
import Data.List (isPrefixOf)
import qualified Runix.FileSystem.Path as Path
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.Map.Strict as Map
import Prelude hiding (readFile)

import Runix.FileSystem hiding (computeChrootCwd, makeProperRelativePath, isPrefixOfPath)
import qualified Runix.FileSystem as FS
import qualified Runix.FileSystem.System as System
import Runix.FileSystem.InMemory (InMemoryFS, filesystemInMemory)

-- | Test the core chroot translation logic
-- This tests the PURE translation functions, independent of effects

--------------------------------------------------------------------------------
-- Test-only helper functions (wrap production code for testing)
--------------------------------------------------------------------------------

computeCorrectionTerm :: FilePath -> FilePath -> Maybe FilePath
computeCorrectionTerm chrootRoot parentCwd =
  let relative = makeRelative chrootRoot parentCwd
      isOutside = isAbsolute relative || ".." `isPrefixOf` relative
  in if isOutside
     then Just $ FS.makeProperRelativePath parentCwd chrootRoot
     else Nothing

translateFromChroot :: FilePath -> FilePath -> FilePath -> FilePath
translateFromChroot = FS.translateChrootToParent

translateToChroot :: FilePath -> FilePath -> FilePath -> FilePath
translateToChroot chrootRoot parentCwd parentPath =
  let isInputAbsolute = isAbsolute parentPath
      parentAbsolute = if isInputAbsolute
                       then parentPath
                       else Path.resolveRelative parentCwd parentPath
      chrootAbsolute = "/" </> makeRelative chrootRoot parentAbsolute
      chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
  in if isInputAbsolute
     then chrootAbsolute
     else makeRelative chrootCwd chrootAbsolute

sanitizePath :: FilePath -> FilePath
sanitizePath = Path.resolvePath

sanitizeRelativePath :: FilePath -> FilePath
sanitizeRelativePath = id

pathDepth :: FilePath -> Int
pathDepth path =
  let resolved = Path.resolvePath path
  in if resolved == "/"
     then 0
     else length $ filter (not . null) $ filter (/= "/") $ splitPath resolved

spec :: Spec
spec = do
  describe "Chroot Translation - Core Logic" $ do
    describe "Computing chroot CWD from parent CWD" $ do
      it "parent CWD inside chroot -> chroot CWD is relative path" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/subdir"
            chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
        chrootCwd `shouldBe` "/subdir"

      it "parent CWD at chroot root -> chroot CWD is /" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project"
            chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
        chrootCwd `shouldBe` "/"

      it "parent CWD outside chroot -> chroot CWD is /" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user"
            chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
        chrootCwd `shouldBe` "/"

      it "parent CWD outside chroot (different path) -> chroot CWD is /" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/tmp"
            chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
        chrootCwd `shouldBe` "/"

      it "parent CWD in nested subdir -> chroot CWD preserves nesting" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/deep/nested/dir"
            chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
        chrootCwd `shouldBe` "/deep/nested/dir"

    describe "makeProperRelativePath" $ do
      it "same parent and target returns ." $ do
        let result = FS.makeProperRelativePath "/project" "/project"
        result `shouldBe` "."

      it "parent CWD equals chroot root, target is inside" $ do
        let result = FS.makeProperRelativePath "/project" "/project/config.yaml"
        result `shouldBe` "config.yaml"

      it "parent CWD is subdir of chroot root, target at root" $ do
        let result = FS.makeProperRelativePath "/project/subdir" "/project/file.txt"
        result `shouldBe` "../file.txt"

    describe "translateChrootToParent" $ do
      it "relative input when parent CWD == chroot root" $ do
        let result = FS.translateChrootToParent "/project" "/project" "config.yaml"
        result `shouldBe` "config.yaml"

      it "absolute input when parent CWD == chroot root" $ do
        let result = FS.translateChrootToParent "/project" "/project" "/config.yaml"
        result `shouldBe` "/project/config.yaml"

      it "relative input when parent CWD inside chroot" $ do
        let result = FS.translateChrootToParent "/project" "/project/subdir" "file.txt"
        result `shouldBe` "file.txt"

    describe "Computing correction term" $ do
      it "parent CWD inside chroot -> no correction needed" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/subdir"
            correctionTerm = computeCorrectionTerm chrootRoot parentCwd
        correctionTerm `shouldBe` Nothing

      it "parent CWD outside chroot -> correction term is relative path from parent to chroot" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user"
            correctionTerm = computeCorrectionTerm chrootRoot parentCwd
        correctionTerm `shouldBe` Just "project"

      it "parent CWD far outside chroot -> correction term bridges the gap" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/"
            correctionTerm = computeCorrectionTerm chrootRoot parentCwd
        correctionTerm `shouldBe` Just "home/user/project"

      it "parent CWD completely different tree -> go up and back down" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/tmp"
            -- Go up from /tmp to /, then down to /home/user/project
            -- This is ../home/user/project (still relative!)
            correctionTerm = computeCorrectionTerm chrootRoot parentCwd
        correctionTerm `shouldBe` Just "../home/user/project"

    describe "Translate chroot path to parent (fromChroot)" $ do
      describe "When parent CWD is inside chroot" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/subdir"
            chrootCwd = "/subdir"

        it "relative input -> relative output" $ do
          let chrootPath = "file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          parentPath `shouldBe` "file.txt"
          isAbsolute parentPath `shouldBe` False

        it "relative input with .." $ do
          let chrootPath = "../file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          parentPath `shouldBe` "../file.txt"
          isAbsolute parentPath `shouldBe` False

        it "absolute chroot input -> absolute parent output" $ do
          let chrootPath = "/subdir/file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          parentPath `shouldBe` "/home/user/project/subdir/file.txt"
          isAbsolute parentPath `shouldBe` True

        it "absolute chroot input at root" $ do
          let chrootPath = "/file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          parentPath `shouldBe` "/home/user/project/file.txt"

      describe "When parent CWD is outside chroot" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user"
            chrootCwd = "/"

        it "relative chroot input -> relative parent output (with correction)" $ do
          let chrootPath = "file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          -- From /home/user, accessing chroot's /file.txt requires project/file.txt
          parentPath `shouldBe` "project/file.txt"
          isAbsolute parentPath `shouldBe` False

        it "relative chroot input in subdir" $ do
          let chrootPath = "subdir/file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          parentPath `shouldBe` "project/subdir/file.txt"
          isAbsolute parentPath `shouldBe` False

        it "absolute chroot input -> absolute parent output" $ do
          let chrootPath = "/subdir/file.txt"
              parentPath = translateFromChroot chrootRoot parentCwd chrootPath
          parentPath `shouldBe` "/home/user/project/subdir/file.txt"
          isAbsolute parentPath `shouldBe` True

    describe "Translate parent path to chroot (toChroot)" $ do
      describe "When parent CWD is inside chroot" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/subdir"

        it "relative parent input -> relative chroot output" $ do
          let parentPath = "file.txt"
              chrootPath = translateToChroot chrootRoot parentCwd parentPath
          chrootPath `shouldBe` "file.txt"
          isAbsolute chrootPath `shouldBe` False

        it "absolute parent input inside chroot -> absolute chroot output" $ do
          let parentPath = "/home/user/project/subdir/file.txt"
              chrootPath = translateToChroot chrootRoot parentCwd parentPath
          chrootPath `shouldBe` "/subdir/file.txt"
          isAbsolute chrootPath `shouldBe` True

        it "absolute parent input at chroot root" $ do
          let parentPath = "/home/user/project/file.txt"
              chrootPath = translateToChroot chrootRoot parentCwd parentPath
          chrootPath `shouldBe` "/file.txt"

      describe "When parent CWD is outside chroot" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user"

        it "relative parent input (that reaches chroot) -> relative chroot output" $ do
          let parentPath = "project/file.txt"
              chrootPath = translateToChroot chrootRoot parentCwd parentPath
          -- From parent's perspective it's project/file.txt
          -- In chroot this is /file.txt, but returned relative to chroot CWD (/)
          chrootPath `shouldBe` "file.txt"
          isAbsolute chrootPath `shouldBe` False

        it "absolute parent input inside chroot -> absolute chroot output" $ do
          let parentPath = "/home/user/project/file.txt"
              chrootPath = translateToChroot chrootRoot parentCwd parentPath
          chrootPath `shouldBe` "/file.txt"
          isAbsolute chrootPath `shouldBe` True

    describe "Round-trip property: fromChroot . toChroot = id" $ do
      it "round-trip with parent CWD inside chroot (relative)" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/subdir"
            originalPath = "file.txt"
            parentPath = translateFromChroot chrootRoot parentCwd originalPath
            backToChrootPath = translateToChroot chrootRoot parentCwd parentPath
        backToChrootPath `shouldBe` originalPath

      it "round-trip with parent CWD inside chroot (absolute)" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user/project/subdir"
            originalPath = "/other/file.txt"
            parentPath = translateFromChroot chrootRoot parentCwd originalPath
            backToChrootPath = translateToChroot chrootRoot parentCwd parentPath
        backToChrootPath `shouldBe` originalPath

      it "round-trip with parent CWD outside chroot (relative)" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user"
            originalPath = "file.txt"
            parentPath = translateFromChroot chrootRoot parentCwd originalPath
            backToChrootPath = translateToChroot chrootRoot parentCwd parentPath
        backToChrootPath `shouldBe` originalPath

      it "round-trip with parent CWD outside chroot (absolute)" $ do
        let chrootRoot = "/home/user/project"
            parentCwd = "/home/user"
            originalPath = "/file.txt"
            parentPath = translateFromChroot chrootRoot parentCwd originalPath
            backToChrootPath = translateToChroot chrootRoot parentCwd parentPath
        backToChrootPath `shouldBe` originalPath

    describe "Path sanitization (prevents chroot escape)" $ do
      it "sanitizes absolute path with .." $ do
        let path = "/foo/bar/../baz"
            sanitized = sanitizePath path
        sanitized `shouldBe` "/foo/baz"

      it "sanitizes .. that would escape root" $ do
        let path = "/.."
            sanitized = sanitizePath path
        sanitized `shouldBe` "/"

      it "sanitizes multiple .. that escape root" $ do
        let path = "/../../../.."
            sanitized = sanitizePath path
        sanitized `shouldBe` "/"

      it "keeps .. that don't escape (for symlinks)" $ do
        let path = "/foo/../bar/../baz"
            sanitized = sanitizePath path
        sanitized `shouldBe` "/baz"

      it "removes . components" $ do
        let path = "/foo/./bar/./baz"
            sanitized = sanitizePath path
        sanitized `shouldBe` "/foo/bar/baz"

      it "collapses multiple slashes" $ do
        let path = "/foo//bar///baz"
            sanitized = sanitizePath path
        sanitized `shouldBe` "/foo/bar/baz"

      it "handles relative path with escaping .." $ do
        let path = "../../foo"
            sanitized = sanitizeRelativePath path
        -- Can't sanitize relative paths without context, but we track depth
        sanitized `shouldBe` "../../foo"

    describe "Path sanitization with depth tracking" $ do
      it "tracks depth correctly for /" $ do
        let depth = pathDepth "/"
        depth `shouldBe` 0

      it "tracks depth correctly for /foo" $ do
        let depth = pathDepth "/foo"
        depth `shouldBe` 1

      it "tracks depth correctly for /foo/bar/baz" $ do
        let depth = pathDepth "/foo/bar/baz"
        depth `shouldBe` 3

      it "tracks depth with .. going back" $ do
        let depth = pathDepth "/foo/bar/.."
        depth `shouldBe` 1

      it "clamps depth at 0 for escapes" $ do
        let depth = pathDepth "/../.."
        depth `shouldBe` 0

  describe "Integration with existing Path module" $ do
    it "sanitizePath should match Path.resolvePath" $ do
      let path = "/foo/bar/../baz"
      sanitizePath path `shouldBe` Path.resolvePath path

    it "handles root escaping like Path.resolvePath" $ do
      let path = "/.."
      sanitizePath path `shouldBe` Path.resolvePath path

    it "handles relative resolution" $ do
      let cwd = "/foo/bar"
          path = "../baz"
      Path.resolveRelative cwd path `shouldBe` "/foo/baz"

  describe "Integration Tests - Full Effect Stack" $ do
    describe "chrootFileSystem with InMemory backend" $ do
      it "debug: translateChrootToParent with our exact scenario" $ do
        -- Parent CWD = /project, Chroot Root = /project, Chroot Path = "config.yaml"
        -- Step by step:
        -- 1. chrootCwd = computeChrootCwd("/project", "/project") = "/"
        -- 2. isInputAbsolute = False
        -- 3. chrootAbsolute = "/" </> "config.yaml" = "/config.yaml"
        -- 4. sanitized = resolvePath("/config.yaml") = "/config.yaml"
        -- 5. parentAbsolute = "/project" </> "config.yaml" = "/project/config.yaml"
        -- 6. Since relative: makeProperRelativePath("/project", "/project/config.yaml")
        let chrootRoot = "/project"
            parentCwd = "/project"
            chrootPath = "config.yaml"
            result = FS.translateChrootToParent chrootRoot parentCwd chrootPath
        result `shouldBe` "config.yaml"

      it "debug: step by step breakdown" $ do
        let chrootRoot = "/project"
            parentCwd = "/project"
        -- Step 1: Compute chroot CWD
        let chrootCwd = FS.computeChrootCwd chrootRoot parentCwd
        chrootCwd `shouldBe` "/"
        -- Step 2: Make path absolute in chroot
        let chrootAbsolute = chrootCwd </> "config.yaml"
        chrootAbsolute `shouldBe` "/config.yaml"
        -- Step 3: Sanitize (resolve .. and .)
        let sanitized = Path.resolvePath chrootAbsolute
        sanitized `shouldBe` "/config.yaml"
        -- Step 4: Convert to parent absolute
        let parentAbsolute = chrootRoot </> dropWhile (== '/') sanitized
        parentAbsolute `shouldBe` "/project/config.yaml"
        -- Step 5: Make relative from parent CWD
        let result = FS.makeProperRelativePath parentCwd parentAbsolute
        result `shouldBe` "config.yaml"

      it "sanity check: InMemory filesystem alone works" $ do
        let fs = Map.fromList [("/project/config.yaml", "content")]
            proj = TestProject "/project"
            result = run
                   . runError @String
                   . failToError id
                   . filesystemInMemory "/project" fs proj
                   $ readFile @TestProject "config.yaml"
        result `shouldBe` Right "content"

      it "sanity check: what CWD does filesystemInMemory return" $ do
        let fs = Map.empty
            proj = TestProject "/project"
            result = run
                   . runError @String
                   . failToError id
                   . filesystemInMemory "/project" fs proj
                   $ getCwd @TestProject
        result `shouldBe` Right "/project"

      it "sanity check: what CWD does chrootFileSystem on top return" $ do
        let fs = Map.empty
            proj = TestProject "/project"
            result = run
                   . runError @String
                   . failToError id
                   . filesystemInMemory "/project" fs proj
                   . chrootFileSystem @TestProject
                   $ getCwd @TestProject
        result `shouldBe` Right "/"

      it "debug: what does fileExists return through the stack" $ do
        let fs = Map.fromList [("/project/config.yaml", "content")]
            proj = TestProject "/project"
            result = run
                   . runError @String
                   . failToError id
                   . filesystemInMemory "/project" fs proj
                   . chrootFileSystem @TestProject
                   $ fileExists @TestProject "config.yaml"
        -- This should translate "config.yaml" correctly and find the file
        result `shouldBe` Right True

      it "reads relative path from chrooted filesystem" $ do
        let fs = Map.fromList [("/project/config.yaml", "content")]
            result = runChrootTest "/project" fs $ readFile @TestProject "config.yaml"
        result `shouldBe` Right "content"

      it "reads absolute chroot path" $ do
        let fs = Map.fromList [("/project/config.yaml", "content")]
            result = runChrootTest "/project" fs $ readFile @TestProject "/config.yaml"
        result `shouldBe` Right "content"

      it "reads from nested directory with relative path" $ do
        let fs = Map.fromList [("/project/subdir/file.txt", "nested")]
            result = runChrootTest "/project" fs $ readFile @TestProject "subdir/file.txt"
        result `shouldBe` Right "nested"

      it "getCwd returns chroot-relative path when parent CWD is inside chroot" $ do
        let fs = Map.empty
            result = runChrootTestWithCwd "/project/subdir" "/project" fs $ getCwd @TestProject
        result `shouldBe` Right "/subdir"

      it "getCwd returns / when parent CWD is outside chroot" $ do
        let fs = Map.empty
            result = runChrootTestWithCwd "/home/user" "/home/user/project" fs $ getCwd @TestProject
        result `shouldBe` Right "/"

      it "relative path with .. that stays within chroot works" $ do
        let fs = Map.fromList
              [ ("/project/subdir/file.txt", "subdir")
              , ("/project/file.txt", "root")
              ]
            -- Start with CWD at /project/subdir (which is /subdir in chroot)
            -- Reading "../file.txt" should get /project/file.txt
            result = runChrootTestWithCwd "/project/subdir" "/project" fs $
                     readFile @TestProject "../file.txt"
        result `shouldBe` Right "root"

      it "relative path correctly translates when parent CWD is inside chroot" $ do
        let fs = Map.fromList [("/project/subdir/file.txt", "content")]
            -- Parent CWD is /project/subdir
            -- Chroot root is /project
            -- Reading relative "file.txt" should resolve to /project/subdir/file.txt
            result = runChrootTestWithCwd "/project/subdir" "/project" fs $
                     readFile @TestProject "file.txt"
        result `shouldBe` Right "content"

      it "relative path correctly translates when parent CWD is outside chroot" $ do
        let fs = Map.fromList [("/home/user/project/file.txt", "content")]
            -- Parent CWD is /home/user (outside chroot)
            -- Chroot root is /home/user/project
            -- In chroot, CWD is / (forced)
            -- Reading relative "file.txt" should resolve to /home/user/project/file.txt
            result = runChrootTestWithCwd "/home/user" "/home/user/project" fs $
                     readFile @TestProject "file.txt"
        result `shouldBe` Right "content"

      it "glob returns chroot-relative paths" $ do
        let fs = Map.fromList
              [ ("/project/file1.txt", "1")
              , ("/project/file2.txt", "2")
              , ("/project/doc.md", "doc")
              ]
            result = runChrootTest "/project" fs $ glob @TestProject "." "*.txt"
        result `shouldBe` Right ["file1.txt", "file2.txt"]

      it "listFiles returns chroot-relative paths" $ do
        let fs = Map.fromList
              [ ("/project/file1.txt", "1")
              , ("/project/file2.txt", "2")
              ]
            result = runChrootTest "/project" fs $ listFiles @TestProject "/"
        result `shouldSatisfy` \case
          Right files -> "file1.txt" `elem` files && "file2.txt" `elem` files
          Left _ -> False

-- | Test project type
newtype TestProject = TestProject FilePath deriving (Show, Eq)

instance HasProjectPath TestProject where
  getProjectPath (TestProject p) = p

-- | Run a chrooted filesystem test with default CWD = project root
runChrootTest :: FilePath -> InMemoryFS
              -> Sem '[FileSystemWrite TestProject, FileSystemRead TestProject, FileSystem TestProject, Fail, Error String] a
              -> Either String a
runChrootTest projectRoot fs action =
  runChrootTestWithCwd projectRoot projectRoot fs action

-- | Run a chrooted filesystem test with custom CWD
runChrootTestWithCwd :: FilePath   -- ^ Parent CWD
                     -> FilePath   -- ^ Project root (chroot root)
                     -> InMemoryFS
                     -> Sem '[FileSystemWrite TestProject, FileSystemRead TestProject, FileSystem TestProject, Fail, Error String] a
                     -> Either String a
runChrootTestWithCwd parentCwd projectRoot fs action =
  let proj = TestProject projectRoot
  in run
    . runError @String
    . failToError id
    . filesystemInMemory parentCwd fs proj
    . chrootFileSystem @TestProject
    $ action
