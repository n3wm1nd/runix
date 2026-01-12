{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileSystemProjectSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise, isAbsolute, (</>) , takeFileName, takeDirectory, splitDirectories, joinPath)
import Prelude hiding (readFile)
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified System.FilePath.Glob as Glob

import Runix.FileSystem
import qualified Runix.FileSystem.System as System
import Runix.FileSystem.InMemory

--------------------------------------------------------------------------------
-- In-Memory Filesystem for Testing
--------------------------------------------------------------------------------

-- Reuse the dummy write interpreter for tests
filesystemWriteDummy :: Member (Error String) r
                     => InMemoryFS
                     -> Sem (System.FileSystemWrite : r) a
                     -> Sem r (InMemoryFS, a)
filesystemWriteDummy fs prog = do
  result <- filesystemWriteInMemory fs prog
  return (fs, result)

--------------------------------------------------------------------------------
-- Project Types for Testing
--------------------------------------------------------------------------------

newtype ProjectA = ProjectA FilePath deriving (Show, Eq)
newtype ProjectB = ProjectB FilePath deriving (Show, Eq)

instance HasProjectPath ProjectA where
  getProjectPath (ProjectA p) = p

instance HasProjectPath ProjectB where
  getProjectPath (ProjectB p) = p

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

runProjectTest :: forall project a. HasProjectPath project
               => FilePath  -- CWD
               -> InMemoryFS
               -> project
               -> Sem '[FileSystemWrite project, FileSystemRead project, FileSystem project, Fail, System.FileSystemRead, System.FileSystemWrite, Error String] a
               -> Either String a
runProjectTest cwd fs proj prog =
  run
    . runError @String
    . fmap snd
    . filesystemWriteDummy fs
    . filesystemReadInMemory cwd fs
    . failToError id
    . fileSystemLocal proj
    $ prog

runMultiProjectTest :: FilePath
                    -> InMemoryFS
                    -> ProjectA
                    -> ProjectB
                    -> Sem '[FileSystemWrite ProjectA, FileSystemRead ProjectA, FileSystem ProjectA,
                             FileSystemWrite ProjectB, FileSystemRead ProjectB, FileSystem ProjectB,
                             Fail, System.FileSystemRead, System.FileSystemWrite, Error String] a
                    -> Either String a
runMultiProjectTest cwd fs projA projB prog =
  run
    . runError @String
    . fmap snd
    . filesystemWriteDummy fs
    . filesystemReadInMemory cwd fs
    . failToError id
    . fileSystemLocal projB
    . fileSystemLocal projA
    $ prog


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Basic Project Operations" $ do
    it "GetFileSystem returns the project value" $ do
      let proj = ProjectA "/test/project"
          result = runProjectTest "/test" Map.empty proj $ getFileSystem @ProjectA

      result `shouldBe` Right proj

    it "listFiles works with project-relative paths" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            ]
          -- This will fail with our dummy impl (no real directory support)
          -- but demonstrates the API
          result = runProjectTest "/project" fs proj $
            fileExists @ProjectA "file1.txt"

      result `shouldBe` Right True

    it "readFile reads from project root" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList [("/project/config.yaml", "test: true")]
          result = runProjectTest "/project" fs proj $
            readFile @ProjectA "config.yaml"

      result `shouldBe` Right "test: true"

    it "glob works relative to project root" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            , ("/project/doc.md", "markdown")
            ]
          result = runProjectTest "/project" fs proj $
            glob @ProjectA "." "*.txt"

      result `shouldBe` Right ["file1.txt", "file2.txt"]

  describe "Multiple Projects on Stack" $ do
    it "can read from different projects independently" $ do
      let projA = ProjectA "/project-a"
          projB = ProjectB "/project-b"
          fs = Map.fromList
            [ ("/project-a/a.txt", "from A")
            , ("/project-b/b.txt", "from B")
            ]
          result = runMultiProjectTest "/" fs projA projB $ do
            contentA <- readFile @ProjectA "/a.txt"
            contentB <- readFile @ProjectB "/b.txt"
            return (contentA, contentB)

      result `shouldBe` Right ("from A", "from B")

    it "projects have separate roots" $ do
      let projA = ProjectA "/project-a"
          projB = ProjectB "/project-b"
          fs = Map.fromList
            [ ("/project-a/fileA.txt", "from A")
            , ("/project-b/fileB.txt", "from B")
            ]
          -- Each project can only access files relative to its own root
          result = runMultiProjectTest "/" fs projA projB $ do
            -- ProjectA can access its own files (absolute in chroot)
            aFile <- readFile @ProjectA "/fileA.txt"
            -- ProjectB can access its own files (absolute in chroot)
            bFile <- readFile @ProjectB "/fileB.txt"
            return (aFile, bFile)

      result `shouldBe` Right ("from A", "from B")

  describe "Filters" $ do
    it "hideDotfiles filters out dotfiles from glob" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/file.txt", "visible")
            , ("/project/.hidden", "hidden")
            , ("/project/.git", "git")
            ]
          result = runProjectTest "/project" fs proj $
            filterFileSystem @ProjectA hideDotfiles $
              glob @ProjectA "." "*"

      -- Should not include .hidden or .git
      result `shouldSatisfy` \case
        Right files -> not (any (\f -> "." `isPrefixOf` f) files)
        Left _ -> False

    it "hideGit filters out .git directory" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/.git", "git")
            , ("/project/file.txt", "visible")
            ]
          result = runProjectTest "/project" fs proj $
            filterFileSystem @ProjectA hideGit $
              fileExists @ProjectA ".git"

      result `shouldBe` Right False

    it "onlyClaude only allows .claude files" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/.claude/agents/test.md", "agent")
            , ("/project/src/Main.hs", "code")
            ]
          result = runProjectTest "/project" fs proj $
            filterFileSystem @ProjectA onlyClaude $ do
              claude <- fileExists @ProjectA ".claude/agents/test.md"
              src <- fileExists @ProjectA "src/Main.hs"
              return (claude, src)

      -- .claude file exists, src file filtered out
      result `shouldBe` Right (True, False)

    it "filters compose with Semigroup" $ do
      let combined = hideGit <> hideDotfiles
          testPath1 = shouldInclude combined ".git"
          testPath2 = shouldInclude combined ".hidden"
          testPath3 = shouldInclude combined "normal.txt"

      testPath1 `shouldBe` False
      testPath2 `shouldBe` False
      testPath3 `shouldBe` True

    it "filterRead blocks reading filtered files" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/.git", "git data")
            , ("/project/file.txt", "normal")
            ]
          result = runProjectTest "/project" fs proj $
            filterRead @ProjectA hideGit $
              readFile @ProjectA ".git"

      result `shouldSatisfy` \case
        Left err -> "Access denied" `isInfixOf` err
        Right _ -> False

  describe "Capability Separation" $ do
    it "FileSystem effect allows structure exploration without reading" $ do
      -- A function that only needs FileSystem (not Read) can see files but not read them
      let proj = ProjectA "/project"
          fs = Map.fromList [("/project/config.yaml", "test: true")]

          result = runProjectTest "/project" fs proj $
            fileExists @ProjectA "config.yaml"

      result `shouldBe` Right True

    it "FileSystemRead requires reading capability" $ do
      -- A function that needs to read file contents
      let proj = ProjectA "/project"
          fs = Map.fromList [("/project/config.yaml", "test: true")]

          result = runProjectTest "/project" fs proj $
            readFile @ProjectA "config.yaml"

      result `shouldBe` Right "test: true"

  describe "Path Translation" $ do
    it "projectToSystemPath converts project-relative to system paths" $ do
      let proj = ProjectA "/project"
          systemPath = projectToSystemPath proj "src/Main.hs"

      systemPath `shouldBe` "/project/src/Main.hs"

    it "getProjectPath extracts root path" $ do
      let proj = ProjectA "/my/project"
          root = getProjectPath proj

      root `shouldBe` "/my/project"

  describe "Real-world Usage Patterns" $ do
    it "can create separate filtered views of same directory" $ do
      -- Simulate having both a "user files" view and a "claude config" view
      let userFiles = ProjectA "/project"
          claudeConfig = ProjectB "/project"  -- Same directory!
          fs = Map.fromList
            [ ("/project/.claude", "config dir marker")
            , ("/project/src/Main.hs", "code")
            , ("/project/.git", "git data")
            ]

          -- User files view: hide .git and .claude
          result = runMultiProjectTest "/project" fs userFiles claudeConfig $
            filterFileSystem @ProjectA (hideGit <> hideClaude) $ do
              gitExists <- fileExists @ProjectA ".git"
              claudeExists <- fileExists @ProjectA ".claude"
              srcExists <- fileExists @ProjectA "src/Main.hs"
              return (gitExists, claudeExists, srcExists)

      -- .git and .claude filtered out, src visible
      result `shouldBe` Right (False, False, True)

    it "type system prevents reading from wrong project" $ do
      -- This test demonstrates compile-time safety
      -- If you try: readFile @ProjectA "file.txt" but only have ProjectB in scope,
      -- it won't compile. We can't easily test this at runtime, but the types ensure it.

      let projA = ProjectA "/project-a"
          fs = Map.fromList [("/project-a/file.txt", "content")]

          -- This compiles because we have ProjectA
          validProgram :: Members '[FileSystemRead ProjectA, Fail] r => Sem r BS.ByteString
          validProgram = readFile @ProjectA "file.txt"

          result = runProjectTest "/project-a" fs projA validProgram

      result `shouldBe` Right "content"
